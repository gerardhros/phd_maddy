# #################################################################################################
# #   CONNECT TO META-ANALYTICAL DATA
# #################################################################################################

  # load packages
  require(readxl);require(data.table)

  # remove data
  rm(list=ls())

  # load functions for aggregation and optimisation DST
  source('scripts/dst_functions.r')

  # location of data objects not stored on github
  floc <- 'D:/ESA/02 phd projects/01 maddy young/01 data/'

  # read in the earlier saved database from integrator
  d1 <- fread(paste0(floc,'db_final_europe.csv'))

  # load the default meta-analytical models
  ma_models <- lmam(fname = 'data/MA models template AGEE.xlsx')

  # join MA-impact models per management measure
  dt.m1 <- cIMAm(management='CC',db = d1, mam = ma_models)
  dt.m2 <- cIMAm(management='RES',db = d1, mam = ma_models)
  dt.m3 <- cIMAm(management='ROT',db = d1, mam = ma_models)
  dt.m4 <- cIMAm(management='NT-CT',db = d1, mam = ma_models)
  dt.m5 <- cIMAm(management='RT-CT',db = d1, mam = ma_models)
  dt.m6 <- cIMAm(management='CF-MF',db = d1, mam = ma_models)
  dt.m7 <- cIMAm(management='OF-MF',db = d1, mam = ma_models)

  # combine all measures and their impacts into one data.table
  dt.m <- rbind(dt.m1,dt.m2,dt.m3,dt.m4,dt.m5,dt.m6,dt.m7)

  # run a DST simulation
  sim1 <- runDST(db = d1, dt.m = dt.m, output = 'total_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE)


  # do you want to run Monte Carlo Simulations?
  sim2 <- runMC_DST(db = d1,mam = ma_models, nsim = 1, covar = TRUE,simyear = 5,
                    uw = c(1,1,1),
                    measures = c('CC','RES','ROT','NT-CT','RT-CT','CF-MF','OF-MF'),
                    output = 'all')
