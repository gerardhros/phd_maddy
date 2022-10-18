#CONTROLLING FUNCTIONS AND EVEENTUALLY CAN JUST USE THIS FOR FINAL RUNNING

# #################################################################################################
# #   MAIN CODE TO RUN SIMULATIONS
# #################################################################################################

  # dst_prepare_input - combines Integrator/Eurostat to make db_final_europe
  # dst_loaddb = loads external databases and calculates management areas
  # dst_functions =

  setwd('C:/phd_maddy/')

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

  # load the default meta-analytical models AND covariate models
  # creates a list of objects --grand mean man (7) - grand SD-cov means (58) - cov SDs--
  ma_models <- lmam(fname = 'data/MA models template AGEE.xlsx')

  # join MA-impact models per management measure (main DST model)
  # outcome: ncu / area meas. / indicator / meas. code / mean change / SD
  dt.m1 <- cIMAm(management='CC',db = d1, mam = ma_models)
  dt.m2 <- cIMAm(management='RES',db = d1, mam = ma_models)
  dt.m3 <- cIMAm(management='ROT',db = d1, mam = ma_models)
  dt.m4 <- cIMAm(management='NT-CT',db = d1, mam = ma_models)
  dt.m5 <- cIMAm(management='RT-CT',db = d1, mam = ma_models)
  dt.m6 <- cIMAm(management='CF-MF',db = d1, mam = ma_models)
  dt.m7 <- cIMAm(management='OF-MF',db = d1, mam = ma_models)

  # combine all measures and their impacts into one data.table
  dt.m <- rbind(dt.m1,dt.m2,dt.m3,dt.m4,dt.m5,dt.m6,dt.m7)
  rm(dt.m1,dt.m2,dt.m3,dt.m4,dt.m5,dt.m6,dt.m7)

  # run a DST simulation
  sim1 <- runDST(db = d1, dt.m = dt.m, output = 'total_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=1)

  simX <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=1)
  saveRDS(simX,'D:/ESA/02 phd projects/01 maddy young/01 data/simx.rds')
  outputX <- simX$impact_best

  sim2 <- runDST(db = d1, dt.m = dt.m, output = 'score_best',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=1)
  output2 <- sim2$score_best

  simX3<- runDST(db = d1, dt.m = dt.m, output = 'total_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=3)


  # do you want to run Monte Carlo Simulations?
  sim2 <- runMC_DST(db = d1,mam = ma_models, nsim = 1, covar = TRUE,simyear = 5,
                    uw = c(1,1,1),
                    measures = c('CC','RES','ROT','NT-CT','RT-CT','CF-MF','OF-MF'),
                    output = 'all')
