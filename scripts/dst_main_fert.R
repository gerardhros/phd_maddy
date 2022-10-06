#CONTROLLING FUNCTIONS AND EVEENTUALLY CAN JUST USE THIS FOR FINAL RUNNING

# #################################################################################################
# #   MAIN CODE TO RUN SIMULATIONS
# #################################################################################################


# dst_loaddb = loads/combines Integrator/Eurostat external databases and likelihoods
#            = only use if db_final_europe needs to be generated

# dst_prepare_input = applies downscaling to make db_final_europe
#                   = only use if db_final_europe needs to be generated
#                   = update the dst_outputs folder with correct version (subset BE or all EU)

# dst_functions = used by all scripts

setwd('C:/phd_maddy/')

# load packages
require(readxl);require(data.table)

# remove data
rm(list=ls())

# load functions for aggregation and optimisation DST
source('scripts/dst_functions_fert.r')

# location of data objects not stored on github
floc <- 'C:/dst_outputs/'

# read in the earlier saved database from integrator
# replace in dst_outputs with smaller BE dataset for testing
d1 <- fread(paste0(floc,'db_final_europe.csv'))

# load the default meta-analytical models AND covariate models
# creates a list of objects --grand mean man (7) - grand SD-cov means (58) - cov SDs--
ma_models <- lmam(fname = 'data/mmc2_fert_meas.xlsx')

# join MA impact models for fertiliser measures (specific to IFS conference paper)
# outcome: ncu / area meas. / indicator / meas. code / mean change / SD

dt.m1 <- cIMAm(management='EE',db = d1, mam = ma_models)
dt.m2 <- cIMAm(management='RFP',db = d1, mam = ma_models)
dt.m3 <- cIMAm(management='RFR',db = d1, mam = ma_models)
dt.m4 <- cIMAm(management='RFT',db = d1, mam = ma_models)
dt.m5 <- cIMAm(management='CF-MF',db = d1, mam = ma_models)
dt.m6 <- cIMAm(management='OF-MF',db = d1, mam = ma_models)

# combine all measures and their impacts into one data.table
dt.m <- rbind(dt.m1,dt.m2,dt.m3,dt.m4,dt.m5,dt.m6)



# run a DST simulation, varying the max number of combinations

# total_impact gives all outcomes/combinations possible per ncu and the outcome on each indicator
# number of rows changes by number of practices = longer when more combinations are possible
sim1a <- runDST(db = d1, dt.m = dt.m, output = 'total_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=1)
output.1a <- sim1a$impact_total
sim1b <- runDST(db = d1, dt.m = dt.m, output = 'total_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=5)
output.1b <- sim1b$impact_total
sim1c <- runDST(db = d1, dt.m = dt.m, output = 'total_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=2)
output.1c <- sim1c$impact_total

# best_impact gives one single best measure, 1 row per NCU
# 3 impacts listed (empty for SOC and Nsu)
# at first the best was only RFP when missing models were NA, regardless of varying the parameters
sim2a <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=1)
output.2a <- sim2a$impact_best

sim2a.dist <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=1)
output.2a.dist <- sim2a.dist$impact_best

sim2b <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=5)
output.2b <- sim2b$impact_best
sim2c <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=2)
output.2c <- sim2c$impact_best


# score_single gives ranking of all measures, each one as an individual column
# no impacts listed
# nmax only changes the result slightly most likely due to the small random error
sim3a<- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=1)
output.3a <- sim3a$score_single
sim3b<- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=3)
output.3b <- sim3b$score_single

#testing Uw parameters
sim3c<- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(1,10,1),simyear = 5,quiet = FALSE,nmax=1)
output.3c <- sim3c$score_single

sim3d<- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(10,1,1),simyear = 5,quiet = FALSE,nmax=1)
output.3d <- sim3d$score_single

sim3e<- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(1,1,10),simyear = 5,quiet = FALSE,nmax=1)
output.3e <- sim3e$score_single


# score_duo gives ranking of each pair combo of measures (2 max) as a column
sim4a<- runDST(db = d1, dt.m = dt.m, output = 'score_duo',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=2)
output.4a <- sim4a$score_duo
sim4b<- runDST(db = d1, dt.m = dt.m, output = 'score_duo',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=4)
output.4b <- sim4b$score_duo
# changing nmax still only returns 2 measures combined, again with different results due to the random error

# score_best seems the same as "best_impact" but only 2 col's (no impacts per indicator copied)
sim5a<- runDST(db = d1, dt.m = dt.m, output = 'score_best',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=2)
output.5a <- sim5a$score_best
sim5b<- runDST(db = d1, dt.m = dt.m, output = 'score_best',uw = c(0,10,1),simyear = 5,quiet = FALSE,nmax=1)
output.5b <- sim5a$score_best


# do you want to run Monte Carlo Simulations?
sim.MC <- runMC_DST(db = d1,mam = ma_models, nsim = 1, covar = FALSE,simyear = 5,
                  uw = c(1,1,1),
                  measures = c('CF-MF','OF-MF','EE','RFR','RFT','RFP'),
                  output = 'all', nmax=3)
output.MC <-
