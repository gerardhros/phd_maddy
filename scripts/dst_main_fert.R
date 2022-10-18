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

#================================================
# set data sources
#================================================

setwd('C:/phd_maddy/')

# load packages
require(readxl);require(data.table); require(dplyr)

# remove data
rm(list=ls())

# load functions for aggregation and optimisation DST
source('scripts/dst_functions_fert.r')

# location of data objects not stored on github
floc <- 'D:/ESA/02 phd projects/01 maddy young/01 data/'

#================================================
# connect databases, meta-models, site factors
#================================================

# read in the earlier saved database from integrator
# replace in dst_outputs with smaller BE dataset for testing
d1 <- fread(paste0(floc,'db_final_europe.csv'))

# load the global AND covariate meta-models when available
ma_models <- lmam(fname = 'C:/dst_outputs/mmc2_fert_meas_0-1_OF-Nsu.xlsx')

# join MA impact models for fertiliser measures
dt.m1 <- cIMAm(management='EE',db = d1, mam = ma_models)
dt.m2 <- cIMAm(management='RFP',db = d1, mam = ma_models)
dt.m3 <- cIMAm(management='RFR',db = d1, mam = ma_models)
dt.m4 <- cIMAm(management='RFT',db = d1, mam = ma_models)
dt.m5 <- cIMAm(management='CF-MF',db = d1, mam = ma_models, covar = TRUE)
dt.m6 <- cIMAm(management='OF-MF',db = d1, mam = ma_models, covar = TRUE)

# combine all measures and their impacts into one data.table
dt.m <- rbind(dt.m1,dt.m2,dt.m3,dt.m4,dt.m5,dt.m6)

# create data table to summarize over mean of continuous variables
# N critical is left out - MISSING VALUES AND VARUES PER CROP TYPE - NEEDS WEIGHTED MEAN PER NCU
d1.fact.cont <- data.table(cbind(d1$ncu, d1$texture, d1$density, d1$cn, d1$clay, d1$ph,
                             d1$yield_ref, d1$soc_ref, d1$n_sp_ref,
                             d1$yield_target, d1$soc_target)) #d1$n_sp_sw_crit, d1$n_sp_gw_crit
colnames(d1.fact.cont) <- c('ncu', 'texture', 'density', 'cn', 'clay', 'ph',
                         'yield_ref', 'soc_ref', 'n_sp_ref','yield_target', 'soc_target') #'n_sp_sw_crit', 'n_sp_gw_crit'
d1.fact.cont <- aggregate(.~ncu,d1.fact.cont,mean)

# save meta-models in table
ma.models <- data.frame(ma_models$ma_mean)
fwrite(ma.models,paste0(floc,'ma.models.csv'))
ma.cov.models <- data.frame(ma_models$ma_cov_mean)
fwrite(ma.cov.models,paste0(floc,'ma.cov.models.csv'))

#=====================================================================================
# DST simulation for best measure and ranking of measures, ONE measure applied at time
#=====================================================================================

sim.all <- runDST(db = d1, dt.m = dt.m, output = 'all',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=1)

# IMPACT_BEST -----------------------------------------------------------------

# save 1 best measure
out.best <- sim.all$impact_best

  #frequency of best measures - make table for single rankings
  table(out.best$man_code)

  #summary over input/output parameters
  out.best.param <- aggregate(.~man_code,data=out.best,mean)
  #merge out.best with continuous site factors
  fact.cont.best <- merge(d1.fact.cont,out.best,by='ncu')
  fact.best <- aggregate(.~man_code,data=fact.cont.best,mean)

  fwrite(fact.best,paste0(floc,'fact.best.csv'))


# SCORE_SINGLE ----------------------------------------------------------------

# ranking of each measure - map - check second ranked measure after EE
out.single <- sim.all$score_single
out.single$concat <- paste(out.single$`1`,out.single$`2`,out.single$`3`,
                           out.single$`4`,out.single$`5`,out.single$`6`, sep='-')

  #frequency of best measures - make table for single rankings
  rank.single <- data.frame(table(out.single$concat))
  fwrite(rank.single,paste0(floc,'rank.single.csv'))
  colnames(rank.single) <- c('concat', 'freq')

  #select columns to summarize continuous variables
  out.single.stat <- out.single[,.(ncu,concat,dist_Y,dist_C,dist_N)]
  #summary over input/output parameters
  out.single.param <- aggregate(.~concat,data=out.single.stat,mean)
  #merge with continuous site factors and save
  fact.cont.single <- merge(d1.fact.cont,out.single.stat,by='ncu')
  fact.cont.single <- merge(fact.cont.single,rank.single,by='concat')

  fact.single <- aggregate(.~concat,data=fact.cont.single,mean)

  fwrite(fact.single,paste0(floc,'fact.single.csv'))


###### summary statistics  NOT WORKING FOR CATEGORICAL
# ??? must choose just one line for categorical variables but not sure how to aggregate ???
# simply remove duplicates categorical variables or group by
# main crop type can be derived per NCU later by regrouping in main categories and assigning majority area
# or save each crop area as column and check relationship to proportion of the crop area

#categorical variables
# colnames(d1.fact) <- c('ncu', 'texture', 'cov_soil', 'cov_clim', 'cov_fert','cov_soc',
#                        'yield_ref', 'soc_ref', 'n_sp_ref','yield_target', 'soc_target', 'n_sp_sw_crit', 'n_sp_gw_crit')
######

#=====================================================================================
# DST simulation for TWO combined measures applied at once
#=====================================================================================
sim.all.2 <- runDST(db = d1, dt.m = dt.m, output = 'all',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=2)

  # best pair of TWO measures
  out.duo <- sim.all.2$score_duo
  #frequency of best measures - make table for single rankings
  rank.duo <- data.frame(table(out.duo$`1`))
  fwrite(rank.duo,paste0(floc,'rank.duo.csv'))
  colnames(rank.single) <- c('concat', 'freq')

  #select columns to summarize continuous variables
  out.duo.stat <- out.duo[,.(ncu,`1`,dist_Y,dist_C,dist_N)]
  #summary over input/output parameters
  #merge with continuous site factors and save
  fact.cont.duo <- merge(d1.fact.cont,out.duo.stat,by='ncu')

  fact.duo <- aggregate(.~`1`,data=fact.cont.duo,mean)
  fwrite(fact.duo,paste0(floc,'fact.duo.csv'))

#=====================================================================================
# DST simulation for THREE combined measures applied at once
#=====================================================================================
sim.all.3 <- runDST(db = d1, dt.m = dt.m, output = 'score_trio',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=3)
  # best pair of THREE measures
  out.trio <- sim.all.3$score_trio
  #frequency of best measures - make table for single rankings
  rank.trio <- data.frame(table(out.trio$`1`))
  fwrite(rank.trio,paste0(floc,'rank.trio.csv'))

  #select columns to summarize continuous variables
  out.trio.stat <- out.trio[,.(ncu,`1`,dist_Y,dist_C,dist_N)]
  #summary over input/output parameters
  #merge with continuous site factors and save
  fact.cont.trio <- merge(d1.fact.cont,out.trio.stat,by='ncu')
  fact.trio <- aggregate(.~`1`,data=fact.cont.trio,mean)
  fwrite(fact.trio,paste0(floc,'fact.trio.csv'))

# output_total lists all possible combinations per ncu and impacts
# not relevant to map but might be useful later to show different applications

#=====================================================================================
# Reference maps
#=====================================================================================

d1.sub <- d1[,.(ncu,yield_ref,soc_ref,n_sp_ref,yield_target,soc_target)]
ref.values <- aggregate(.~ncu,data=d1.sub,mean)

soc.ref <- aggregate(.~soc_ref,data=d1,mean)
nsu.ref <- aggregate(.~n_sp_ref,data=d1,mean)



#score_single
# ncu+dist_Y+dist_C+dist_N~bipmcs, value=man_code
sim7 <- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=1)
output7 <- sim7$score_single
single_rank <- data.frame(rbind(table(output7$'CF-MF'), table(output7$'OF-MF'),
                                table(output7$'EE'), table(output7$'RFR'),
                                table(output7$'RFT'), table(output7$'RFP')))
fwrite(single_rank,paste0(floc,'single_rank.csv'))
# R1 (best measure)
# CF-MF   EE   OF-MF   RFP    RFR    RFT
# 527    453   15535   11105  1424   432

# ncuN~man_code, value=bipmcs

sim7b <- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=1)
output7b <- sim7b$score_single
single_rank_b <- data.frame(rbind(table(output7b$'CF-MF'), table(output7b$'OF-MF'),
                                table(output7b$'EE'), table(output7b$'RFR'),
                                table(output7b$'RFT'), table(output7b$'RFP')))
fwrite(single_rank_b,paste0(floc,'single_rank_b.csv'))
#Rank 1
# CF-MF   EE   OF-MF   RFP    RFR    RFT
# 514    439   15690   11034  1393   406



# do you want to run Monte Carlo Simulations?
# Note - 'best_impact' is the string  input for impact_best
sim.MC <- runMC_DST(db = d1,mam = ma_models, nsim = 1, covar = FALSE,simyear = 5,
                  uw = c(1,1,1),
                  measures = c('CF-MF','OF-MF','EE','RFR','RFT','RFP'),
                  output = 'all', nmax=1)

# run a DST simulation, varying the max number of combinations
# without MC, the SD of the input values are not taken into account in generating variation in output

#replace missing models with 0.001 and 0.001
sim1 <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=1)
output1 <- sim1$impact_best
# CF-MF EE  OF-MF   RFP   RFR   RFT
# 532   427 15588  11110  1412   407

#replace missing models with 0.000001 and 100
sim5 <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=1)
output5 <- sim5$impact_best
# CF-MF    EE OF-MF   RFP   RFR   RFT
# 505   470 15619 11065  1417   400

#prioritize N - Not much effect
sim2 <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(1,1,10),simyear = 5,quiet = FALSE,nmax=1)
output2 <- sim2$impact_best
# CF-MF  EE  OF-MF   RFP    RFR    RFT
# 553   445  14798   12252  1045   383

#replace missing models with 0
sim3 <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=1)
output3 <- sim3$impact_best
# CF-MF   EE   OF-MF   RFP   RFR   RFT
# 4532   1164   160    193    51   23376

#replace missing models with NA
sim4 <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=1)
output4 <- sim4$impact_best
# CF-MF  EE    OF-MF   RFP   RFR   RFT
# 4400   1122   148    186   58   23562



#replace missing models with NA and change NaN dX output to 0
sim6 <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=1)
output6 <- sim6$impact_best
# CF-MF   EE   OF-MF   RFP   RFR   RFT
# 11170   476   157   162   2531   14980


#score_single
sim7 <- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=1)
output7 <- sim7$score_single
single_rank <- data.frame(rbind(table(output7$'CF-MF'), table(output7$'OF-MF'),
                             table(output7$'EE'), table(output7$'RFR'),
                             table(output7$'RFT'), table(output7$'RFP')))
fwrite(single_rank,paste0(floc,'single_rank.csv'))


 #PLOT SCORE SINGLE AND TRY TO RECLASSIFY?



#score_duo
sim8 <- runDST(db = d1, dt.m = dt.m, output = 'score_duo',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=2)
output8 <- sim8$score_duo

duo_rank <- data.frame(rbind(table(output8$'CF-MF-EE'), table(output8$'CF-MF-OF-MF'), table(output8$'CF-MF-RFP'),
                       table(output8$'CF-MF-RFR'), table(output8$'CF-MF-RFT'),
                       table(output8$'EE-OF-MF'), table(output8$'EE-RFP'), table(output8$'EE-RFR'), table(output8$'EE-RFT'),
                       table(output8$'OF-MF-RFP'),
                       table(output8$'OF-MF-RFR'), table(output8$'OF-MF-RFT'), table(output8$'RFP-RFR'), table(output8$'RFP-RFT'), table(output8$'RFR-RFT')))
colnames(duo_rank) <- c('R1', 'R2', 'R3', 'R4', 'R5', 'R6', 'R7', 'R8', 'R9', 'R10', 'R11', 'R12',
                        'R13', 'R14', 'R15')
man_combo <- c('CF-MF-EE', 'CF-MF-OF-MF', 'CF-MF-RFP', 'CF-MF-RFR', 'CF-MF-RFT', 'EE-OF-MF',
        'EE-RFP', 'EE-RFR', 'EE-RFT', 'OF-MF-RFP', 'OF-MF-RFR', 'OF-MF-RFT','RFP-RFR',
        'RFP-RFT','RFR-RFT')
duo_rank <- cbind(man_combo,duo_rank)
fwrite(duo_rank,paste0(floc,'duo_rank.csv'))


#score duo test to change output table
sim9 <- runDST(db = d1, dt.m = dt.m, output = 'score_duo',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=2)
output9 <- sim9$score_duo
output9mini <- data.table(ncu=output9$ncu, R1=output9$`1`, R2=output9$`2`, R3=output9$`3`)


#
# # total_impact gives all outcomes/combinations possible per ncu and the outcome on each indicator
# # number of rows changes by number of practices = longer when more combinations are possible
# sim1a <- runDST(db = d1, dt.m = dt.m, output = 'total_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=1)
# output.1a <- sim1a$impact_total
# sim1b <- runDST(db = d1, dt.m = dt.m, output = 'total_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=5)
# output.1b <- sim1b$impact_total
# sim1c <- runDST(db = d1, dt.m = dt.m, output = 'total_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=2)
# output.1c <- sim1c$impact_total
#
# # best_impact gives one single best measure, 1 row per NCU
# # 3 impacts listed (empty for SOC and Nsu)
# # at first the best was only RFP when missing models were NA, regardless of varying the parameters
# sim2a <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=1)
# output.2a <- sim2a$impact_best
#
#
#
#
# sim2b <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=5)
# output.2b <- sim2b$impact_best
# sim2c <- runDST(db = d1, dt.m = dt.m, output = 'best_impact',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=2)
# output.2c <- sim2c$impact_best
#
#
# # score_single gives ranking of all measures, each one as an individual column
# # no impacts listed
# # nmax only changes the result slightly most likely due to the small random error
# sim3a<- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=1)
# output.3a <- sim3a$score_single
# sim3b<- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(1,1,1),simyear = 5,quiet = FALSE,nmax=3)
# output.3b <- sim3b$score_single
#
# #testing Uw parameters
# sim3c<- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(1,10,1),simyear = 5,quiet = FALSE,nmax=1)
# output.3c <- sim3c$score_single
#
# sim3d<- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(10,1,1),simyear = 5,quiet = FALSE,nmax=1)
# output.3d <- sim3d$score_single
#
# sim3e<- runDST(db = d1, dt.m = dt.m, output = 'score_single',uw = c(1,1,10),simyear = 5,quiet = FALSE,nmax=1)
# output.3e <- sim3e$score_single
#
#
# # score_duo gives ranking of each pair combo of measures (2 max) as a column
# sim4a<- runDST(db = d1, dt.m = dt.m, output = 'score_duo',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=2)
# output.4a <- sim4a$score_duo
# sim4b<- runDST(db = d1, dt.m = dt.m, output = 'score_duo',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=4)
# output.4b <- sim4b$score_duo
# # changing nmax still only returns 2 measures combined, again with different results due to the random error
#
# # score_best seems the same as "best_impact" but only 2 col's (no impacts per indicator copied)
# sim5a<- runDST(db = d1, dt.m = dt.m, output = 'score_best',uw = c(2,1,1),simyear = 5,quiet = FALSE,nmax=2)
# output.5a <- sim5a$score_best
# sim5b<- runDST(db = d1, dt.m = dt.m, output = 'score_best',uw = c(0,10,1),simyear = 5,quiet = FALSE,nmax=1)
# output.5b <- sim5a$score_best




