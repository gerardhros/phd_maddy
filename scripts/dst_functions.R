# Functions to run DST of Maddy

#' @param db (data.table) a data.table with INTEGRATOR data used for the DST
#' @param dt.m (data.table) a data.table containing the meta-analytical estimates for Y, Nsu and SOC per management
#' @param uw (vector) (user weight) is an argument that gives the relative importance for yield, soc or n-surplus (a value above 0, likely a fraction between 0 and 2). Default is c(1,1,1).
#' @param simyear (num) value for the default year for simulation of impacts (default is 5 years)
#' @param output (string) optional argument to select different types of outputs. Options include: 'total impact', 'score_long','score_wide','score_long_combi', or 'score_wide_combi', or 'all'.
#' @param quiet (boolean) option to show progress bar to see progress of the function
#'
#' @details
#'  run the optimizer with the following options for output:
#'  "total_impact" gives the total change in SOC, Nsp and yield for all measures combined
#'  "score_long" gives the integrative score per measure, as a long table
#'  "score_wide" gives the integrative score per measure, as a wide table
#'  "score_long_combi" gives the integrative score per combination measure, as a long table
#'  "score_wide_combi" gives the integrative score per measure, as a wide table
#'
#'  @export
runDST <- function(db, dt.m, output = 'total_impact',uw = c(1,1,1), simyear = 5, quiet = TRUE){

            #input is combo of 2 previously created datasets (itnegrator/eurostat + meta-models)
            # output sums over indicators or over one measure
            # user weights are defined here in order of Y, C, N (e.g. 2 is twice as important as 1)
            # simyear = time period over which we look at impacts / predicted changes
            # so C can continue to increase linearly whereas Y and N do not - so adds complexity and we could pick one simyear

  # make progress bar to know how the run time - at different points in the function it is updated while running
  if(!quiet) {pb <- txtProgressBar(min = 0, max = 8, style = 3);i=0}

  # make local copy
  d2 <- copy(db)

  # subset only the integrator database columns needed to estimate DISTANCE TO TARGET
  # how far is initial status from desired status
  d2 <- d2[,.(ncu,crop_name,NUTS2,area_ncu,yield_ref,yield_target,density,
              soc_ref,soc_target,n_sp_ref,n_sp_sw_crit,n_sp_gw_crit,c_man_ncu)]

  # re-arrange dt.m to facilitate joining with integrator data
  # create one line per NCU so everything moves back to columns
  dtm.mean <- dcast(dt.m,ncu + man_code ~ indicator, value.var = c('mmean','msd'))

  # update progress bar
  if(!quiet) {i = i+1; setTxtProgressBar(pb, i)}

  # merge the impact of measures with the integrator db
  # get integrator data plus predicted changes per indicator and man code as col
  d3 <- merge(d2,dtm.mean,by=c('ncu'),allow.cartesian = TRUE)
  # allow cartesian because we are joining 1 NCU to many measures and normally it is 1:1
  # have 1 NCU, x# of crops, duplicated for each measure

  # update progress bar
  if(!quiet) {i = i+1; setTxtProgressBar(pb, i)}

  # add estimated change in indicators for a period of 20 years
  # changes only relevant for SOC over years so simyear for others set as constant at 1
  # becomes fraction instead of % like in models
  d3[, dY := 1 * mmean_Y * 0.01]
  d3[, dSOC := simyear * mmean_SOC * 0.01]
  d3[, dNsu := 1 * mmean_Nsu * 0.01]

  # prevent that the change in SOC due to manure input exceeds available C
  d3[grepl('^CF|^OF', man_code) & dSOC > 0, dSOC := pmin(dSOC, c_man_ncu * 1000 *.1/(100 * 100 *.25 * density))]
  #filter rows for fert measures; where C is increasing; take min of predicted change and amount C avail in manure
  #****check kg/ha to mg/kg conversion

  # DISTANCE TO TARGET APPROACH
  # add score reflecting distance to given target, being a linear function

  # for yield and SOC
  d3[, sY := 1 - pmin(1,((1 + dY) * yield_ref) / yield_target)]
  d3[, sSOC := 1 - pmin(1,((1 + dSOC) * soc_ref) / soc_target)]
  d3[, sNsu := pmax(0,1 - (1 + dNsu) * n_sp_ref / pmin(n_sp_sw_crit,n_sp_gw_crit))]

  # update progress bar
  if(!quiet) {i = i+1; setTxtProgressBar(pb, i)}

  # average the impact of measures and scores over crop types to get area weighted mean per ncu
  d4 <- copy(d3)[,.(ncu,crop_name,man_code, NUTS2,area_ncu,dY,dSOC,dNsu,sY,sSOC,sNsu)]
  cols <- colnames(d4)[grepl('^dY|^sY|^sSOC|^dSOC|^sNsu|^dNsu',colnames(d4))]
  d4 <- d4[,lapply(.SD,function(x) weighted.mean(x,area_ncu)),.SDcols = cols,by=c('ncu','man_code')]

  # update progress bar
  if(!quiet) {i = i+1; setTxtProgressBar(pb, i)}

  if(output=='total_impact'|output=='all'){

        #combination of the measures - sum impact of all over each indicator (without site/user weighting)

    # RELATED TO DIMINISHING ADDITIVE EFFECT WHEN MULTIPLE MEASURES APPLIED TOGETHER (UNLIKLY TO BE MAX ADDITIVE WHEN ALTOGETHER)
    # estimate the total impact of all measures combined (be aware, the change is a fraction)
    # Using a simple weighting factor where the additive contribution declines with the
    # order of the measure when sorted from highest to lowest impact
    # output is relative change (in %) over a x-year period (given by simyear)
    d4[, c('odY','odSOC','oNsu') := lapply(.SD,function(x) frankv(abs(x),order=-1)),.SDcols = c('dY','dSOC','dNsu'),by='ncu']
    # still aggregated weighted avg but fraction form
    out1 <- d4[,list(dY = sum(dY/odY), dSOC = sum(dSOC/odSOC),dNsu = sum(dNsu/oNsu)),by = 'ncu']



  }
  # update progress bar
  if(!quiet) {i = i+1; setTxtProgressBar(pb, i)}

    #now summing over each individual measure with site and user weights integrated ; results in rank of measures

  #SINGLE MEASURES - ORDERED
  #sorting/ranking of measures per NCU
  #depends how far from target ; also how important each individual indicator is (user weight)
  if((grepl('score', output) & !grepl('combi$',output))|output=='all'){

    # estimate the integrative score per measure, and sort them on importance
    # the lower the score, the better the measure helps to reach desired targets
    #weighted avg of a measure (uw) - still coming out bw 0-1
    #distance for each indicator can come out as 2x the other, etc.
    d4[,bipm := uw[1] * sY + uw[2] * sSOC + uw[3] * sNsu / sum(uw)]
    #orders each measure (positive now)
    d4[,bipms := as.integer(frankv(bipm)),by='ncu']
    #adds the measure and ranking
    out2 <- d4[,.(ncu,man_code,bipms)]
    #here 2 measures can have identical ranking

    #SCORE ENDS UP VALUE FROM 0-1; EACH MEASURE GETS A RANK COMPARED TO OTHERS (LIKE OUTCOME PAPER 1)

  }
  # update progress bar
  if(!quiet) {i = i+1; setTxtProgressBar(pb, i)}

  #what combinations of measures are best or worst when applied together (including )

  #GERARD ADAPTS SO THAT SOME COMBINATIONS CANNOT TAKE PLACE (RT/NT), AND TO ALLOW FOR MORE THAN 2 MEASURES COMBINED
  #might look at what needed to achieve targets yes/no (e.g. does adding more measures actually reach goals?)

  #DIFFERENT COMBINATIONS OF MEASURES - ORDERED - ONLY MAX 2 AT ONCE RATHER THAN MORE - BUT WE CAN ADAPT THIS TO REALISTIC COMBO
  if((grepl('score', output) & grepl('combi$',output))|output=='all'){

    #collect score columns
    cols <- colnames(d4)[grepl('^s|man|ncu',colnames(d4))]
    #merge with itself to get combos of measures (listed in cols) so here 7x7
    d4 <- merge(d4[,mget(cols)],d4[,mget(cols)],by='ncu',allow.cartesian = T, suffixes = c('_m1','_m2'))
    #only selecting m1-m2, etc not m1-m1
    d4 <- d4[man_code_m1 != man_code_m2]
    #changing names of man to number/code
    d4[,man_code_fm1 := as.numeric(as.factor(man_code_m1))]
    d4[,man_code_fm2 := as.numeric(as.factor(man_code_m2))]
    #removes duplicates by changing names between columns and then removing second row
    d4[,c('c1','c2') := list(pmax(man_code_fm1,man_code_fm2),pmin(man_code_fm1,man_code_fm2)),by=c('man_code_fm1','man_code_fm2')]
    d4 <- unique(d4,by=c('ncu','c1','c2'))
    #remove temporary cols
    d4[,c('c1','c2','man_code_fm1','man_code_fm2') := NULL]

    #first sort each measure because diminishing impacts for added measures
    #highest one counts 100% second 50%
    #then what is change in dist to target for each ind
    d4[,bipmc := (uw[1] * (pmax(sY_m1,sY_m2) + pmin(sY_m1,sY_m2)*0.5) +
                    uw[3] * (pmax(sNsu_m1,sNsu_m2) + pmin(sNsu_m1,sNsu_m2)*0.5) +
                    uw[2] * (pmax(sSOC_m1,sSOC_m2) + pmin(sSOC_m1,sSOC_m2)*0.5))/sum(uw)]
    #ranks different combinations on the total integrated score for all 3 indicators
    d4[,bipmcs := as.integer(frankv(bipmc)),by='ncu']
    #adding col to identify combo of measures
    d4[,mcombi := paste0(man_code_m1,'_',man_code_m2)]
    #table with ncu, man name, ranking; might add combined score as well (bipmc)
    out3 <- d4[,.(ncu,mcombi,bipmcs)]

  }
  # update progress bar
  if(!quiet) {i = i+1; setTxtProgressBar(pb, i)}

  # select relevant output to be returned
  if(output == 'total_impact'){out <- out1}
  if(output == 'score_long'){out <- out2}
  if(output == 'score_wide'){out <- dcast(out2,ncu ~ man_code,value.var = 'bipms')}
  if(output == 'score_long_combi'){out <- out3}
  if(output == 'score_wide_combi'){out <- dcast(out3,ncu ~ mcombi,value.var = 'bipmcs')}
  if(output == 'all'){out <- list(total_impact = out1, score_long = out2, score_long_combi = out3)}

  # update progress bar
  if(!quiet) {i = i+1; setTxtProgressBar(pb, i);close(pb)}

  # return output
  return(out)
}

#' Function to run the DST including Monte Carlo simulations to estimate the mean and stand deviations of the DST output

#' @param db (data.table) a data.table with INTEGRATOR data used for the DST
#' @param measures (data.table) a vector with different management options to be estimated. Options include: c('CC','RES','ROT','NT-CT','RT-CT','CF-MF','OF-MF')
#' @param uw (vector) (user weight) is an argument that gives the relative importance for yield, soc or n-surplus (a value above 0, likely a fraction between 0 and 2). Default is c(1,1,1).
#' @param nsim (integer) the number of Monte Carlo simulations to be runned
#' @param mam (list) a list containing the generic meta-analytical model information
#' @param simyear (num) value for the default year for simulation of impacts (default is 5 years)
#' @param covar (boolean) is it desired to make use of the ma-models including covariates. Options: TRUE/FALSE.
#' @param output (string) optional argument to select different types of outputs. Options include: 'total impact', 'score_long','score_wide','score_long_combi', or 'score_wide_combi', or 'all'.
#'
#' @export
runMC_DST <- function(db, uw = c(1,1,1),simyear = 5,
                      measures = c('CC','RES','ROT','NT-CT','RT-CT','CF-MF','OF-MF'),
                      mam = ma_models, nsim = 10, covar = TRUE,
                      output = 'all'){

  #we can adapt nsim later to be what needed (at least 100 but needs a lot of time)
  #output is the different options in runDST

  # make local copy of the input database integrator
  d1.rmc <- copy(db)

  # make list to store output of join MA- impact models per management measure as well ass Monte Carlo simulations
  dt.list = sim.list = sim.list1 = sim.list2 = sim.list3 = list()

  # function to calculate the model
  #table is counting frequency of occurance within a column
  #stores the most frequent option
  fmod <- function(x) as.integer(names(sort(table(x),decreasing = T)[1]))

  # make progress bar
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)

  # do Monte Carlo simulations
  for(i in 1:nsim){

    dt.list = list()


    # join MA-impact models per management measure (those defined above in measures)
    # cIMAm gives different output each time if mc set to true; here running various sims
    # list of separate data tables per measure
    for(j in measures){dt.list[[j]] <- cIMAm(management= j,db = d1.rmc, mam = mam, covar = covar, montecarlo = TRUE)}

    # combine all measures and their impacts into one data.table
    dt.m <- rbindlist(dt.list)

    sim <- runDST(db = d1.rmc, dt.m = dt.m, output = output,uw = uw,simyear = simyear,quiet = TRUE)

    # ?? CHECKING 1 NCU sim$total_impact[ncu==1830]

    #adding column with sim number
    if(output !='all'){
      sim.list[[i]] <- copy(sim)[,sim:=i]
    } else {
      #if you want all, multiple outputs each in a list
      sim.list1[[i]] <- sim$total_impact[,sim:=i]
      sim.list2[[i]] <- sim$score_long[,sim:=i]
      sim.list3[[i]] <- sim$score_long_combi[,sim:=i]
    }

    # update progress bar (just for simulation)
    setTxtProgressBar(pb, i)

  }

  # combine all in three objects
  if(output !='all'){
    out <- rbindlist(sim.list)
  } else{
    #summarizing the output and finding most frequent ranking
    out1 <- rbindlist(sim.list1)
    #scores
    out1 <- out1[,as.list(unlist(lapply(.SD,function(x) list(mean = mean(x), sd = sd(x))))),.SDcols = c('dY','dSOC','dNsu'),by=ncu]
    out2 <- rbindlist(sim.list2)
    #ind measures
    out2 <- out2[, .(modal = fmod(bipms)),by=c('ncu','man_code')]
    out3 <- rbindlist(sim.list3)
    #combo of measures
    out3 <- out3[, .(modal = fmod(bipmcs)),by=c('ncu','mcombi')]

    #combine all output
    out <- list(total_impact = out1, score_long = out2, score_long_combi = out3)
  }

  # close progressbar
  close(pb)

  # return output
  return(out)
}

#' Function to to link Integrator to MA models being available.

#' @param db (data.table) a data.table with INTEGRATOR data used for the DST
#' @param management (string) a management code with different management options to be estimated. Options include: c('CC','RES','ROT','NT-CT','RT-CT','CF-MF','OF-MF')
#' @param mam (list) a list containing the generic meta-analytical model information
#' @param covar (boolean) is it desired to make use of the ma-models including covariates. Options: TRUE/FALSE.
#' @param montecarlo (boolean) is it desired to make use of MC estimates of the ma-model estimates. Options: TRUE/FALSE.
#'
#' @export
cIMAm <- function(management,db = d1, mam = ma_models,montecarlo = FALSE, covar = TRUE){

  # select the right column for the area
  # one of the following 7 measures is selected when running the function
  if(management=='CC'){
    dt.m1 <- db[,.(ncu,ha_m1 = parea.cc)] # just selecting area from integrator
    dt.cov.m1 <- db[,.(ncu,cov_soil,cov_clim,cov_crop,cov_fert,cov_soc, ha_m1 = parea.cc)] # separate selection adding covariate property
  } else if(management=='RES'){
    dt.m1 <- db[,.(ncu,ha_m1 = parea.cres)]
    dt.cov.m1 <- db[,.(ncu,cov_soil,cov_clim,cov_crop,cov_fert,cov_soc, ha_m1 = parea.cres)]
  } else if(management=='ROT'){
    dt.m1 <- db[,.(ncu,ha_m1 = parea.cr)]
    dt.cov.m1 <- db[,.(ncu,cov_soil,cov_clim,cov_crop,cov_fert,cov_soc, ha_m1 = parea.cr)]
  } else if(management=='NT-CT'){
    dt.m1 <- db[,.(ncu,ha_m1 = parea.ntct)]
    dt.cov.m1 <- db[,.(ncu,cov_soil,cov_clim,cov_crop,cov_fert,cov_soc, ha_m1 = parea.ntct)]
  } else if(management=='RT-CT'){
    dt.m1 <- db[,.(ncu,ha_m1 = parea.rtct)]
    dt.cov.m1 <- db[,.(ncu,cov_soil,cov_clim,cov_crop,cov_fert,cov_soc, ha_m1 = parea.rtct)]
  } else if(management %in% c('OF-NF','CF-NF','MF-NF')){
    #dt.m1 <- db[,.(ncu,ha_m1 = parea.nifnof)]
    #dt.cov.m1 <- db[,.(ncu,cov_soil,cov_clim,cov_crop,cov_fert,cov_soc, ha_m1 = parea.nifnof)]
    stop('this management measure was not yet available in MA models. Please, update the function by adding which area the measure can be applied')
    #might add these later if available

  } else if(management=='CF-MF'){
    # impact of partly replacing mineral fertilizers by organic ones
    # since impact is lower on highly fertilized soils, I halve the area (being identical to half the impact)
    dt.m1 <- db[,.(ncu,ha_m1 = parea.mifnof + 0.5 * parea.hifnof)]
    dt.cov.m1 <- db[,.(ncu,cov_soil,cov_clim,cov_crop,cov_fert,cov_soc, ha_m1 = parea.mifnof + 0.5 * parea.hifnof)]
  } else if(management=='OF-MF'){
    # replacing inorganic with organic has only impact on soils with no organic input and limited inorganic
    dt.m1 <- db[,.(ncu,ha_m1 = parea.nifnof + parea.mifnof)]
    dt.cov.m1 <- db[,.(ncu,cov_soil,cov_clim,cov_crop,cov_fert,cov_soc, ha_m1 = parea.nifnof + parea.mifnof)]
  }

  # unlist the meta-analytical models
  ma_mean <- mam$ma_mean
  ma_sd <- mam$ma_sd
  ma_cov_mean <- mam$ma_cov_mean
  ma_cov_sd <- mam$ma_cov_sd

  # join the integrator database with the main meta-analytical models
  dt.m1 <- dt.m1[,.(ha_m1 = sum(ha_m1),man_code=management),by=ncu] #selecting area and adding man code
  dt.m1 <- merge(dt.m1,ma_mean[man_code==management],by='man_code') #merging mean estimate
  dt.m1 <- merge(dt.m1,ma_sd[man_code==management],by='man_code') #again for SD

  #extract names for indicators (N, C, Y), by removing mean/sd text - selects those in your db
  cols <- unique(gsub('^mean_','',colnames(dt.m1)[grepl('^mean',colnames(dt.m1))]))
  cols <- unique(gsub('^mean_|^sd_','',colnames(dt.m1)[grepl('^mean|^sd',colnames(dt.m1))]))

  #changing columns to row for each indicator (instead of wide it becomes long with a variable to specificy the indicator)
  #also col for mean and sd
  dt.m1 <- melt(dt.m1,id.vars = c('ncu','ha_m1'),
                measure=patterns("^mean_", "^sd_"),
                variable.factor = TRUE,
                value.name = c('mean','sd'),
                variable.name = 'indicator')
  dt.m1[,indicator := cols[as.integer(indicator)]] #adds name for each number assigned to the indicator
  dt.m1[ha_m1 == 0,  c('mean','sd') := list(mean = 0, sd = 0)]


  # estimate the mean impact given normal distribution
  if(montecarlo==TRUE){
    suppressWarnings(dt.m1[,mean := rnorm(.N,mean[1],sd = sd[1]),by='indicator'])
    #create normal distribution (N=number samples or rows for each indicator); select first row because all the same
    #using mean and sd of global models for each indicator-measure combon as basis for MC simulation
    dt.m1[ha_m1==0,mean := 0]
  }

  #some NCUs have no area for a measure;if so the impact should be zero because in end we sum impacts by area to get total impact
  #at this point connected: NCU + area of measure potentially applied + impacts of measures


  if(covar==TRUE){

    # join the integrator database with the meta-analytical models that uses covariables
    dt.cov.m1 <- dt.cov.m1[,.(ha_m1 = sum(ha_m1),unid = .GRP),by=.(ncu,cov_soil,cov_clim,cov_crop,cov_fert,cov_soc)]
    #unique id for all combos of covar with area
    dt.cov.m1[,man_code := management] #input management code you selected
    dt.cov.m1 <- melt(dt.cov.m1,id.vars = c('ncu','ha_m1','man_code','unid'),value.name = 'group') #make columns to rows
    dt.cov.m1[,variable := NULL] #removed column for covariable name to save memory because codes for each group is unique to variable

    dt.cov.m1 <- merge(dt.cov.m1,ma_cov_mean[man_code==management],by=c('group','man_code'))
    #long list of models and filter the management code and join based on right groups (e.g. CC and cov_crop )
    dt.cov.m1 <- unique(dt.cov.m1)[,mods:=NULL] #remove some duplicated NCUs
    dt.cov.m1 <- merge(dt.cov.m1,ma_cov_sd[man_code==management],by=c('group','man_code'))
    #same long list operation for SD
    dt.cov.m1 <- unique(dt.cov.m1)

    #now have predicted change for models including covariates

    #takes an average of the covariate models applicaple for that NCU
    cols <- colnames(dt.cov.m1)[grepl('mean_|sd_',colnames(dt.cov.m1))]
    #not sure why a weighted mean used? is it for different areas of crops in each NCU?
    #considering coarse groups of meta-models, there are different types of each group in the list per NCU
    dt.cov.m1 <- dt.cov.m1[,lapply(.SD,function(x) weighted.mean(x,w=ha_m1,na.rm=T)),.SDcols = c(cols),by=c('ncu','man_code')]
    # if area missing then set to zero change
    dt.cov.m1[,c(cols):= lapply(.SD,function(x) fifelse(is.na(x),0,x)),.SDcols = cols] #GERARD CHECKS THIS LINE (SEE BELOW)
    #reshaping columns to rows
    cols <- unique(gsub('^mean_|^sd_','',colnames(dt.cov.m1)[grepl('^mean|^sd',colnames(dt.cov.m1))]))
    dt.cov.m1 <- melt(dt.cov.m1,id.vars = c('ncu','man_code'),
                      measure=patterns("^mean_", "^sd_"),
                      variable.factor = TRUE,
                      value.name = c('cov_mean','cov_sd'),
                      variable.name = 'indicator')
    dt.cov.m1[,indicator := cols[as.integer(indicator)]] #assigning indicator name instead of integer

    #
    # estimate the mean impact given normal distribution
    if(montecarlo==TRUE){
      suppressWarnings(dt.cov.m1[,cov_mean := rnorm(.N,cov_mean[1],sd = cov_sd[1]),by=indicator])
      dt.cov.m1[cov_sd==0,cov_mean := 0]
    }



    # merge the models with and without covariates and select covariates when available
    #want to select covar model over the global model; without cov take global one
    #covar model at this point is average of the site properties applicable for that NCU
    dt.m1.fin <- merge(dt.m1,dt.cov.m1,by=c('ncu','indicator'))
    dt.m1.fin[,mmean := fifelse(is.na(cov_mean),mean,cov_mean)] #IF COVARIATE MODEL IS NA THEN TAKE GLOBAL; BUT ABOVE CHANGED NAs to 0
    dt.m1.fin[,msd := fifelse(is.na(cov_mean),sd,cov_sd)]
    dt.m1.fin <- dt.m1.fin[,.(ncu,ha_m1,indicator,man_code,mmean,msd)] #define final output = #NCUs * #indicators = 88000

  } else {
    dt.m1.fin <- dt.m1[,.(ncu,ha_m1,indicator,man_code = management,mmean = mean,msd = sd)]
  }


  # return output
  return(dt.m1.fin)
}


# function to load the default meta-analytical models from a given filename
# and the different models are combined using inverse weighting on SD

#' @param fname (string) a filename where to find the excel with ma model results
#'
#' @export
lmam <- function(fname){

  # load meta-analytical models for main models without covariates
  m1.main <- as.data.table(readxl::read_xlsx(fname,sheet='main'))
  setnames(m1.main,gsub('-1','',colnames(m1.main)))

  # remove cases that are not correct
  m1.main<- m1.main[!(n==1 & dyr==1 & SEyr==0.1)] # "markers" in the template file

  # load meta-analytical models for main models with covariates, and select only relevant columns
  m1.covar <- as.data.table(readxl::read_xlsx(fname,sheet='covariates'))
  setnames(m1.covar,gsub('-1','',colnames(m1.covar)))
  m1.covar <- m1.covar[,.(ind_code, mods = `moderator/factor` ,man_code,group,SEyr,dyr,n)]

  # remove models without SE
  m1.covar <- m1.covar[!is.na(SEyr)]

  # add simplified grouping for moderators to simplify joining with integrator data later
  # groups are unique
  m1.covar[grepl('texture',mods), mods := 'cov_soil']
  m1.covar[grepl('crop',mods), mods := 'cov_crop']
  m1.covar[grepl('SOC',mods), mods := 'cov_soc']
  m1.covar[grepl('rate',mods), mods := 'cov_fert']
  m1.covar[grepl('pH',mods), mods := 'cov_ph']
  m1.covar[grepl('durat',mods), mods := 'cov_duration']
  m1.covar[grepl('climate',mods), mods := 'cov_clim']
  m1.covar <- m1.covar[!mods %in% c('cov_ph','cov_duration')]
  m1.covar[grepl('cov_soil',mods) & grepl('coarse|sand',group), group2 := 'coarse']
  m1.covar[grepl('cov_soil',mods) & grepl('medium|loam',group), group2 := 'medium']
  m1.covar[grepl('cov_soil',mods) & grepl('fine|clay|silt',group), group2 := 'fine']
  m1.covar[grepl('cov_crop',mods) & grepl('maiz',group), group2 := 'maize']
  m1.covar[grepl('cov_crop',mods) & grepl('cerea',group), group2 := 'cereals']
  m1.covar[grepl('cov_crop',mods) & grepl('root',group), group2 := 'rootcrops']
  m1.covar[grepl('cov_clim',mods) & grepl('subtropical',group), group2 := 'subtropical']
  m1.covar[grepl('cov_clim',mods) & grepl('temperate',group), group2 := 'temperate']
  m1.covar[grepl('cov_soc|cov_fert',mods) & grepl('low|no fert',group), group2 := 'low']
  m1.covar[grepl('cov_soc|cov_fert',mods) & grepl('medium',group), group2 := 'medium']
  m1.covar[grepl('cov_soc|cov_fert',mods) & grepl('high',group), group2 := 'high']
  m1.covar[grepl('cov_fert',mods),group2 := paste0('f',group2)]
  m1.covar[grepl('cov_soc',mods),group2 := paste0('c',group2)]
  m1.covar <- m1.covar[!is.na(group2)][,group := group2][,group2 := NULL]

  # calculate the weighed mean models per measure
  m1.main[,wm.sd := 1/sqrt(sum(1 / SEyr^2)) , by=c('ind_code','man_code')]
  m1.main[,wm.mean := sum((dyr / SEyr^2)) / sum(1 / SEyr^2) , by=c('ind_code','man_code')]
  m1.covar[,wm.sd := 1/sqrt(sum(1 / SEyr^2)) , by=c('ind_code','man_code','mods','group')]
  m1.covar[,wm.mean := sum((dyr / SEyr^2)) / sum(1 / SEyr^2) , by=c('ind_code','man_code','mods','group')]

  # remove duplicates
  m2.main <- unique(m1.main[,.(ind_code,man_code,wm.mean,wm.sd)])
  m2.covar <- unique(m1.covar[,.(ind_code,man_code,mods,group,wm.mean,wm.sd)])

  # mean model responses for main ma-models (without covariates)
  m2.mean <- dcast(m2.main,man_code ~ ind_code, value.var = 'wm.mean')
  setnames(m2.mean,colnames(m2.mean)[-1],paste0('mean_',colnames(m2.mean)[-1]))
  m2.sd <- dcast(m2.main,man_code ~ ind_code, value.var = 'wm.sd')
  setnames(m2.sd,colnames(m2.sd)[-1],paste0('sd_',colnames(m2.sd)[-1]))

  # mean model responses for main ma-models with covariates
  m2.cov.mean <- dcast(m2.covar, man_code + mods + group ~ ind_code, value.var = 'wm.mean')
  setnames(m2.cov.mean,colnames(m2.cov.mean)[-c(1:3)],paste0('mean_',colnames(m2.cov.mean)[-c(1:3)]))
  m2.cov.sd <- dcast(m2.covar, man_code + mods + group ~ ind_code, value.var = 'wm.sd')
  setnames(m2.cov.sd,colnames(m2.cov.sd)[-c(1:3)],paste0('sd_',colnames(m2.cov.sd)[-c(1:3)]))

  # add the meta-analytical models into one list
  out = list(ma_mean = m2.mean, ma_sd = m2.sd,ma_cov_mean = m2.cov.mean, ma_cov_sd = m2.cov.sd)

  # return
  return(out)
}

# *************************************************************************************************
# helper function to re-allocate cropping categories on NUTS level to NCU level given likelihoods
# *************************************************************************************************

dsRA <- function(area_ncu,area_nuts2,lkh, nuts2){

  # make local copy of the inputs
  dt <- data.table(area_ncu, area_nuts2,lkh, nuts2)

  # convert unit for integrator NCU (km2) to match eurostat (ha)
  dt[,area_ncu := area_ncu * 100]

  # add unique id
  dt[, id := .I]

  # estimate total likely area on NUTS2 level given NCU crop area and likelihood for measure X
  dt[,a_1 := sum(lkh * area_ncu, na.rm = TRUE), by = nuts2]

  # correct this proportial to the area that is likely used for management measure X
  # the area of some NCUs already maximized/filled up, so cannot go over the total crop area
  # if likely area is less than NCU area, it saves this (see below)
  # gives an NCU likely area which is proportional to total area at the NUTS2 level
  dt[,a_2 := pmin(area_ncu,lkh * area_ncu * area_nuts2 / a_1,na.rm = TRUE), by = nuts2]

  # how much NUTS2 area leftover should still be reallocated for management measure X
  dt[,a_3 := area_nuts2 - sum(a_2), by = nuts2]

  # how much NCU area is still left within crops that have a likelihood for measure X
  # calculates area where more may be applied, if lkh is 0 then assigns 0 value
  dt[,a_4 := fifelse(lkh > 0,(area_ncu - a_2),0), by = nuts2]

  # redistribute the area that need to be reallocated to the area that is left
  # calculates exact proportion of NUTS2 area to go to each; if lkh is 0 then a_5 becomes 0
  dt[,a_5 := a_3 * a_4/sum(a_4),by = nuts2]

  # estimate total area of measure X to add to each NCU; if no more room then 0 added
  dt[,a_6 := a_2 + pmax(0,a_5,na.rm=T)]  # ??? why is pmax function needed ???

  # ensure correct order
  setorder(dt,id)

  # extract value
  value <- dt[,a_6]

  # return value
  return(value)

}

# *************************************************************************************************
# updated helper function to re-allocate cropping categories on NUTS level to NCU level given likelihoods
# *************************************************************************************************

dsRA_new <- function(area_ncu,area_nuts2,lkh, nuts2,area_nuts2_all = NULL){

  # make local copy of the inputs
  dt <- data.table(area_ncu, area_nuts2,lkh, nuts2,area_nuts2_all)

  # assume that total area for the measure equals the measure to be reallocated
  if(is.null(area_nuts2_all)){
    dt[,area_nuts2_all := area_nuts2]
  }

  # convert unit for integrator NCU (km2) to match eurostat (ha)
  dt[,area_ncu := area_ncu * 100]

  # add unique id
  dt[, id := .I]

  # estimate the total area in ncu on nutslevel
  dt[,a_0 := sum(area_ncu,na.rm = T),by=nuts2]

  # estimate total likely area on NUTS2 level given NCU crop area and likelihood for measure X
  dt[,a_1 := sum(lkh * area_ncu, na.rm = TRUE), by = nuts2]

  # correct this proportial to the area that is likely used for management measure X
  # the area of some NCUs already maximized/filled up, so cannot go over the total crop area
  # if likely area is less than NCU area, it saves this (see below)
  # gives an NCU likely area which is proportional to total area at the NUTS2 level
  dt[,a_2 := pmin(area_ncu,lkh * area_ncu * area_nuts2_all / a_1,na.rm = TRUE), by = nuts2]

  # how much NUTS2 area leftover should still be reallocated for management measure X
  dt[,a_3 := area_nuts2_all - sum(a_2), by = nuts2]

  # how much NCU area is still left within crops that have a likelihood for measure X
  # calculates area where more may be applied, if lkh is 0 then assigns 0 value
  dt[,a_4 := fifelse(lkh > 0,(area_ncu - a_2),0), by = nuts2]

  # redistribute the area that need to be reallocated to the area that is left
  # calculates exact proportion of NUTS2 area to go to each; if lkh is 0 then a_5 becomes 0
  dt[,a_5 := a_3 * a_4/sum(a_4),by = nuts2]

  # estimate total area of measure X to add to each NCU; if no more room then 0 added
  dt[,a_6 := a_2 + pmax(0,a_5,na.rm=T)]

  # estimate the fraction of measure x
  dt[,afin := a_6 * area_nuts2 / area_nuts2_all]

  # ensure correct order
  setorder(dt,id)

  # extract value
  value <- dt[,afin]

  # return value
  return(value)

}



