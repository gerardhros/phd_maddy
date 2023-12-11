
# applies downscaling to NCUs and generates db_final_europe

#################################################################################################
#   DEVELOPMENT DECISION SUPPORT TOOL MANAGEMENT-IMPACT ACROSS EUROPE
#################################################################################################
  setwd('C:/phd_maddy/')
  # load packages
  require(readxl);require(data.table)

  # clear environment
  rm(list=ls())

  # load all databases (this takes a few seconds). Result is loaded in environment.
  source('scripts/dst_loaddb.R')

  # load functions for aggregation and optimisation DST
  source('scripts/dst_functions_fert.r')

  # location of data objects not stored on github
  floc <- 'C:/dst_outputs/'
  floc <- 'D:/ESA/02 phd projects/01 maddy young/01 data/'
#################################################################################################
#   DOWNSCALING AREAL DISTRIBUTION OF MANAGEMENT STRATEGIES FROM EUROSTAT TO NCU LEVEL
#################################################################################################

  # merge NCU data with given NUTS based on country and ncu
  d3 <- merge.data.table(d1,
                         ncu_nuts_id,
                         by.x = c('ncu','country'),
                         by.y = c('ncu','NUTS0'),
                         all.x = TRUE ,
                         all.y = FALSE)

  # add a country dependent correction factor for area per NCU
  d3[,ncu_areacountry := sum(area_ncu),by=.(ncu,NUTS2)] #total of each ncu type matched at NUTS2 level
  d3[,ncu_areacountrycf := ncu_areacountry / sum(area_ncu),by=.(ncu,country)] #divide by total of ncu type at country level

  # adapt the area of the ncu
  d3[, area_ncu := ncu_areacountrycf * area_ncu]

  # join with EU database - LATER TILLAGE AND SOIL COVER SHOULD BE ADDED #310,418
  d3 <- merge(d3,d2.arable,by.x = 'NUTS2',by.y='nuts_code',all.x=TRUE)
  d3 <- merge(d3,d2.rot[,mget(colnames(d2.rot)[!grepl('nuts_name|country_',colnames(d2.rot))])],by.x = 'NUTS2',by.y='nuts_code',all.x=TRUE)
  d3 <- merge(d3,d2.tillage[,mget(colnames(d2.tillage)[!grepl('nuts_name|country_',colnames(d2.tillage))])],by.x = 'NUTS2',by.y='nuts_code',all.x=TRUE)
  d3 <- merge(d3,d2.cover[,mget(colnames(d2.cover)[!grepl('nuts_name|country_',colnames(d2.cover))])],by.x = 'NUTS2',by.y='nuts_code',all.x=TRUE)

  # about 67 ha (< 0.01%) has no NUTS code, so delete these ncu's (~400 rows)  #309,961
  d3 <- d3[!is.na(NUTS2)]

  # add total area of integrator per NUTS2 area
  d3[,area_int_nuts := sum(area_ncu*100,na.rm = TRUE), by =.(NUTS2)]

  # join likelyhood continous cropping to the ncu database
  d3 <- merge(d3,lkh.crop,by='crop_name',all.x=TRUE)

  # subset for testing purposes
  # d3 <- subset(d3,country == 'BE')

  ############################## calculate the area continuous cropping ##############################

  # estimate the NCU crop area used for continuous cropping (opposite of rotation, so likelihood is 1 minus likelihood)
  d3[, rot_area_cont := dsRA_new(area_ncu = area_ncu, area_nuts2 = area_rot_cont,lkh = 1-lh.rot, nuts2 = NUTS2)]

  # estimate the NCU crop area used for 0-25% crop rotation (likelihood is likelihood for rotation)
  d3[, rot_area_cr25 := dsRA(area_ncu = area_ncu, area_nuts2 = area_rot_25, lkh = lh.rot, nuts2 = NUTS2)]

  # estimate the NCU crop area used for 25-50% crop rotation (likelihood is likelihood for rotation)
  d3[, rot_area_cr50 := dsRA(area_ncu = area_ncu, area_nuts2 = area_rot_50, lkh = lh.rot, nuts2 = NUTS2)]

  # estimate the NCU crop area used for 50-75% crop rotation (likelihood is likelihood for rotation)
  d3[, rot_area_cr75 := dsRA(area_ncu = area_ncu, area_nuts2 = area_rot_75, lkh = lh.rot, nuts2 = NUTS2)]

  # estimate the NCU crop area used for >75% crop rotation (likelihood is likelihood for rotation)
  d3[, rot_area_cr100 := dsRA(area_ncu = area_ncu, area_nuts2 = area_rot_100, lkh = lh.rot, nuts2 = NUTS2)]

  # what is total NCU crop area already under crop rotation (in ha), summing over different categories
  d3[, rot_area_tot := rot_area_cont * 0 + rot_area_cr25 * 0.125 + rot_area_cr50 * 0.375 +
       rot_area_cr75 * 0.625 + rot_area_cr100 * 0.875]

  # remove columns no longer needed
  cols <- c('area_arable_rot','area_rot_cont', 'area_rot_25', 'area_rot_50', 'area_rot_75', 'area_rot_100', 'area_rot_na',
            'rot_area_cont','rot_area_cr25', 'rot_area_cr50', 'rot_area_cr75', 'rot_area_cr100')
  d3[,c(cols) := NULL]

  # area of arable land within each crop type where crop rotation may potentially be applied (in ha)
  d3[, area_mon := area_ncu * 100 - rot_area_tot]

  # if there is zero applicability of rotation, the full area is assigned monoculture but it is
  # instead fully permanent crops with no potential for conversion
  d3[lh.rot == 0, area_mon := 0]

  #cols to check if total area matches up
  d3[,sum_rot := rot_area_tot + area_mon]
  d3[,area_ncu_ha := area_ncu*100]
  d3[,test_rot :=  area_ncu_ha - sum_rot]

  # what is the potential area for more cereals in crop rotation / diversification of crop rotation plan (cr)
  d3[,parea.cr := area_mon]

  # remove variables not needed any more
  d3[,c('sum_rot','test_rot','lh.rot','area_mon','rot_area_tot') := NULL]


############################## calculate the soil cover area ######################################

    # corrects soil cover columns so that the totals do not go over total cover given by EUROSTAT?
    d3[,area_arable_cov_sum := area_cov_bare + area_cov_cover + area_cov_per + area_cov_res + area_cov_winter +area_cov_excl]
    cols <- colnames(d3)[grepl('^area_cov', colnames(d3))]
    d3[,c(cols) := lapply(.SD,function(x) x * area_arable_cov_sum / area_arable_cov), .SDcols = cols]

    # normal harvested winter cover
    d3[, area_win := dsRA_new(area_ncu = area_ncu, area_nuts2 = area_cov_winter,
                              lkh = lh.win, nuts2 = NUTS2,
                              area_nuts2_all = area_arable_cov_sum )]

    # cover/intermediate crop
    d3[, area_cov := dsRA_new(area_ncu = area_ncu, area_nuts2 = area_cov_cover,
                              lkh = lh.cov, nuts2 = NUTS2,
                              area_nuts2_all = area_arable_cov_sum)]

    # plant residue cover
    d3[, area_res := dsRA_new(area_ncu = area_ncu, area_nuts2 = area_cov_res,
                              lkh = lh.res, nuts2 = NUTS2,
                              area_nuts2_all = area_arable_cov_sum)]

    # multi-annuals
    d3[, area_per := dsRA_new(area_ncu = area_ncu, area_nuts2 = area_cov_per,
                              lkh = lh.per, nuts2 = NUTS2,
                              area_nuts2_all = area_arable_cov_sum)]

    # bare soil
    d3[, area_bar := dsRA_new(area_ncu = area_ncu, area_nuts2 = area_cov_bare,
                              lkh = lh.bar, nuts2 = NUTS2,
                              area_nuts2_all = area_arable_cov_sum)]

    # excluded/non applicable area (permanent cover)
    d3[, area_exc := dsRA_new(area_ncu = area_ncu, area_nuts2 = area_cov_excl,
                              lkh = lh.exc, nuts2 = NUTS2,
                              area_nuts2_all = area_arable_cov_sum)]

    # area non cropland
    d3[, area_ncl := area_ncu_ha - (area_win + area_cov + area_res + area_per + area_bar + area_exc)]
    d3[, sum_cov := area_ncl + area_win + area_cov + area_res + area_per + area_bar + area_exc]

    # what is the potential area for cover crops application (cc) and area for crop residue application
    d3[,parea.cc := fifelse(lh.cov < 0.02,0, area_ncl + area_exc + area_bar)]
    d3[,parea.cres := fifelse(lh.res < 0.02,0, area_ncl + area_exc + area_bar + area_per + area_win)]

    # remove variables not needed any more
    d3[,c('lh.win','lh.cov','lh.res','lh.per','lh.bar','lh.exc','area_arable_cov_sum',
          'area_arable_cov', 'area_cov_excl', 'area_cov_winter', 'area_cov_cover', 'area_cov_per', 'area_cov_res', 'area_cov_bare') := NULL]



# -----------------------------------------------------------------------------------------------
# ADD LIKELIHOODS TILLAGE BASED ON correlation with CROPPING PRACTICES
# -----------------------------------------------------------------------------------------------

    # join likelyhood continous cropping to the ncu database
    d4 <- merge(d3,lkh.till[,.(crop_name,lh.ctill = lh)],by='crop_name',all.x=TRUE)

    # reduced and no till are more likely where conservation practices occur (cover and residues)
    d4[, lh.ntill := pmin(fifelse((area_win + area_per + area_bar) / sum_cov > 0.5,lh.ctill + 0.3,lh.ctill),1)]
    d4[, lh.rtill := pmin(fifelse((area_win + area_per) / sum_cov > 0.5,lh.ctill + 0.3,lh.ctill),1)]

    # area conventional tilled soil
    d4[, area_conv_till := dsRA_new(area_ncu = area_ncu, area_nuts2 = area_till_conv,
                              lkh = lh.ctill, nuts2 = NUTS2, area_nuts2_all = area_arable_till)]

    # area conservative tilled soil
    d4[, area_conserv_till := dsRA_new(area_ncu = area_ncu, area_nuts2 = area_till_conserv,
                                lkh = lh.ctill, nuts2 = NUTS2, area_nuts2_all = area_arable_till)]

    # area no-till soil
    d4[, area_no_till := dsRA_new(area_ncu = area_ncu, area_nuts2 = area_till_no,
                                       lkh = lh.ntill, nuts2 = NUTS2, area_nuts2_all = area_arable_till)]

    # what is the potential area that can be used for tillage practices
    d4[,parea.rtct := fifelse(lh.ctill == 0, 0, area_conv_till)]
    d4[,parea.ntct := fifelse(lh.ctill == 0 | grepl('potato|sugar',tolower(crop_name)),0,area_conv_till+area_conserv_till)]

    # remove variables not needed anymore
    d4[,c('lh.ctill','lh.ntill','lh.rtill','area_arable_till', 'area_till_excl', 'area_till_conv' ,'area_till_conserv', 'area_till_no',
          'area_conv_till','area_conserv_till',
          'area_win', 'area_cov' ,  'area_res',  'area_per', 'area_bar','area_exc', 'area_ncl') := NULL]


# -----------------------------------------------------------------------------------------------
# calculate fertilizer strategy (moving all grassland potential organic manure to arable)
# -----------------------------------------------------------------------------------------------

    # how much organic manure is available on grassland within a NUTS region
    # ***** THIS CAN ACTUALLY BE ADAPTED WITH TOTAL PERMANENT GRASSLAND FROM EUROSTAT ?? ****
    # n_man = N inputs from manure
    d4[, n_man_nuts := fifelse(grepl('grassland|Gras',crop_name),n_man,0)] #select manure N only from grassland
    d4[, n_man_nuts := sum(n_man_nuts,na.rm = TRUE),by = NUTS2] #sum over NUTS2 region

    # this amount might be adapted to the volume produced in stables
    # so correct for grazing (see e.g. Eurostat); a relative fraction of total applied

    # what is the total effective N input on arable land (available manure and mineral on all NON-grass crops)
    # assume only 60% of manure N available may be applied on grassland and for other crops 0
    d4[, neff := fifelse(!grepl('grassland|Gras',crop_name),n_man * 0.6 + n_fert,0)]

    # add the extra manure from grassland to all cropland relative to the total N effective dose
    # so that manure at NUTS2 level is proportionally redistributed to each NCU, and none is added to grassland
    d4[, n_man_ncu := n_man_nuts * neff / sum(neff,na.rm = TRUE), by = NUTS2]

    ############################## calculate max change in SOC from organic inputs ######################################

    # assume that 50% of manure is slurry and 50% is solid manure
    # slurry has 50 kg EOS (effect organic material) per ton and solid manure has 109 kg EOS per ton
    # see also humification rations and CN ratios (12 and 17)
    # https://www.handboekbodemenbemesting.nl/nl/handboekbodemenbemesting/Handeling/Organische-stofbeheer/Organische-stofbalans/Kengetallen-organische-stof.htm
    # this is the max change in SOC allowed

    # multiply slurry and manaure parts by
    # CN ratios, humification coefficient of 0.7 (fraction convered to humus), and fraction Norg
    # kg C / ha
    d4[, c_man_ncu := 0.5 * n_man_ncu * 12 * 0.7 * (2.1/4.0) +
                      0.5 * n_man_ncu * 17 * 0.7 * (6.6/7.7)]

    # area for combination of fertilizer vs inorganic fertilizers

    # situation NIFNOF: all cases with less than 40 kg N / ha
    d4[,parea.nifnof := fifelse(!grepl('gras',tolower(crop_name)) & n_man < 40 & n_fert < 40, area_ncu * 100, 0)]

    # medium fertilized, low animal N-input (MIFNOF): all cases up to 100 kg N / ha and less than 40 kg N/ha from animal manure
    d4[,parea.mifnof := fifelse(!grepl('gras',tolower(crop_name)) & n_man < 40 & n_fert >= 40 & n_fert < 100, area_ncu * 100, 0)]

    # highly fertilized, low animal N-input (HIFNOF): all cases with more than 100 kg N /ha and less than 40 kg N/ha from animal manure
    d4[,parea.hifnof := fifelse(!grepl('gras',tolower(crop_name)) & n_man < 40 & n_fert >= 100, area_ncu * 100, 0)]

    # medium fertilized, high animal N-input (MIFHOF): all cases up to 100 kg N / ha and more than 40 kg N/ha from animal manure
    d4[,parea.mifhof := fifelse(!grepl('gras',tolower(crop_name)) & n_man >= 40 & n_fert >= 40 & n_fert < 100, area_ncu * 100, 0)]

    # highly fertilized, high animal N-input (HIFHOF): all cases with more than 100 kg N /ha and more than 40 kg N/ha from animal manure
    d4[,parea.hifhof := fifelse(!grepl('gras',tolower(crop_name)) & n_man >= 40 & n_fert >= 100, area_ncu * 100, 0)]

    # add type MA model
    d4[parea.nifnof > 0,fert_type := 'nifnof']
    d4[parea.mifnof > 0,fert_type := 'mifnof']
    d4[parea.hifnof > 0,fert_type := 'hifnof']
    d4[parea.mifhof > 0,fert_type := 'mifhof']
    d4[parea.hifhof > 0,fert_type := 'hifhof']
    d4[is.na(fert_type),fert_type := 'none']

    # remove
    d4[,c('country_description','nuts_name') := NULL]

    #add letters to covariate names so that low/medium/high categories are different for each variable
    d4[,cov_fert := paste0('f',cov_fert)]
    d4[,cov_soc := paste0('c',cov_soc)]

    # save file as csv
    fwrite(d4,paste0(floc,'db_final_europe_y2.csv'))
