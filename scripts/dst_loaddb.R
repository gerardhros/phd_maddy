

#################################################################################################
#                       LOAD DATA: INTEGRATOR, CROP LIKELIHOODS
#################################################################################################

#this is automatically sourced by dst_prepare_input_fert and so it does not have to be run directly
#LOAD ORIGINAL INTEGRATOR/EUROSTAT DATABASES AND LIKELIHOODS
#OUTPUTS USED IN DST PREPARE INPUT
# NOTES MADDY___________________________________
#     last version before this one is 210805 (GR)
#     data in R-inputs folder
#________________________________________________



# settings
  rm(list=ls())

  # require packages
  require(data.table); require(readxl)

  # location of data objects not stored on github
  #   1. ncu/nuts spatial data (rds)
  #   2. ncu data by crop area with fertilizer (xlsx)
  #   3. capri crop names (xlsx)
  #   4. ncu covariate categories (csv)
  #   5. eurostat nuts2 codes (xls)
  #   6. eurostat categories rotation/tillage/cover
  floc <- 'C:/dst_outputs/'

  # location for gerard
  # floc <- 'D:/ESA/02 phd projects/01 maddy young/01 data/'

# load database to link NCU to NUTS levels

  # load the database which contains NUTS levels per NCU (file prepared in make_ncu_nuts.r)
  ncu_nuts_id <- readRDS(paste0(floc,'ncu_nuts_3035.rds'))

  # 4,000,000 observations - this is total NCU spatial entities (raster cell groups)
  # comes from NCU types (1 unique combo soil) repeated over multiple regions

  # select unique combo of ncu-nuts0-nuts1-nuts2 to join NCUs to NUTS2 level (and not NUTS3)
  ncu_nuts_id <- unique(ncu_nuts_id[,.(ncu,NUTS0,NUTS1,NUTS2)])
  #lists the various NCU types and corresponding NUTS2 region(s) they are present in
  #think of as "farm types" with unique soil types, relief, etc...


# load integrator data

  # read excel with NCU data (ONLY FOR ARABLE/CROPPING AREAS????)
  d1 <- as.data.table(readxl::read_xlsx(paste0(floc,'NCU data crop area.xlsx'),skip=1))

  #250,000 combinations from repeating NCU types for 40 different crop type areas in each NCU
  #includes current status and targets/limits for Y, C, N
  #Think of as "field types" with unique crop type/targets, N surplus/targets, N level

  # setnames with ****yield target and reference switched back correctly****
  setnames(d1,c('country','crop','texture','ncu','area_ncu',
                'yield_target','yield_ref','soc_ref','soc_target','n_sp_ref','n_sp_sw_crit','n_sp_gw_crit',
                'density','cn','clay','ph','n_fert','n_man','n_fix','n_dep','n_covercrop',
                'nh3_man','nh3_fert','n_leach_frac'))

  # rename code 40 (extensive grassland) to code 33 (grassland)
  d1[crop==40, crop := 33]

  # crop naming
  d1.crop <- as.data.table(readxl::read_xls(paste0(floc,'CAPRI_CROP_DEF.xls')))
  setnames(d1.crop, c('crop_id','crop_code','crop_name'))

  # merge with d1 so that the ncu data also contains crop name (and code)
  d1 <- merge(d1, d1.crop, by.x = 'crop', by.y = 'crop_id',all.x = TRUE)

  # merge with covariates
  d2 <- fread(paste0(floc,'ncu_covariates.csv'))
  d1 <- merge(d1,d2,by = c('ncu','crop_code','country'),all.x=TRUE)


#################################################################################################
#                             READ EUROSTAT DATABASES
#################################################################################################

  # load the excel table with country codes for the EU database (file prepared by Maddy)
  # select first column with names (code and full name are together)
  nuts_name_code <- as.data.table(read_xls(paste0(floc,'eurostat_codes.xls'),range = 'A1:A320'))
  # rename column
  setnames(nuts_name_code,'country_description')
  # split/extract NUTS code from the country description (full name)
  nuts_name_code[,nuts_code := tstrsplit(country_description,' - ',keep = 1)]
  # split/extract NUTS name from country description
  nuts_name_code[,nuts_name := sub(".*? - ", "", country_description)]
  # two small countries are same size as NUTS2 level, resulting in 2 identical rows => remove NUTS0 code
  nuts_name_code <- nuts_name_code[!nuts_code %in% c('LU','MT')]
  #(this removes country level codes for the two countries but leaves other countries nuts0)

  # --------
  # **********************************
  # ARABLE LAND, PERMANENT GRASSLAND, PERMANENT CROPS
  # **********************************
  # --------
  d2.arable <- as.data.table(read_xlsx(paste0(floc,'Eurostat_till-cov-rot.xlsx'),sheet='Arable land area',skip=4, range='A5:G335'))
  # change column names
  setnames(d2.arable,c('nuts_name','ar_farm','ar_ag_total','ar_arable','ar_perm_grass','ar_perm_crops','ar_gardens'))
  # remove ':' and other special characters and replace with NA
  d2.arable <- d2.arable[,lapply(.SD,function(x) gsub('\\:|Special value|not available',NA,x)),.SDcols = colnames(d2.arable)]

  cols <- colnames(d2.arable)[grepl('ar_',colnames(d2.arable))]
  d2.arable <- d2.arable[,c(cols) := lapply(.SD,function(x) pmax(0,as.numeric(x),na.rm=TRUE)),.SDcols = cols]

  # remove rows that have no nuts name
  d2.arable <- d2.arable[!is.na(nuts_name)]
  # remove full duplicates
  d2.arable <- unique(d2.arable)
  # update the EU databases with NUTS code
  d2.arable <- merge(d2.arable,nuts_name_code,by ='nuts_name',all.x = TRUE)
  # remove the rows that have totals on NUTS0 level (countries)
  d2.arable <- d2.arable[nchar(nuts_code)>2]
  # remove data where the total area is missing
  d2.arable <- d2.arable[!(ar_ag_total==0)]
  #add garden area to permanent crops and remove gardens col
  d2.arable <- d2.arable[,ar_perm_crops := ar_perm_crops + ar_gardens]
  d2.arable[,c('ar_gardens','ar_farm') := NULL]

############################## assign areas to permanent crops # LATER ?????????????????? ######################################
#
# # normal harvested winter cover
# d3[, area_win := dsRA(area_ncu = area_ncu, area_nuts2 = area_cov_winter, lkh = lh.win, nuts2 = NUTS2)]
#
# # cover/intermediate crop
# d3[, area_cov := dsRA(area_ncu = area_ncu, area_nuts2 = area_cov_cover, lkh = lh.cov, nuts2 = NUTS2)]
#
# # plant residue cover
# d3[, area_res := dsRA(area_ncu = area_ncu, area_nuts2 = area_cov_res, lkh = lh.res, nuts2 = NUTS2)]
#
# # multi-annuals
# d3[, area_per := dsRA(area_ncu = area_ncu, area_nuts2 = area_cov_per, lkh = lh.per, nuts2 = NUTS2)]
#
# # bare soil
# d3[, area_bar := dsRA(area_ncu = area_ncu, area_nuts2 = area_cov_bare, lkh = lh.bar, nuts2 = NUTS2)]
#
# # excluded/non applicable area (permanent cover)
# d3[, area_na := dsRA(area_ncu = area_ncu, area_nuts2 = area_cov_excl, lkh = lh.na, nuts2 = NUTS2)]

#  ????????????????????????????????????????????????????????






# **********************************
# ROTATION
# **********************************
# --------

  # read crop rotation (TOTAL = "ARABLE LAND AREA" PER NUTS2)
  d2.rot <- as.data.table(read_xlsx(paste0(floc,'Eurostat_till-cov-rot.xlsx'),sheet='Crop rotation',skip=4))
  # change column names
  setnames(d2.rot,c('nuts_name','area_arable_rot','area_rot_cont','area_rot_25','area_rot_50','area_rot_75','area_rot_100','area_rot_na'))
  # remove ':' and other special characters and replace with NA
  d2.rot <- d2.rot[,lapply(.SD,function(x) gsub('\\:|Special value|not available',NA,x)),.SDcols = colnames(d2.rot)]
  # remove rows that have no nuts name
  d2.rot <- d2.rot[!is.na(nuts_name)]
  # remove full duplicates
  d2.rot <- unique(d2.rot)
  # update the EU databases with NUTS code
  d2.rot <- merge(d2.rot,nuts_name_code,by ='nuts_name',all.x = TRUE)
  # remove the rows that have totals on NUTS0 level (countries)
  d2.rot <- d2.rot[nchar(nuts_code)>2]
  # remove data where the total area is missing
  d2.rot <- d2.rot[!is.na(area_arable_rot)]

  #DIVIDES MISSING AREA (NA) EQUALLY OVER THE REST OF THE ROTATION GROUPS

  # replace NA with change between total and sum
  d2.rot.melt <- melt(d2.rot,id.vars = c('nuts_name','nuts_code','area_arable_rot','country_description'),
                      variable.name = 'crop_rot')
  #(one row for each rotation category)
  d2.rot.melt[,value := as.numeric(value)]
  d2.rot.melt[,area_arable_rot := as.numeric(area_arable_rot)]
  #(format numeric)
  d2.rot.melt[,value2 := pmax(0,(area_arable_rot - sum(value,na.rm=T)) / sum(is.na(value))),by = nuts_code]
  #(grouping by nuts2 code, divide total area remaining from filled categories by the number of empty categories)
  d2.rot.melt[is.na(value), value := value2]
  #(assign this to each empty category)
  d2.rot <- dcast(d2.rot.melt,nuts_code + nuts_name + area_arable_rot ~ crop_rot, value.var = 'value')
  #(recast into columns - code/name/areatot should be unique, crop_rot has multiple rows with value assigned in cat)


# --------
# **********************************
# COVER CROP, RESIDUES
# **********************************
# --------
# ???????????????
# what to do with area_rot_na? seems for now to be simply ignored. perhaps do the same for others
# ??????????????????????

  # read soil cover and residues
  d2.cover <- as.data.table(read_xlsx(paste0(floc,'Eurostat_till-cov-rot.xlsx'),sheet='Soil cover and residues',skip=4))
  # change column names
  setnames(d2.cover,c('nuts_name','area_arable_cov','area_cov_excl','area_cov_winter','area_cov_cover','area_cov_per',
                      'area_cov_res', 'area_cov_bare'))
  # (normal winter crops, cover/intermediate crops, multi-annual/perennial cover, residues, bare soil)
  # area_cov_excl = NA area due to permanent cover

  # remove ':' and other special characters and replace with NA
  d2.cover <- d2.cover[,lapply(.SD,function(x) gsub('\\:|Special value|not available',NA,x)),.SDcols = colnames(d2.cover)]
  # convert some columns to numeric, ans set to 0 when NA
  cols <- colnames(d2.cover)[grepl('area_',colnames(d2.cover))]
  d2.cover <- d2.cover[,c(cols) := lapply(.SD,function(x) pmax(0,as.numeric(x),na.rm=TRUE)),.SDcols = cols]
  # remove rows that have no nuts name
  d2.cover <- d2.cover[!is.na(nuts_name)]
  # remove full duplicates
  d2.cover <- unique(d2.cover)
  # update the EU databases with NUTS code
  d2.cover <- merge(d2.cover,nuts_name_code,by ='nuts_name',all.x = TRUE)
  # remove the rows that have totals on NUTS0 level
  d2.cover <- d2.cover[nchar(nuts_code)>2]

  # add totals when input is available
  d2.cover[area_arable_cov == 0, area_arable_cov := area_cov_excl + area_cov_winter + area_cov_cover + area_cov_bare + area_cov_per + area_cov_res]
  #(some rows have individual categories filled but no total area, so replace with that)

  # remove data where the total area is missing
  d2.cover <- d2.cover[area_arable_cov > 0]

#area_cov_excl = arable land "excluding soil cover" - ???
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Agri-environmental_indicator_-_soil_cover#Analysis_at_regional_level
# Soil cover was not reported for all of the arable area. For example, for areas under glass or protective cover


# --------
# **********************************
# TILLAGE
# **********************************
# --------

  # read tillage
  d2.tillage <- as.data.table(read_xlsx(paste0(floc,'Eurostat_till-cov-rot.xlsx'),sheet='Tillage Practices',skip=4))
  # change column names
  setnames(d2.tillage,c('nuts_name','area_arable_till','area_till_excl','area_till_conv','area_till_conserv','area_till_no'))
  # remove ':' and other special characters and replace with NA
  d2.tillage <- d2.tillage[,lapply(.SD,function(x) gsub('\\:|Special value|not available',NA,x)),.SDcols = colnames(d2.tillage)]
  # convert some columns to numeric, and set to 0 when NA
  cols <- colnames(d2.tillage)[grepl('area_',colnames(d2.tillage))]
  d2.tillage <- d2.tillage[,c(cols) := lapply(.SD,function(x) pmax(0,as.numeric(x),na.rm=TRUE)),.SDcols = cols]
  # remove rows that have no nuts name
  d2.tillage <- d2.tillage[!is.na(nuts_name)]
  # remove full duplicates
  d2.tillage <- unique(d2.tillage)
  # update the EU databases with NUTS code
  d2.tillage <- merge(d2.tillage,nuts_name_code,by ='nuts_name',all.x = TRUE)
  # remove the rows that have totals on NUTS0 level
  d2.tillage <- d2.tillage[nchar(nuts_code)>2]

  # add totals when total is missing but individual categories are available
  d2.tillage[area_arable_till == 0,area_arable_till := area_till_excl + area_till_conv + area_till_conserv + area_till_no]

  # remove data where the total area is missing
  d2.tillage <- d2.tillage[area_arable_till > 0]

#area_till_excl = arable land "excluding tillage" - similar to not applicable or not possible?
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Agri-environmental_indicator_-_tillage_practices#Analysis_at_regional_level
# Multi-annual plants like temporary grassland, leguminous plants, industrial crops (hops or aromatic plants)
# etc. were excluded from the survey and covered separately in the category 'not recorded' which covers about
# 10 % of the total arable area.





# -----------------------------------------------------------------------------------------------
# ********************************** LIKELIHOODS CROP ROTATION **********************************
# -----------------------------------------------------------------------------------------------

  # prepare separate likelihood table for all cropping practices (later tillage depends on cropping)
  # grepl function searches for the character strings (using lower case for all) and adds a likelihood col
  lkh.crop <- data.table(crop_name = unique(d1$crop_name))

  #small grain cereals
  lkh.crop[grepl('wheat|bar|rey|cere|oat|rice',tolower(crop_name)), lh.rot := 0.4]
  #maize
  lkh.crop[grepl('maize',tolower(crop_name)), lh.rot := 0.8]
  #legumes
  lkh.crop[grepl('soya|puls',tolower(crop_name)), lh.rot := 0.8]
  #tubers
  lkh.crop[grepl('potat|sugar',tolower(crop_name)), lh.rot := 0.8]
  #temporary industrial crops
  lkh.crop[grepl('rape|cotton|oil|tobac|industrial|sunf',tolower(crop_name)), lh.rot := 0.5]
  #permanent industrial crops
  lkh.crop[grepl('industrial',tolower(crop_name)), lh.rot := 0.01]
  #temporary grassland, permanent grassland, permanent crops, nursery
  lkh.crop[grepl('oliv|orang|wine|fruit|grap|tempor|gras$|vegeta',tolower(crop_name)), lh.rot := 0.01]


# -----------------------------------------------------------------------------------------------
# ********************************** LIKELIHOODS SOIL COVER *************************************
# -----------------------------------------------------------------------------------------------

#  -------winter cover (double cropping)---------------------------

  #small grain cereals
  lkh.crop[grepl('wheat|bar|rey|cere|oat|rice',tolower(crop_name)), lh.win := 0.99]
  #maize
  lkh.crop[grepl('maize',tolower(crop_name)), lh.win := 0.7]
  #legumes
  lkh.crop[grepl('soya|puls',tolower(crop_name)), lh.win := 0.7]
  #tubers
  lkh.crop[grepl('potat|sugar',tolower(crop_name)), lh.win := 0.4]
  #temporary industrial
  lkh.crop[grepl('rape|cotton|oil|tobac|industrial|sunf',tolower(crop_name)), lh.win := 0.8]
  #permanent industrial
  lkh.crop[grepl('industrial',tolower(crop_name)), lh.win := 0.7]
  #temporary grassland, permanent grassland, permanent crops, nursery
  lkh.crop[grepl('oliv|orang|wine|fruit|grap|tempor|gras$|vegeta',tolower(crop_name)), lh.win := 0.01]


# --------cover or intermediate crop ---------------------------

  #small grain cereals
  lkh.crop[grepl('wheat|bar|rey|cere|oat|rice',tolower(crop_name)), lh.cov := 0.99]
  #maize
  lkh.crop[grepl('maize',tolower(crop_name)), lh.cov := 0.8]
  #legumes
  lkh.crop[grepl('soya|puls',tolower(crop_name)), lh.cov := 0.9]
  #tubers
  lkh.crop[grepl('potat|sugar',tolower(crop_name)), lh.cov := 0.5]
  #temporary industrial
  lkh.crop[grepl('rape|cotton|oil|tobac|industrial|sunf',tolower(crop_name)), lh.cov := 0.6]
  #permanent industrial
  lkh.crop[grepl('industrial',tolower(crop_name)), lh.cov := 0.01]
  #temporary grassland, permanent grassland, permanent crops, nursery
  lkh.crop[grepl('oliv|orang|wine|fruit|grap|tempor|gras$|vegeta',tolower(crop_name)), lh.cov := 0.01]


# -------- residues ---------------------------

  #small grain cereals
  lkh.crop[grepl('wheat|bar|rey|cere|oat|rice',tolower(crop_name)), lh.res := 0.99]
  #maize
  lkh.crop[grepl('maize',tolower(crop_name)), lh.res := 0.9]
  #legumes
  lkh.crop[grepl('soya|puls',tolower(crop_name)), lh.res := 0.7]
  #tubers
  lkh.crop[grepl('potat|sugar',tolower(crop_name)), lh.res := 0.7]
  #temporary industrial
  lkh.crop[grepl('rape|cotton|oil|tobac|industrial|sunf',tolower(crop_name)), lh.res := 0.7]
  #permanent industrial
  lkh.crop[grepl('industrial',tolower(crop_name)), lh.res := 0.6]
  #temporary grassland, permanent grassland, permanent crops, nursery
  lkh.crop[grepl('oliv|orang|wine|fruit|grap|tempor|gras$|vegeta',tolower(crop_name)), lh.res := 0.01]


# ---------- perennials --------------------------

  #small grain cereals
  lkh.crop[grepl('wheat|bar|rey|cere|oat|rice',tolower(crop_name)), lh.per := 0.99]
  #maize
  lkh.crop[grepl('maize',tolower(crop_name)), lh.per := 0.01]
  #legumes
  lkh.crop[grepl('soya|puls',tolower(crop_name)), lh.per := 0.01]
  #tubers
  lkh.crop[grepl('potat|sugar',tolower(crop_name)), lh.per := 0.01]
  #temporary industrial
  lkh.crop[grepl('rape|cotton|oil|tobac|industrial|sunf',tolower(crop_name)), lh.per := 0.01]
  #permanent industrial
  lkh.crop[grepl('industrial',tolower(crop_name)), lh.per := 0.01]
  #temporary grassland, permanent grassland, permanent crops, nursery
  lkh.crop[grepl('oliv|orang|wine|fruit|grap|tempor|gras$|vegeta',tolower(crop_name)), lh.per := 0.99]

# -----------bare soil -----------------------------

  #small grain cereals
  lkh.crop[grepl('wheat|bar|rey|cere|oat|rice',tolower(crop_name)), lh.bar := 0.99]
  #maize
  lkh.crop[grepl('maize',tolower(crop_name)), lh.bar := 0.7]
  #legumes
  lkh.crop[grepl('soya|puls',tolower(crop_name)), lh.bar := 0.7]
  #tubers
  lkh.crop[grepl('potat|sugar',tolower(crop_name)), lh.bar := 0.7]
  #temporary industrial
  lkh.crop[grepl('rape|cotton|oil|tobac|industrial|sunf',tolower(crop_name)), lh.bar := 0.7]
  #permanent industrial
  lkh.crop[grepl('industrial',tolower(crop_name)), lh.bar := 0.7]
  #temporary grassland, permanent grassland, permanent crops, nursery
  lkh.crop[grepl('oliv|orang|wine|fruit|grap|tempor|gras$|vegeta',tolower(crop_name)), lh.bar := 0.7]

# ----------- NA (under glass or protective cover) -----------------------------
# exclude this for now??

  #small grain cereals
  lkh.crop[grepl('wheat|bar|rey|cere|oat|rice',tolower(crop_name)), lh.exc := 0.99]
  #maize
  lkh.crop[grepl('maize',tolower(crop_name)), lh.exc := 0.01]
  #legumes
  lkh.crop[grepl('soya|puls',tolower(crop_name)), lh.exc := 0.01]
  #tubers
  lkh.crop[grepl('potat|sugar',tolower(crop_name)), lh.exc := 0.01]
  #temporary industrial
  lkh.crop[grepl('rape|cotton|oil|tobac|industrial|sunf',tolower(crop_name)), lh.exc := 0.01]
  #permanent industrial
  lkh.crop[grepl('industrial',tolower(crop_name)), lh.exc := 0.01]
  #temporary grassland, permanent grassland, permanent crops, nursery
  lkh.crop[grepl('oliv|orang|wine|fruit|grap|tempor|gras$',tolower(crop_name)), lh.exc := 0.01]
  lkh.crop[grepl('vegeta',tolower(crop_name)), lh.exc := 0.5]

  # prepare table likelihood for conventional tillage
  lkh.till <- data.table(crop_name = unique(d1$crop_name))
  lkh.till[grepl('wheat|bar|rey|cere',tolower(crop_name)), lh := 0.8]
  lkh.till[grepl('potat|maize|soya|sugar|vegeta|oat|industrial',tolower(crop_name)), lh := 1.0]
  lkh.till[grepl('puls|rice|sunf',tolower(crop_name)), lh := 0.4]
  lkh.till[grepl('oliv|orang|wine|fruit|oil|tobac|grap',tolower(crop_name)), lh := 0]
  lkh.till[grepl('grassland|gras$',tolower(crop_name)), lh := 0]
  lkh.till[grepl('rape|cotton',tolower(crop_name)), lh := 0.2]


