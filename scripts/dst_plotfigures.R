

#GENERATING TIFF AND OUTPUT MAPS


library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
require(data.table)

# install packages once for those not used before
# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("terra")

#================================================
# load spatial data and create raster for outputs
#================================================

# set theme
theme_set(theme_bw())

# get the raster to plot
r1 <- terra::rast('products/gncu2010_ext.asc')

# convert to data.frame
# adding na.rm=F solves the following error, even though the map not affected
# Error in x$.self$finalize() : attempt to apply non-function
r1.p <- as.data.frame(r1,xy=TRUE,na.rm=FALSE)
r1.p <- as.data.table(r1.p)


#================================================
# IMPACT_BEST
#================================================

r.ncu <- merge(r1.p, out.best, by.x = 'gncu2010_ext', by.y = 'ncu')

# make man codes numeric
### try later ###
# # better is to replace this part of code by r.ncu[, man_num := as.numeric(as.factor(man_code))]
r.ncu[man_code == 'CF-MF' , man_num := 1]
r.ncu[man_code == 'OF-MF' , man_num := 2]
r.ncu[man_code == 'EE' , man_num := 3]
r.ncu[man_code == 'RFR' , man_num := 4]
r.ncu[man_code == 'RFT' , man_num := 5]
r.ncu[man_code == 'RFP' , man_num := 6]

# set columns in right order for conversion to raster
 setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','man_num','dY','dist_Y','dSOC','dist_C','dNsu','dist_N','man_code'))

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'products/out.best.tif', overwrite = TRUE)


#================================================
# SCORE_SINGLE
#================================================

r.ncu <- merge(r1.p, out.single, by.x = 'gncu2010_ext', by.y = 'ncu')

r.ncu$`1` <- factor(r.ncu$`1`, levels=c('CF-MF','OF-MF','EE','RFR','RFT','RFP'))
r.ncu$`2` <- factor(r.ncu$`2`, levels=c('CF-MF','OF-MF','EE','RFR','RFT','RFP'))
r.ncu$`3` <- factor(r.ncu$`3`, levels=c('CF-MF','OF-MF','EE','RFR','RFT','RFP'))
r.ncu$`4` <- factor(r.ncu$`4`, levels=c('CF-MF','OF-MF','EE','RFR','RFT','RFP'))
r.ncu$`5` <- factor(r.ncu$`5`, levels=c('CF-MF','OF-MF','EE','RFR','RFT','RFP'))
r.ncu$`6` <- factor(r.ncu$`6`, levels=c('CF-MF','OF-MF','EE','RFR','RFT','RFP'))
r.ncu[, R1 := as.numeric(r.ncu$`1`)]
r.ncu[, R2 := as.numeric(r.ncu$`2`)]
r.ncu[, R3 := as.numeric(r.ncu$`3`)]
r.ncu[, R4 := as.numeric(r.ncu$`4`)]
r.ncu[, R5 := as.numeric(r.ncu$`5`)]
r.ncu[, R6 := as.numeric(r.ncu$`6`)]

# concatenate into single column for raster symbology
r.ncu$concat <- as.numeric(paste0(r.ncu$R1, r.ncu$R2, r.ncu$R3, r.ncu$R4, r.ncu$R5, r.ncu$R6))
table(r.ncu$concat)

# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','concat','R1','R2','R3','R4','R5','R6','dist_Y','dist_C','dist_N',
                     '1', '2', '3', '4','5','6'))

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'products/out.single.tif', overwrite = TRUE)


#================================================
# SCORE DUO
#================================================

#impact_best
r.ncu <- merge(r1.p, out.duo.2, by.x = 'gncu2010_ext', by.y = 'ncu')

#make man codes numeric for raster
ord <- c('EE-RFR','CF-MF-EE','EE-RFT','CF-MF-RFT','CF-MF-RFR','CF-MF-OF-MF','CF-MF-RFP','EE-OF-MF',
   'OF-MF-RFP','OF-MF-RFT','RFP-RFT','RFR-RFT','OF-MF-RFR','RFP-RFR')

r.ncu$`1` <- as.numeric(factor(r.ncu$`1`, levels=ord))
r.ncu$`2` <- as.numeric(factor(r.ncu$`2`, levels=ord))
r.ncu$`3` <- as.numeric(factor(r.ncu$`3`, levels=ord))
r.ncu$`4` <- as.numeric(factor(r.ncu$`4`, levels=ord))
r.ncu$`5` <- as.numeric(factor(r.ncu$`5`, levels=ord))
r.ncu$`6` <- as.numeric(factor(r.ncu$`6`, levels=ord))
r.ncu$`7` <- as.numeric(factor(r.ncu$`7`, levels=ord))
r.ncu$`8` <- as.numeric(factor(r.ncu$`8`, levels=ord))
r.ncu$`9` <- as.numeric(factor(r.ncu$`9`, levels=ord))
r.ncu$`10` <- as.numeric(factor(r.ncu$`10`, levels=ord))
r.ncu$`11` <- as.numeric(factor(r.ncu$`11`, levels=ord))
r.ncu$`12` <- as.numeric(factor(r.ncu$`12`, levels=ord))
r.ncu$`13` <- as.numeric(factor(r.ncu$`13`, levels=ord))
r.ncu$`14` <- as.numeric(factor(r.ncu$`14`, levels=ord))
r.ncu$`15` <- as.numeric(factor(r.ncu$`15`, levels=ord))


# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','1','2','3','4','5','6','7','8',
                     '9','10','11','12','13','14','15'))

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'products/out.duo.2.tif', overwrite = TRUE)






#================================================
# SCORE TRIO
#================================================

#impact_best
r.ncu <- merge(r1.p, out.trio, by.x = 'gncu2010_ext', by.y = 'ncu')

#make man codes numeric for raster
ord <- c('EE-RFR','CF-MF-EE','EE-RFT','CF-MF-RFT','CF-MF-RFR','CF-MF-OF-MF','CF-MF-RFP','EE-OF-MF',
         'OF-MF-RFP','OF-MF-RFT','RFP-RFT','RFR-RFT','OF-MF-RFR','RFP-RFR')

r.ncu$`1` <- as.numeric(factor(r.ncu$`1`, levels=ord))
r.ncu$`2` <- as.numeric(factor(r.ncu$`2`, levels=ord))
r.ncu$`3` <- as.numeric(factor(r.ncu$`3`, levels=ord))
r.ncu$`4` <- as.numeric(factor(r.ncu$`4`, levels=ord))
r.ncu$`5` <- as.numeric(factor(r.ncu$`5`, levels=ord))
r.ncu$`6` <- as.numeric(factor(r.ncu$`6`, levels=ord))
r.ncu$`7` <- as.numeric(factor(r.ncu$`7`, levels=ord))
r.ncu$`8` <- as.numeric(factor(r.ncu$`8`, levels=ord))
r.ncu$`9` <- as.numeric(factor(r.ncu$`9`, levels=ord))
r.ncu$`10` <- as.numeric(factor(r.ncu$`10`, levels=ord))
r.ncu$`11` <- as.numeric(factor(r.ncu$`11`, levels=ord))
r.ncu$`12` <- as.numeric(factor(r.ncu$`12`, levels=ord))
r.ncu$`13` <- as.numeric(factor(r.ncu$`13`, levels=ord))
r.ncu$`14` <- as.numeric(factor(r.ncu$`14`, levels=ord))
r.ncu$`15` <- as.numeric(factor(r.ncu$`15`, levels=ord))


# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','1','2','3','4','5','6','7','8',
                     '9','10','11','12','13','14','15'))

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'products/out.duo.2.tif', overwrite = TRUE)















# TEST 1 where missing models are NA
# - saved as output 3a NA model

# =========== OPTION 1 - ranking of 6 measures ===========
#
# missing models replaced with 0.0001
# (many categories ~400)
# ========================================================

# join/merge output.3a  with r1.p
r.ncu <- merge(r1.p, output.3a, by.x = 'gncu2010_ext', by.y = 'ncu')
# concatenate into single column for raster symbology
r.ncu$concat <- as.numeric(paste0(r.ncu$`CF-MF`, r.ncu$`OF-MF`, r.ncu$EE, r.ncu$RFR, r.ncu$RFT, r.ncu$RFP))
# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext', 'concat', 'CF-MF', 'OF-MF', 'EE', 'RFR', 'RFT', 'RFP'))
# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'products/output.3a2.tif', overwrite = TRUE)


# =========== OPTION 2 - one best measure ================
#
# missing models replaced with 0.0001
# (6 categories)
# ========================================================

# join/merge output.2a  with r1.p
r.ncu <- merge(r1.p, output1, by.x = 'gncu2010_ext', by.y = 'ncu')

# make man codes numeric
### try later ###
# better is to replace this part of code by r.ncu[, man_num := as.numeric(as.factor(man_code))]
r.ncu[man_code == 'CF-MF' , man_num := 1]
r.ncu[man_code == 'OF-MF' , man_num := 2]
r.ncu[man_code == 'EE' , man_num := 3]
r.ncu[man_code == 'RFR' , man_num := 4]
r.ncu[man_code == 'RFT' , man_num := 5]
r.ncu[man_code == 'RFP' , man_num := 6]

# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','man_num','dY','dist_Y','dSOC','dist_C','dNsu','dist_N','man_code'))

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
#warning messages above showed up here

terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'products/output1_test.tif', overwrite = TRUE)
