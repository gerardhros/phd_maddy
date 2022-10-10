

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

r.ncu <- merge(r1.p, output7, by.x = 'gncu2010_ext', by.y = 'ncu')

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
terra::writeRaster(r.fin,'products/output7single.tif', overwrite = TRUE)


#================================================
# SCORE_SINGLE 7a
#================================================

r.ncu <- merge(r1.p, output7, by.x = 'gncu2010_ext', by.y = 'ncu')
# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','CF-MF','OF-MF','EE','RFR','RFT','RFP','dist_Y','dist_C','dist_N'))
# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'C:/dst_outputs/output7.tif', overwrite = TRUE)

#================================================
# SCORE_SINGLE 7b
#================================================

r.ncu <- merge(r1.p, output7, by.x = 'gncu2010_ext', by.y = 'ncu')
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
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','CF-MF','OF-MF','EE','RFR','RFT','RFP','dist_Y','dist_C','dist_N'))
# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'C:/dst_outputs/output7.tif', overwrite = TRUE)





# SCORE DUO

#impact_best
r.ncu <- merge(r1.p, output9mini, by.x = 'gncu2010_ext', by.y = 'ncu')

# make man codes numeric
### try later ###
#better is to replace this part of code by
r.ncu[, man_num1 := as.numeric(as.factor(R1))]


# r.ncu[man_code == 'CF-MF' , man_num := 1]
# r.ncu[man_code == 'OF-MF' , man_num := 2]
# r.ncu[man_code == 'EE' , man_num := 3]
# r.ncu[man_code == 'RFR' , man_num := 4]
# r.ncu[man_code == 'RFT' , man_num := 5]
# r.ncu[man_code == 'RFP' , man_num := 6]

# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','man_num1','R1','R2','R3'))

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'products/output9mini.tif', overwrite = TRUE)



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
