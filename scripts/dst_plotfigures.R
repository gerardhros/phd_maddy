# make spatial figure

#  this is a simple test


#LATER FOR GENERATING TIFF AND OUTPUT MAPS


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

# set theme
theme_set(theme_bw())

# get the raster to plot
r1 <- terra::rast('products/gncu2010_ext.asc')

# convert to data.frame
r1.p <- as.data.frame(r1,xy=TRUE)
r1.p <- as.data.table(r1.p)

# TEST 1 where missing models are NA
# - saved as output 3a NA model

# =========== OPTION 1 - ranking of 6 measures ===========
#
# missing models replaced with 0.0001
# (many categories ~400)
# ========================================================

# # join/merge output.3a  with r1.p
# r.ncu <- merge(r1.p, output.3a, by.x = 'gncu2010_ext', by.y = 'ncu')
# # concatenate into single column for raster symbology
# r.ncu$concat <- as.numeric(paste0(r.ncu$`CF-MF`, r.ncu$`OF-MF`, r.ncu$EE, r.ncu$RFR, r.ncu$RFT, r.ncu$RFP))
# # set columns in right order for conversion to raster
# setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext', 'concat', 'CF-MF', 'OF-MF', 'EE', 'RFR', 'RFT', 'RFP'))
# # convert to spatial raster
# r.fin <- terra::rast(r.ncu,type='xyz')
# terra::crs(r.fin) <- 'epsg:4326'
# # write as output
# terra::writeRaster(r.fin,'products/output.3a2.tif', overwrite = TRUE)


# =========== OPTION 2 - one best measure ================
#
# missing models replaced with 0.0001
# (6 categories)
# ========================================================

# join/merge output.2a  with r1.p
r.ncu <- merge(r1.p, output.2a, by.x = 'gncu2010_ext', by.y = 'ncu')

#make man codes numeric
r.ncu[man_code == 'CF-MF' , man_num := 1]
r.ncu[man_code == 'OF-MF' , man_num := 2]
r.ncu[man_code == 'EE' , man_num := 3]
r.ncu[man_code == 'RFR' , man_num := 4]
r.ncu[man_code == 'RFT' , man_num := 5]
r.ncu[man_code == 'RFP' , man_num := 6]

# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext', 'man_num' , 'dY', 'dSOC', 'dNsu', 'man_code'))

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'products/output.2a.tif', overwrite = TRUE)