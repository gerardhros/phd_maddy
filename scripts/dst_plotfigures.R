

#GENERATING TIFF AND OUTPUT MAPS
setwd('C:/phd_maddy/')

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
r1.p <- as.data.frame(r1,xy=TRUE)
r1.p <- as.data.table(r1.p)

#===============================================================================
# IMPACT_BEST map for QGIS inspection
#===============================================================================

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

#===============================================================================
# link raster to different output sets
#===============================================================================

# with different input files
#   out.best - values at t=5, etc.
#   d1.fact.cont - values at t=0 and site properties from Integrator

r.ncu <- merge(r1.p, d1.Yref.tar, by.x = 'gncu2010_ext', by.y = 'ncu')

# set columns in right order for conversion to raster
#setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','dist_Y.x','dist_C.x','dist_N.x','dist_Y_fin','dist_C_fin','dist_N_fin'))
# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','yield_ref_w','yield_targ_w','dist_Y','diff_Y'))

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
#terra::writeRaster(r.fin,'products/yield_ref_targ.tif', overwrite = TRUE)


r.ncu <- merge(r1.p, d1.targ, by.x = 'gncu2010_ext', by.y = 'ncu')

# set columns in right order for conversion to raster
#setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','dist_Y.x','dist_C.x','dist_N.x','dist_Y_fin','dist_C_fin','dist_N_fin'))
# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','soc_target','n_sp_crit','n_sp_sw_crit','n_sp_gw_crit'))

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
#terra::writeRaster(r.fin,'products/yield_ref_targ.tif', overwrite = TRUE)



#===============================================================================
# function from Gerard to make a map by each band/output
#===============================================================================

# visualisation function of a raster file (its a global plot, so it still uses
# object world2 as starting point (similar as your first plot function for EU map)
# the argument ftitle is a string with the title that you want to plot on top of the figure

visualize <- function(raster, layer, name, breaks, labels, ftitle){
  # select the correct layer
  raster.int <- raster[layer]
  # define crs
  plotcrs <- coord_sf(crs = 4326, lims_method = "box")
  #raster to xy
  df <- as.data.frame(raster.int, xy = TRUE)
  #colnames
  colnames(df) <- c("x", "y", "variable")
  #plot
  ggplot() +
    geom_tile(data = df, aes(x = x, y = y,fill = cut(variable, breaks,labels = labels))) +
    plotcrs +
    scale_fill_viridis_d(direction=-1) +
    xlab("") + ylab("")+
    #xlab("Longitude") + ylab("Latitude") +
    labs(fill = name) +
    theme(text = element_text(size = 18),
          legend.text=element_text(size=12),
          legend.position = c(0.1,0.8),
          legend.background = element_rect(fill = "white",color='white'),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(ftitle)
}

#===============================================================================
# yield REFERENCE (timestep 0)
#===============================================================================
# labels and breaks: breaks is a vector showing the cut between labels,
# labels is a vector with strings how to describe the classes
# name is a string being the text above the legend

p1 <- visualize(raster = r.fin,
                layer = 'diff_Y',
                name = "Ref value",
                breaks = c(0,2500,5000,7500,10000,12500,15000,17500,20000,22500,25000,27500,30000,32500,35000,37500,55000),
                labels = c('<2500','2500-5000','5000-7500','7500-10000','10000-12500','12500-15000','15000-17500','17500-20000','20000-22500',
                           '22500-25000','25000-27500','27500-30000','30000-32500','32500-35000','35000-37500','37500-50000'),
                ftitle = 'Yield (kg ha-1)')
ggsave(filename = "products/diff_Y.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)

#===============================================================================
# soc REFERENCE (timestep 0)
#===============================================================================

p1 <- visualize(raster = r.fin,
                layer = 'soc_ref',
                name = "Ref value",
                breaks = c(0,0.5,1,1.5,3,5,10,20,30,50),
                labels = c('< 0.5','0.5 - 1','1 - 1.5','1.5 - 3','3 - 5','5 - 10','10 - 20','20 - 30','30 - 50'),
                ftitle = 'SOC (%)')
ggsave(filename = "products/soc_ref.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)

#===============================================================================
# Nsu REFERENCE (timestep 0)
#===============================================================================

p1 <- visualize(raster = r.fin,
                layer = 'n_sp_ref',
                name = "Ref value",
                breaks = c(-300,25,50,75,100,150,300),
                labels = c('< 25','25 - 50','50 - 75','75 - 100','100 - 150','150 - 200'),
                ftitle = 'N surplus (kg ha-1)')
ggsave(filename = "products/n_sp_ref.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)

#===============================================================================
# yield DISTANCE (INITIAL TIMESTEP 0)
#===============================================================================

p1 <- visualize(raster = r.fin,
                layer = 'diff_Y',
                name = "Ref/Target",
                breaks = c(0,1,1.25,1.5,1.75,2,2.5,3,4),
                labels = c('< 1','1 - 1.25','1.25 - 1.5','1.5 - 1.75','1.75 - 2','2 - 2.5','2.5 - 3','3 - 4'),
                ftitle = 'Yield distance to target (ratio)')
ggsave(filename = "products/diff_Y_ref_w.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)

#===============================================================================
# yield TARGET (timestep 0)
#===============================================================================

p1 <- visualize(raster = r.fin,
                layer = 'yield_targ_w',
                name = "Target value",
                breaks = c(0,2500,5000,7500,10000,12500,15000,17500,20000,22500,25000,27500,30000,32500,35000,37500,55000),
                labels = c('<2500','2500-5000','5000-7500','7500-10000','10000-12500','12500-15000','15000-17500','17500-20000','20000-22500',
                           '22500-25000','25000-27500','27500-30000','30000-32500','32500-35000','35000-37500','37500-50000'),
                ftitle = 'Target yield (kg ha-1)')
ggsave(filename = "products/targ_Y.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)

#===============================================================================
# soc TARGET (INITIAL TIMESTEP 0)
#===============================================================================

p1 <- visualize(raster = r.fin,
                layer = 'soc_target',
                name = "Target value",
                breaks = c(0,1,1.1,1.2,1.3,1.4,1.5),
                labels = c('1','1 - 1.1','1.1 - 1.2','1.2 - 1.3','1.3 - 1.4','1.4 - 1.5'),
                ftitle = 'Target SOC (%)')
ggsave(filename = "products/targ_C.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)

#===============================================================================
# Nsu TARGET (timestep 0)
#===============================================================================

p1 <- visualize(raster = r.fin,
                layer = 'n_sp_crit',
                name = "Critical limit",
                breaks = c(0,25,50,75,100,150,10000),
                labels = c('< 25','25 - 50','50 - 75','75 - 100','100 - 150','> 150'),
                ftitle = 'Critical N surplus (kg ha-1)')
ggsave(filename = "products/targ_N.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)

#===============================================================================
# soc DISTANCE (INITIAL TIMESTEP 0)
#===============================================================================

p1 <- visualize(raster = r.fin,
                layer = 'dist_C.x',
                name = "Ref/Target",
                breaks = c(0,0.5,1,1.5,3,5,10,20,30,40),
                labels = c('< 0.5','0.5 - 1','1 - 1.5','1.5 - 3','3 - 5','5 - 10','10 - 20','20 - 30','30 - 40'),
                ftitle = 'SOC distance to target (ratio)')
ggsave(filename = "products/dist_C_ref.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)


#===============================================================================
# Nsu distance (INITIAL TIMESTEP 0)
#===============================================================================

p1 <- visualize(raster = r.fin,
                layer = 'dist_N.x',
                name = "Ref/Target",
                breaks = c(-2,0.5,1,2.5,5,10,25),
                labels = c('< 0.5','0.5 - 1','1 - 2.5','2.5 - 5','5 - 10','10 - 25'),
                ftitle = 'N surplus distance to target (ratio)')
ggsave(filename = "products/dist_N_ref.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)



#===============================================================================
# soc distance (FINAL TIMESTEP 5 YEARS)
#===============================================================================

p1 <- visualize(raster = r.fin,
                layer = 'dist_C',
                name = "Ref/Target",
                breaks = c(0,0.5,1,1.5,3,5,10,20,30,40),
                labels = c('< 0.5','0.5 - 1','1 - 1.5','1.5 - 3','3 - 5','5 - 10','10 - 20','20 - 30','30 - 40'),
                ftitle = 'SOC distance to target (ratio)')
ggsave(filename = "products/dist_SOC.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)

#===============================================================================
# Nsu distance (FINAL TIMESTEP 5 YEARS)
#===============================================================================

p1 <- visualize(raster = r.fin,
                layer = 'dist_N',
                name = "Ref/Target",
                breaks = c(-2,0.5,1,2.5,5,10,25),
                labels = c('< 0.5','0.5 - 1','1 - 2.5','2.5 - 5','5 - 10','10 - 25'),
                ftitle = 'N surplus distance to target (ratio)')
ggsave(filename = "products/dist_N.png",
       plot = p1, width = 25, height = 25, units = c("cm"), dpi = 1200)

#===============================================================================
# SCORE_SINGLE
#===============================================================================

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


#===============================================================================
# SCORE DUO
#===============================================================================

#impact_best
r.ncu <- merge(r1.p, out.duo, by.x = 'gncu2010_ext', by.y = 'ncu')

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
terra::writeRaster(r.fin,'products/out.duo.tif', overwrite = TRUE)



#===============================================================================
# SCORE TRIO
#===============================================================================

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
terra::writeRaster(r.fin,'products/out.trio.tif', overwrite = TRUE)



#===============================================================================
# Reference values
#===============================================================================

r.ncu <- merge(r1.p, ref.values, by.x = 'gncu2010_ext', by.y = 'ncu')

# set columns in right order for conversion to raster
setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext','yield_ref','soc_ref','n_sp_ref','yield_target','soc_target'))

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# write as output
terra::writeRaster(r.fin,'products/ref.values.tif', overwrite = TRUE)












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
