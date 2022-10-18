# plotting

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
# get spatial raster data
#================================================

# set theme
theme_set(theme_bw())

# get the raster to plot
r1 <- terra::rast('products/gncu2010_ext.asc')

# convert to data.frame
r1.p <- as.data.frame(r1,xy=TRUE)
r1.p <- as.data.table(r1.p)


#================================================
# link raster to map different band outputs
#================================================

# join/merge outputs with r1.p
r.ncu <- merge(r1.p, out.best, by.x = 'gncu2010_ext', by.y = 'ncu')

# convert to spatial raster
r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'

# write as output
terra::writeRaster(r.fin,'products/out.best.tif', overwrite = TRUE)
r1 <- terra::rast('C:/phd_maddy/products/out.best.tif')
r1.p <- as.data.frame(r1,xy=TRUE)
#================================================
# d yield
#================================================
p1 = ggplot() +
  geom_raster(data = r1.p,aes(x=x,y=y,fill= dY)) + #changing fill= whatever col to plot
  scale_fill_viridis_c(direction=-1)+
  # theme_void() + #automatic continuous color legend (can check others, e.g. for categorical)
  theme(legend.position = 'bottom')+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Yield", subtitle = "Change due to best measure (%)") +
  coord_sf(crs = 4326) + theme_bw()
p1
ggsave(plot = p1, filename = 'C:/phd_maddy/products/dY.jpg')


#================================================
# d soc
#================================================
p2 = ggplot() +
  geom_raster(data = r1.p,aes(x=x,y=y,fill= dSOC)) + #changing fill= whatever col to plot
  scale_fill_viridis_c(direction=-1)+
  # theme_void() + #automatic continuous color legend (can check others, e.g. for categorical)
  theme(legend.position = 'bottom')+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SOC", subtitle = "Change due to best measure (%)") +
  coord_sf(crs = 4326) + theme_bw()
p2
ggsave(plot = p2, filename = 'C:/phd_maddy/products/dSOC.jpg')


#================================================
# Nsu distance
#================================================
p3 = ggplot() +
  geom_raster(data = r1.p,aes(x=x,y=y,fill= dNsu)) + #changing fill= whatever col to plot
  scale_fill_viridis_c(direction=-1)+
  # theme_void() + #automatic continuous color legend (can check others, e.g. for categorical)
  theme(legend.position = 'bottom')+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("N surplus", subtitle = "Change due to best measure (%)") +
  coord_sf(crs = 4326) + theme_bw()
p3
ggsave(plot = p3, filename = 'C:/phd_maddy/products/dNsu.jpg')

#================================================
# yield distance
#================================================
p1 = ggplot() +
  geom_raster(data = r1.p,aes(x=x,y=y,fill= dist_Y)) + #changing fill= whatever col to plot
  scale_fill_viridis_c(direction=-1)+
  # theme_void() + #automatic continuous color legend (can check others, e.g. for categorical)
  theme(legend.position = 'bottom')+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Yield", subtitle = "Distance to target (ref/target)") +
  coord_sf(crs = 4326) + theme_bw()
p1
ggsave(plot = p1, filename = 'C:/phd_maddy/products/yield_dist.jpg')


#================================================
# soc distance
#================================================
p2 = ggplot() +
  geom_raster(data = r1.p,aes(x=x,y=y,fill= dist_C)) + #changing fill= whatever col to plot
  scale_fill_viridis_c(direction=-1)+
  # theme_void() + #automatic continuous color legend (can check others, e.g. for categorical)
  theme(legend.position = 'bottom')+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SOC", subtitle = "Distance to target (ref/target)") +
  coord_sf(crs = 4326) + theme_bw()
p2
ggsave(plot = p2, filename = 'C:/phd_maddy/products/soc_dist.jpg')


#================================================
# Nsu distance
#================================================
p3 = ggplot() +
  geom_raster(data = r1.p,aes(x=x,y=y,fill= dist_N)) + #changing fill= whatever col to plot
  scale_fill_viridis_c(direction=-1)+
  # theme_void() + #automatic continuous color legend (can check others, e.g. for categorical)
  theme(legend.position = 'bottom')+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("N surplus", subtitle = "Distance to target (ref/target)") +
  coord_sf(crs = 4326) + theme_bw()
p3
ggsave(plot = p3, filename = 'C:/phd_maddy/products/nsu_dist.jpg')


#================================================
# yield ref
#================================================

p1 = ggplot() +
  geom_raster(data = r1.p,aes(x=x,y=y,fill= yield_ref)) + #changing fill= whatever col to plot
  scale_fill_viridis_c(direction=-1)+
  # theme_void() + #automatic continuous color legend (can check others, e.g. for categorical)
  theme(legend.position = 'bottom')+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Yield", subtitle = "Reference value (kg)") +
  coord_sf(crs = 4326) + theme_bw()
p1
ggsave(plot = p1, filename = 'C:/phd_maddy/products/yield_ref.jpg')


#================================================
# soc ref
#================================================

p2 = ggplot() +
  geom_raster(data = r1.p,aes(x=x,y=y,fill= soc_ref)) + #changing fill= whatever col to plot
  scale_fill_viridis_c(direction=-1)+
  # theme_void() + #automatic continuous color legend (can check others, e.g. for categorical)
  theme(legend.position = 'bottom')+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SOC", subtitle = "Reference value (%)") +
  coord_sf(crs = 4326) + theme_bw()
p2
ggsave(plot = p2, filename = 'C:/phd_maddy/products/soc_ref.jpg')


#================================================
# soc ref
#================================================

p3 = ggplot() +
  geom_raster(data = r1.p,aes(x=x,y=y,fill= n_sp_ref)) + #changing fill= whatever col to plot
  scale_fill_viridis_c(direction=-1)+
  # theme_void() + #automatic continuous color legend (can check others, e.g. for categorical)
  theme(legend.position = 'bottom')+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("N surplus", subtitle = "Reference value (kg)") +
  coord_sf(crs = 4326) + theme_bw()
p3
ggsave(plot = p3, filename = 'C:/phd_maddy/products/nsu_ref.jpg')




# get base world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# plot a basic world map plot
p1 <- ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    # geom_tile(data = r1.p,aes(x=x,y=y,fill= improvement)) +
    # scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean change for scenario 1") +
    coord_sf(crs = 4326)
ggsave(plot = p1, filename = 'products/nue_effect_s1.jpg')


# plot source data to understand spatial patterns

  # get the raster to plot crops
  r1 <- terra::rast('C:/phd_maddy/products/ref.values.tif')
  r1.p <- as.data.frame(r1,xy=TRUE)
  # r1.p$total = (r1.p$RICE + r1.p$MAIZ + r1.p$other + r1.p$wheat)/1000
  # ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
  p1 = ggplot() +
  geom_raster(data = r1.p,aes(x=x,y=y,fill= dY)) + #changing fill= whatever col to plot
        scale_fill_viridis_c()+
    # theme_void() + #automatic continuous color legend (can check others, e.g. for categorical)
        theme(legend.position = 'bottom')+
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean change for scenario 1") +
    coord_sf(crs = 4326) + theme_bw()

  ggsave(plot = p1, filename = 'C:/phd_maddy/products/output.2a2.jpg')


  # get the raster to plot MAT
  r1 <- terra::rast('data/climate.tif')
  r1.p <- as.data.frame(r1,xy=TRUE)
  ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
        geom_raster(data = r1.p,aes(x=x,y=y,fill= mat)) +
        scale_fill_viridis_c()+ theme_void() +
        theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean Anntual Temperature") +
    coord_sf(crs = 4326)
  ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_raster(data = r1.p,aes(x=x,y=y,fill= pre*0.1)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean Annual Precipitation") +
    coord_sf(crs = 4326)

  # get the raster to plot soil properties
  r1 <- terra::rast('data/soil.tif')
  r1.p <- as.data.frame(r1,xy=TRUE)
  colnames(r1.p) <- c('x','y','ph','clay','soc')
  r1.p <- r1.p[r1.p$ph>0,]

  ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_raster(data = r1.p,aes(x=x,y=y,fill= ph)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean pH isric") +
    coord_sf(crs = 4326)
  ggplot(data = world) + geom_sf(color = "black", fill = "gray92") +
    geom_raster(data = r1.p,aes(x=x,y=y,fill= soc)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "Mean SOC isric") +
    coord_sf(crs = 4326)

  # get the raster to plot tillage properties TEST
  # SOETHING GOES WRONG HERE
  # SEE DIFFERENCE BETWEEN DEFAULT PLOT AND THE GGPLOT

  r1 <- terra::rast('data/tillage.tif')
  r1.p <- as.data.frame(r1,xy=TRUE,na.rm=F)
  r1.p <- as.data.table(r1.p)
  r1.p <- r1.p[!(is.na(RICE) & is.na(MAIZ) & is.na(other) & is.na(wheat))]
  r1.p[,total := pmax(0,RICE,na.rm=T) + pmax(MAIZ,0,na.rm=T) +
         pmax(other,0,na.rm=T) + pmax(wheat,0,na.rm=T)]
  ggplot(data = world) + geom_sf(color = "black", fill = "gray98") +
    geom_tile(data = r1.p,aes(x=x,y=y,fill= total)) +
    scale_fill_viridis_c()+ theme_void() +
    theme(legend.position = 'bottom') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("World map", subtitle = "All tillage practices") +
    coord_sf(crs = 4326)

