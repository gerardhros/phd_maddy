require(terra)
require(data.table)

simX <- readRDS('D:/ESA/02 phd projects/01 maddy young/01 data/simx.rds')
output2 <- simX$impact_best

r1 <- terra::rast('D:/ESA/02 phd projects/01 maddy young/01 data/gncu2010_ext.asc')
r1.p <- as.data.frame(r1,xy=TRUE)
r1.p <- as.data.table(r1.p)

# join/merge output.2a  with r1.p
r.ncu <- merge(r1.p, output2, by.x = 'gncu2010_ext', by.y = 'ncu')

#make man codes numeric
r.ncu[man_code == 'CF-MF' , man_num := 1]
r.ncu[man_code == 'OF-MF' , man_num := 2]
r.ncu[man_code == 'EE' , man_num := 3]
r.ncu[man_code == 'RFR' , man_num := 4]
r.ncu[man_code == 'RFT' , man_num := 5]
r.ncu[man_code == 'RFP' , man_num := 6]

r.ncu[, man_num := as.numeric(as.factor(man_code))]

r.ncu <- r.ncu[,.(x,y,man_num)]

r.fin <- terra::rast(r.ncu,type='xyz')
terra::crs(r.fin) <- 'epsg:4326'
# # write as output
terra::writeRaster(r.fin,'D:/ESA/02 phd projects/01 maddy young/01 data/output.test.tif', overwrite = TRUE)
