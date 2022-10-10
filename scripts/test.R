require(terra)
require(data.table)

simX <- readRDS('D:/ESA/02 phd projects/01 maddy young/01 data/simx.rds')
output1 <- simX$impact_best

simX <- readRDS('D:/ESA/02 phd projects/01 maddy young/01 data/sim2.rds')
output2 <- simX$impact_best

output3 <- output1[!ncu %in% output2$ncu]

r1 <- terra::rast('D:/ESA/02 phd projects/01 maddy young/01 data/gncu2010_ext.asc')
r1.p <- as.data.frame(r1,xy=TRUE)
r1.p <- as.data.table(r1.p)

# join/merge output.2a  with r1.p
r.ncu <- merge(r1.p, output3, by.x = 'gncu2010_ext', by.y = 'ncu')

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
terra::writeRaster(r.fin,'D:/ESA/02 phd projects/01 maddy young/01 data/output.test3.tif', overwrite = TRUE)


# make example for Nsurplus
test <- CJ(dNsu = seq(-1,1.8,0.2),n_sp_ref = c(20,40,60),n_sp_crit = 25)
test[,n_sp_new := (1 + dNsu) * n_sp_ref]
test[,n_sp_delta := dNsu  * n_sp_ref]
test[, sNsu := pmax(0,(1 + dNsu) * n_sp_ref / n_sp_crit - 1)]

plot(sNsu~n_sp_new,data= test[n_sp_ref==20],col='blue',xlim=c(0,100),type='l')
points(sNsu~n_sp_new,data= test[n_sp_ref==20],col='blue',pch=16)
points(sNsu~n_sp_new,data= test[n_sp_ref==40],col='red',pch=16)
points(sNsu~n_sp_new,data= test[n_sp_ref==60],col='black',pch=16)
abline(v = 25,lty=2)


# make example for Nsurplus
test <- CJ(dSOC = seq(-1,1.8,0.2),soc_ref = c(0.8,1.2,1.6),soc_target = 1.25)
test[,soc_new := (1 + dSOC) * soc_ref]
test[, sSOC := 1 - pmin(1,((1 + dSOC) * soc_ref) / soc_target)]


plot(sSOC~soc_new,data= test[soc_ref==0.8],col='blue',type='l')
points(sSOC~soc_new,data= test[soc_ref==0.8],col='blue',pch=16)
points(sSOC~soc_new,data= test[soc_ref==1.2],col='red',pch=16)
points(sSOC~soc_new,data= test[soc_ref==1.6],col='black',pch=16)
abline(v = 1.25,lty=2)
