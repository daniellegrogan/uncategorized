# WBM processing for GAO GLEAM project, v1
# Danielle S Grogan
# 2019-11-05

##################################################################################################################
### LIBRARIES AND SOURCE FILES ###

library(raster)
library(rgdal)
library(rgeos)
library(rasterVis)
source("~/git_repos/WBMr/wbm_load.R")
source("~/git_repos/WBMr/spatial_aggregation.R")
##################################################################################################################
### FILE PATHS ###

path = "/net/nfs/squam/raid/data/GLEAM/WBM/GLEAM_v1/GeoTIFF/yearly_clim"
path.figures = "/net/nfs/squam/raid/data/GLEAM/Figures/v1"

##################################################################################################################
### Map data ###
country = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/FAOCountryBoundaries2012/","Adm012p")

##################################################################################################################
### WBM output from geotiff files

# Total crop + pasture ET
GDALinfo(file.path(path, "etIrrCrops/etIrrCrops_yc.0000-00-00.tif")) # view metadata
# UNITS: mm/day

et.irr = 365.25*raster(file.path(path, "etIrrCrops/etIrrCrops_yc.0000-00-00.tif")) # load; x365 to get yearly total
et.rfd = 365.25*raster(file.path(path, "etRfdCrops/etRfdCrops_yc.0000-00-00.tif")) # load; x365 to get yearly total
et.tot = overlay(et.irr, et.rfd, fun=function(x,y){x+y}, na.rm=T)

# Pasture ET
GDALinfo(file.path(path, "Pasture_BW_ET/Pasture_BW_ET_yc.0000-00-00.tif")) # view metadata
# UNITS = mm/mo

et.rfd.pasture = 12*raster(file.path(path, "Pasture_rfd_AET/Pasture_rfd_AET_yc.0000-00-00.tif"))
et.irr.bw.pasture = 12*raster(file.path(path, "Pasture_BW_ET/Pasture_BW_ET_yc.0000-00-00.tif"))
et.irr.gw.pasture = 12*raster(file.path(path, "Pasture_GW_ET/Pasture_GW_ET_yc.0000-00-00.tif"))
et.irr.pasture = et.irr.bw.pasture + et.irr.gw.pasture

# Global sums
# Total irr
et.tot.km3 = global.sum(et.tot)

et.irr.km3 = global.sum(et.irr)
et.irr.percent = 100*(et.irr.km3/et.tot.km3)

et.rfd.km3 = global.sum(et.rfd)
et.rfd.percent = 100*(et.rfd.km3/et.tot.km3)

# Pasture
et.rfd.pasture.km3 = global.sum(et.rfd.pasture)
et.irr.pasture.km3 = global.sum(et.irr.pasture)

et.pasture.total.km3 = et.rfd.pasture.km3 + et.irr.pasture.km3
et.pasture.percent = 100*(et.pasture.total.km3/et.tot.km3)

# crops - pasture
crops.et.km3 = et.tot.km3 - et.pasture.total.km3
crops.irr.et.km3 = et.irr.km3 - et.irr.pasture.km3
crops.rfd.et.km3 = et.rfd.km3 - et.rfd.pasture.km3

### Blue water ET (irrigated crops only)
output.list = list.files(path)
bw.list = output.list[which(grepl("BW", output.list))]

for(i in bw.list){
  bw.et = raster(file.path(path, i, paste(i, "_yc.0000-00-00.tif", sep="")))
  if(i == bw.list[1]){
    bw.stack = bw.et
  }else{
    bw.stack = stack(bw.stack, bw.et)
  }
}
bw.tot = 12*calc(bw.stack, fun = sum, na.rm=T)


### Green water ET (irrigated + rainfed crops)
output.list = list.files(path)
gw.list = output.list[which(grepl("GW", output.list))]
rfd.list = output.list[which(grepl("rfd_AET", output.list))]
gw.list = c(gw.list, rfd.list)

for(i in gw.list){
  gw.et = raster(file.path(path, i, paste(i, "_yc.0000-00-00.tif", sep="")))
  if(i == gw.list[1]){
    gw.stack = gw.et
  }else{
    gw.stack = stack(gw.stack, gw.et)
  }
}
gw.tot = 12*calc(gw.stack, fun = sum, na.rm=T)

# Global sums
bw.tot.km3 = global.sum(bw.tot)
bw.percent = 100*(bw.tot.km3/et.tot.km3)

gw.tot.km3 = global.sum(gw.tot)
gw.percent = 100*(gw.tot.km3/et.tot.km3)

##################################################################################################################
# Table: global ET by crop
et.table = data.frame(matrix(nr=length(bw.list), nc=4))

output.list = list.files(path)
bw.list  = output.list[which(grepl("BW", output.list))]
gw.list  = output.list[which(grepl("GW", output.list))]
rfd.list = output.list[which(grepl("rfd_AET", output.list))]

for(i in 1:length(bw.list)){
  
  # crop name
  et.table[i,1] = unlist(strsplit(bw.list[i], "_"))[1]
  
  # blue water et
  bw.et = 12*raster(file.path(path, bw.list[i], paste(bw.list[i], "_yc.0000-00-00.tif", sep="")))
  et.table[i,2] = global.sum(bw.et)
  
  # green water et (irr only)
  gw.et = 12*raster(file.path(path, gw.list[i], paste(gw.list[i], "_yc.0000-00-00.tif", sep="")))
  et.table[i,3] = global.sum(gw.et)
  
  # rfd et
  rfd.et = 12*raster(file.path(path, rfd.list[i], paste(rfd.list[i], "_yc.0000-00-00.tif", sep="")))
  et.table[i,4] = global.sum(rfd.et)
  
  print(i)
}

# output table
out.table = data.frame(matrix(nr=length(bw.list), nc=6))

out.table[,1] = et.table[,1]             # crop names
out.table[,2] = rowSums(et.table[,2:4])  # total ET
out.table[,3] = et.table[,2]             # blue water ET
out.table[,4] = rowSums(et.table[,3:4])  # green water ET: irr + rainfed
out.table[,5] = rowSums(et.table[,2:3])  # irrigated crop ET
out.table[,6] = et.table[,4]             # rainfed crop ET
out.table[,2:6] = signif(out.table[,2:6], 3)
colnames(out.table) = c("Crop", "Total_ET_km3Yr", "Blue_Water_ET", "Green_Water_ET", "Irr_ET", "Rainfed_ET")
write.table(out.table, "/net/nfs/squam/raid/data/GLEAM/R/ET_Summary.csv", sep=",", row.names=F)

##################################################################################################################
### Validation: aquastat vs WBM irrigation water withdrawals

# load aquastat data
fao.dat = read.csv("/net/nfs/squam/raid/data/GLEAM/R/aquastat_AgWithdrawals.csv")
fao.dat = subset(fao.dat, !is.na(fao.dat[,6])) # remove NA values and empty rows

# subset country shapefile to countries with matching FAO names
match.list = mat.or.vec(nr=nrow(fao.dat), nc=2)
match.list[,1] = as.character(fao.dat[,1])
for(i in 1:nrow(fao.dat)){
   dat.name = as.character(fao.dat[i,1])
   m = which(grepl(dat.name, as.character(country$ADM0_NAME)))
   if(length(m) > 0){
     match.list[i,2] = m
   }else{
     match.list[i,2] = NA
   }
}

# manually identify unmatched names
unm = which(is.na(match.list[,2]))
match.list[19, 2] = 33

which(grepl("Bolivia", as.character(country$ADM0_NAME)))
match.list[33,2] = 33

which(grepl("Korea", as.character(country$ADM0_NAME)))
match.list[43,2] = 66

which(grepl("Iran", as.character(country$ADM0_NAME)))
match.list[76,2] = 119

m=which(grepl("Britain", as.character(country$ADM0_NAME)))
as.character(country$ADM0_NAME)[m]
match.list[170,2] = 258

which(grepl("Venezuela", as.character(country$ADM0_NAME)))
match.list[175,2] = 268

# subset fao list
fao.dat = subset(fao.dat, !is.na(match.list[,2]))

match.list = subset(match.list, !is.na(match.list[,2]))

# spatial aggregation of WBM output
# Note: use netCDF output here to get uncertainty due to interannual variability
wbm.irr = wbm_load("/net/nfs/squam/raid/data/GLEAM/WBM/GLEAM_v1/yearly/irrigationGross", "irrigationGross") # stack
wbm.irr = 365.25 * wbm.irr # convert from ave mm/day to total mm/year
wbm.irr.country = spatial_aggregation(raster.data = wbm.irr,
                                      shapefile   = country)

for(i in 1:nrow(fao.dat)){
  fao.dat[i,9]   = min(wbm.irr.country[as.numeric(match.list[i,2]),]@data[16:26])
  fao.dat[i,10]  = max(wbm.irr.country[as.numeric(match.list[i,2]),]@data[16:26])
  fao.dat[i,11]  = mean(as.numeric(wbm.irr.country[as.numeric(match.list[i,2]),]@data[16:26]))
}
colnames(fao.dat)[9:11] = c("wbm_min", "wbm_max", "wbm_mean")

# identify fao data that was taken within the simulation time frame (2005-2015)
fao.2010 = subset(fao.dat, fao.dat$Year > 2004)
fao.old  = subset(fao.dat, fao.dat$Year <= 2004)

# data for linear regression
y = log10(fao.2010$wbm_mean)
x = log10(fao.2010$Value)
z = cbind(x,y)
z = subset(z, !is.infinite(z[,1]))
z = subset(z, !is.infinite(z[,2]))

png(file.path(path.figures, "Water_withdrawal_validation.png"), res=100, width=800, height=800)
plot(fao.2010$Value, fao.2010$wbm_mean, pch=19,  ylim=c(0.0000001,1250), xlim=c(0.0000001,1250), log = 'xy',
     xlab = c("AQUASTAT Agricultural Water Withdrawal by Country (km3/yr)"),
     ylab = c("Simulated Irrigation Withdrawal by Country (km3/yr)"))
arrows(fao.2010$Value, fao.2010$wbm_min, 
       fao.2010$Value, fao.2010$wbm_max, 
       length=0.05, angle=90, code=3)
points(fao.old$Value, fao.old$wbm_mean)
arrows(fao.old$Value, fao.old$wbm_min, 
       fao.old$Value, fao.old$wbm_max, 
       length=0.05, angle=90, code=3)
abline(lm(z[,2]~z[,1]), col='blue')
abline(a=0, b=1, lty=2, col='grey')
legend("topleft", legend = c("Data from < 2005", "Data from 2005 - 2015", "Linear regression", "1:1 Line"),
       pch=c(1, 19, NA, NA), lty=c(NA, NA, 1, 2), col=c("black", "black", "blue", "grey"))
dev.off()


##### subset to countries with > 0.01 km3/yr water withdrawals
fao.2010.l = subset(fao.2010, fao.2010$Value >= 0.01)
fao.old.l  = subset(fao.old, fao.old$Value >= 0.01)

# data for linear regression
y = log10(fao.2010.l$wbm_mean)
x = log10(fao.2010.l$Value)
z = cbind(x,y)
z = subset(z, !is.infinite(z[,1]))
z = subset(z, !is.infinite(z[,2]))

png(file.path(path.figures, "Water_withdrawal_validation_0.01.png"), res=100, width=800, height=800)
plot(fao.2010.l$Value, fao.2010.l$wbm_mean, pch=19,  ylim=c(0.001,1250), xlim=c(0.001,1250), log = 'xy',
     xlab = c("AQUASTAT Agricultural Water Withdrawal by Country (km3/yr)"),
     ylab = c("Simulated Irrigation Withdrawal by Country (km3/yr)"))
arrows(fao.2010.l$Value, fao.2010.l$wbm_min, 
       fao.2010.l$Value, fao.2010.l$wbm_max, 
       length=0.05, angle=90, code=3)
points(fao.old.l$Value, fao.old.l$wbm_mean)
arrows(fao.old.l$Value, fao.old.l$wbm_min, 
       fao.old.l$Value, fao.old.l$wbm_max, 
       length=0.05, angle=90, code=3)
abline(lm(z[,2]~z[,1]), col='blue')
abline(a=0, b=1, lty=2, col='grey')
legend("topleft", legend = c("Data from < 2005", "Data from 2005 - 2015", "Linear regression", "1:1 Line"),
       pch=c(19, 1, NA, NA), lty=c(NA, NA, 1, 2), col=c("black", "black", "blue", "grey"))
dev.off()
##################################################################################################################
### MAPS
yl = c(-56, 85)

png(file.path(path.figures,"ET_AllCrops_Pasture_mmYr.png"), res=100, height=550, width=1000)
plot(et.tot,  ylim=yl)
plot(country, ylim=yl, add=T, lwd=0.5)
dev.off()

bw.tot = mask(bw.tot, country)
png(file.path(path.figures,"BW_ET_IrrCrops_IrrPasture_mmYr.png"), res=100, height=550, width=1000)
plot(bw.tot,  ylim=yl)
plot(country, ylim=yl, add=T, lwd=0.5)
dev.off()

gw.tot = mask(gw.tot, country)
png(file.path(path.figures,"GW_ET_AllCrops_Pasture_mmYr.png"), res=100, height=550, width=1000)
plot(gw.tot,  ylim=yl)
plot(country, ylim=yl, add=T, lwd=0.5)
dev.off()
