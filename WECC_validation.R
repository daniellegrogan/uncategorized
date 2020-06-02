# WECC_validation()

# compare WBM output using different search distance parameters to USGS groundwater and total irrigation water use data

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(readxl)

source("/net/home/eos/dgrogan/git_repos/WBMr/wbm_load.R")
source("/net/home/eos/dgrogan/git_repos/WBMr/vars_from_init.R")
source("/net/home/eos/dgrogan/git_repos/WBMr/spatial_aggregation.R")


# load WBM data
wbm.path.sd50 = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/WECC_MERRA/yearly"
wbm.path.sd00 = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/WECC_MERRA_sd0/yearly"

irr.total.sd50 = wbm_load(path = file.path(wbm.path.sd50, "irrigationGross"), varname = "irrigationGross")
irr.total.sd00 = wbm_load(path = file.path(wbm.path.sd00, "irrigationGross"), varname = "irrigationGross")

irr.ugw.sd50 = wbm_load(path = file.path(wbm.path.sd50, "irrigationExtra"), varname = "irrigationExtra")
irr.ugw.sd00 = wbm_load(path = file.path(wbm.path.sd00, "irrigationExtra"), varname = "irrigationExtra")

irr.sgw.sd50 = wbm_load(path = file.path(wbm.path.sd50, "irrigationGrwt"), varname = "irrigationGrwt")
irr.sgw.sd00 = wbm_load(path = file.path(wbm.path.sd00, "irrigationGrwt"), varname = "irrigationGrwt")

# calculate total groundwater use (sum of UGW and SGW)
irr.gw.sd50 = irr.ugw.sd50 + irr.sgw.sd50
irr.gw.sd00 = irr.ugw.sd00 + irr.sgw.sd00


# spatial aggregation of WBM data
counties = readOGR(dsn   = "/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_county_500k/", 
                   layer = "cb_2015_us_county_500k")

counties = crop(counties, extent(irr.total.sd50))
counties = counties[counties$STATEFP != "02",]  # remove alaska

total.sd50.counties = spatial_aggregation(irr.total.sd50*365, counties)
total.sd00.counties = spatial_aggregation(irr.total.sd00*365, counties)

gw.sd50.counties = spatial_aggregation(irr.gw.sd50*365, counties)
gw.sd00.counties = spatial_aggregation(irr.gw.sd00*365, counties)


# Calculate mean and range

# total irr
total.sd50.counties.stats = cbind(total.sd50.counties@data[,c(1,2,6)],
                                 apply(total.sd50.counties@data[,10:25], c(1), mean), 
                                 apply(total.sd50.counties@data[,10:25], c(1), min),
                                 apply(total.sd50.counties@data[,10:25], c(1), max))
colnames(total.sd50.counties.stats)[4:6] = c("wbm.sd50.mean", "wbm.sd50.min", "wbm.sd50.max")


total.sd00.counties.stats = cbind(total.sd00.counties@data[,c(1,2,6)],
                                  apply(total.sd00.counties@data[,10:25], c(1), mean), 
                                  apply(total.sd00.counties@data[,10:25], c(1), min),
                                  apply(total.sd00.counties@data[,10:25], c(1), max))
colnames(total.sd00.counties.stats)[4:6] = c("wbm.sd00.mean", "wbm.sd00.min", "wbm.sd00.max")

# groundwater
gw.sd50.counties.stats = cbind(gw.sd50.counties@data[,c(1,2,6)],
                                  apply(gw.sd50.counties@data[,10:25], c(1), mean), 
                                  apply(gw.sd50.counties@data[,10:25], c(1), min),
                                  apply(gw.sd50.counties@data[,10:25], c(1), max))
colnames(gw.sd50.counties.stats)[4:6] = c("wbm.sd50.mean", "wbm.sd50.min", "wbm.sd50.max")


gw.sd00.counties.stats = cbind(gw.sd00.counties@data[,c(1,2,6)],
                                  apply(gw.sd00.counties@data[,10:25], c(1), mean), 
                                  apply(gw.sd00.counties@data[,10:25], c(1), min),
                                  apply(gw.sd00.counties@data[,10:25], c(1), max))
colnames(gw.sd00.counties.stats)[4:6] = c("wbm.sd00.mean", "wbm.sd00.min", "wbm.sd00.max")

# groundwater fraction
gwfrac.sd50.counties.stats = cbind(gw.sd50.counties@data[,c(1,2,6)],
                                 apply( (gw.sd50.counties@data[,10:25]/total.sd50.counties@data[,10:25]), c(1), mean), 
                                 apply( (gw.sd50.counties@data[,10:25]/total.sd50.counties@data[,10:25]), c(1), min),
                                 apply( (gw.sd50.counties@data[,10:25]/total.sd50.counties@data[,10:25]), c(1), max))
colnames(gwfrac.sd50.counties.stats)[4:6] = c("gwfrac.wbm.sd50.mean", "gwfrac.wbm.sd50.min", "gwfrac.wbm.sd50.max")

gwfrac.sd00.counties.stats = cbind(gw.sd00.counties@data[,c(1,2,6)],
                                   apply( (gw.sd00.counties@data[,10:25]/total.sd00.counties@data[,10:25]), c(1), mean), 
                                   apply( (gw.sd00.counties@data[,10:25]/total.sd00.counties@data[,10:25]), c(1), min),
                                   apply( (gw.sd00.counties@data[,10:25]/total.sd00.counties@data[,10:25]), c(1), max))
colnames(gwfrac.sd00.counties.stats)[4:6] = c("gwfrac.wbm.sd00.mean", "gwfrac.wbm.sd00.min", "gwfrac.wbm.sd00.max")

# load USGS county-level data
usgs.path = "/net/nfs/merrimack/raid2/data/localdb/biophys/wateruse/USGS-WaterUse-AllYears"
yrs = c(2005, 2010, 2015) #NB: year 2000 doesn't have irrigation data 

for(y in yrs){
  if(y == 2005){
    usgs = read_excel(file.path(usgs.path, y, paste("usco", y, ".xls", sep="")), col_names = TRUE)
  }else if(y == 2010){
    usgs = read_excel(file.path(usgs.path, y, paste("usco", y, ".xlsx", sep="")), na = "N/A", col_names = TRUE)
  }else if(y == 2015){
    usgs = read_excel(file.path(usgs.path, y, paste("usco", y, "v2.0.xlsx", sep="")), na = "N/A", skip=1, col_names = TRUE) 
  }
  
  
  # subset to states in WECC
  usgs.sub = subset(usgs, usgs$STATEFIPS %in% counties$STATEFP)
  
  # total irrigation
  if(y == yrs[1]){
    usgs.tot = cbind(usgs.sub[,1:4], 
                     as.numeric(unlist(usgs.sub[,'IC-WFrTo'])))  # total crop irrigation withdrawals in Mgal/day
    colnames(usgs.tot)[5] = paste('IC-WFrTo', y, sep="_")
                     
    usgs.gw = cbind(usgs.sub[,1:4], 
                    as.numeric(unlist(usgs.sub[,'IC-WGWFr'])))    # groundwater crop irrigation withdrawals in Mgal/day 
    colnames(usgs.tot)[5] = paste('IC-WGWFr', y, sep="_")

  }else{
    usgs.tot = cbind(usgs.tot,                     
                     as.numeric(unlist(usgs.sub[,'IC-WFrTo'])))    # total crop irrigation withdrawals in Mgal/day
    colnames(usgs.tot)[ncol(usgs.tot)] = paste('IC-WFrTo', y, sep="_")
    
    usgs.gw = cbind(usgs.gw,                     
                    as.numeric(unlist(usgs.sub[,'IC-WGWFr'])))   # groundwater crop irrigation withdrawals in Mgal/day
    colnames(usgs.gw)[ncol(usgs.gw)] = paste('IC-WGWFr', y, sep="_")
  }
}

# calculate mean, min, max for USGS data
# also convert units from Mgal/d to km3/yr
unit_conv = 365*4.54609E-6
total.usgs.mean = apply(usgs.tot[,5:7], c(1), mean, na.rm=T)*unit_conv
total.usgs.min = apply(usgs.tot[,5:7], c(1), min, na.rm=T)*unit_conv
total.usgs.max = apply(usgs.tot[,5:7], c(1), max, na.rm=T)*unit_conv
tot.usgs.stats = cbind(usgs.tot[,1:3], total.usgs.mean, total.usgs.min, total.usgs.max)

gw.usgs.mean = apply(usgs.gw[,5:7], c(1), mean, na.rm=T)*unit_conv
gw.usgs.min = apply(usgs.gw[,5:7], c(1), min, na.rm=T)*unit_conv
gw.usgs.max = apply(usgs.gw[,5:7], c(1), max, na.rm=T)*unit_conv
gw.usgs.stats = cbind(usgs.gw[,1:3], gw.usgs.mean, gw.usgs.min, gw.usgs.max)

gwfrac.usgs = usgs.gw[,5:7]/usgs.tot[,5:7]
gwfrac.usgs.mean = apply(gwfrac.usgs, c(1), mean, na.rm=T)
gwfrac.usgs.min  = apply(gwfrac.usgs, c(1), min, na.rm=T)
gwfrac.usgs.max  = apply(gwfrac.usgs, c(1), max, na.rm=T)
gwfrac.usgs.stats = cbind(usgs.gw[,1:3], gwfrac.usgs.mean, gwfrac.usgs.min, gwfrac.usgs.max)

# COMPARE!
colnames(tot.usgs.stats)[2:3] = colnames(gw.usgs.stats)[2:3] = colnames(gwfrac.usgs.stats)[2:3] = c("STATEFP", "COUNTYFP")  # ensure consistent column names for merging

tot.merge = merge(tot.usgs.stats, total.sd00.counties.stats)
tot.merge = merge(tot.merge, total.sd50.counties.stats)

gw.merge = merge(gw.usgs.stats, gw.sd00.counties.stats)
gw.merge = merge(gw.merge, gw.sd50.counties.stats)

frac.mean.sd00 = gw.merge$wbm.sd00.mean / tot.merge$wbm.sd00.mean
frac.mean.sd50 = gw.merge$wbm.sd50.mean / tot.merge$wbm.sd50.mean
frac.mean.usgw = gw.merge$gw.usgs.mean / tot.merge$total.usgs.mean
frac.merge = cbind(gw.merge[,1:4], frac.mean.sd00, frac.mean.sd50, frac.mean.usgw)

#frac.merge = merge(gwfrac.usgs.stats, gwfrac.sd00.counties.stats)
#frac.merge = merge(frac.merge, gwfrac.sd50.counties.stats)

# plot
png("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/USGS_GrossIrr_comparison_volume.png", 
    res=300, unit='in', width=6, height=6)
plot(tot.merge$wbm.sd00.mean ~ tot.merge$total.usgs.mean, pch=19,
     xlab = "USGS Irrigation Water Extraction (km3/year)",
     ylab = "WBM Irrigation Water Use (km3/year)",
     main = "County Irrigation Water")
#points(tot.merge$wbm.sd50.mean ~ tot.merge$total.usgs.mean, col='blue')
abline(a=0, b=1, lty=2, col='grey')
abline(lm(tot.merge$wbm.sd00.mean ~ tot.merge$total.usgs.mean), col='black')
text(x=3.2, y=0,  paste("R2 =", signif(summary(lm(tot.merge$wbm.sd00.mean ~ tot.merge$total.usgs.mean))$r.square,2)))
dev.off()

png("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/USGS_GW_comparison_volume.png", 
    res=300, unit='in', width=6, height=6)
plot(gw.merge$wbm.sd00.mean ~ gw.merge$gw.usgs.mean, pch=19,
     xlab = "USGS Groundwater Use (km3/year)",
     ylab = "WBM Groundwater Use (km3/year)",
     main = "County groundwater use")
points(gw.merge$wbm.sd50.mean ~ gw.merge$gw.usgs.mean, col='blue')
abline(a=0, b=1, lty=2, col='grey')
abline(lm(gw.merge$wbm.sd00.mean ~ gw.merge$gw.usgs.mean), col='black')
abline(lm(gw.merge$wbm.sd50.mean ~ gw.merge$gw.usgs.mean), col='blue')
text(x=1.85, y=2.9,  paste("R2 =", signif(summary(lm(gw.merge$wbm.sd00.mean ~ gw.merge$gw.usgs.mean))$r.square,2)))
text(x=1.85, y=2.75, paste("R2 =", signif(summary(lm(gw.merge$wbm.sd50.mean ~ gw.merge$gw.usgs.mean))$r.square,2)), col='blue')
legend("topleft", legend=c("Search Dist 0 km", "Search Dist 50 km"), pch=c(19, 1), col=c("black", "blue"))
dev.off()


# compare gw as fraction of total
png("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/USGS_GW_comparison_fraction.png", 
    res=300, unit='in', width=6, height=6)
plot(frac.merge$frac.mean.sd00 ~ frac.merge$frac.mean.usgw, pch=19,
     xlab = "USGS Groundwater Use (fraction)",
     ylab = "WBM Groundwater Use (fraction)",
     main = "County groundwater use as fraction of total")
points(frac.merge$frac.mean.sd50 ~ frac.merge$frac.mean.usgw, col='blue')
abline(a=0, b=1, lty=2, col='grey')
abline(lm(frac.merge$frac.mean.sd00 ~ frac.merge$frac.mean.usgw), col='black')
abline(lm(frac.merge$frac.mean.sd50 ~ frac.merge$frac.mean.usgw), col='blue')
text(x=0.95, y=0.2,  paste("R2 =", signif(summary(lm(frac.merge$frac.mean.sd00 ~ frac.merge$frac.mean.usgw))$r.square,2)))
text(x=0.95, y=0.15, paste("R2 =", signif(summary(lm(frac.merge$frac.mean.sd50 ~ frac.merge$frac.mean.usgw))$r.square,2)), col='blue')
legend("topleft", legend=c("Search Dist 0 km", "Search Dist 50 km"), pch=c(19, 1), col=c("black", "blue"))
dev.off()


#### compare for states of interest to DNDC
states = c("WA", "OR", "ID", "AZ", "CA", "NV", "UT")
tot.merge.sub = subset(tot.merge, tot.merge$STATE %in% states)
gw.merge.sub = subset(gw.merge, gw.merge$STATE %in% states)
frac.merge.sub = subset(frac.merge, frac.merge$STATE %in% states)

# color by state
cols = c('#3cb44b', '#4363d8', '#f58231', '#911eb4', 
         '#46f0f0', '#f032e6',  '#fabebe', '#008080', '#bcf60c',
         '#e6beff', '#9a6324', "#A9A9A9", '#800000', '#000075',
         '#808080','#aaffc3', '#808000', '#ffd8b1','#e6194b')
tot.merge.sub$col = cols[unlist(lapply(tot.merge.sub$STATE, FUN = function(x) which(states == x)))]

png("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/USGS_GrossIrr_comparison_volume_States.png", 
    res=300, unit='in', width=6, height=6)
par(mar=c(5.1, 4.1, 4.1, 5.1), xpd=FALSE)
plot(tot.merge.sub$wbm.sd00.mean ~ tot.merge.sub$total.usgs.mean, pch=19, col=tot.merge.sub$col,
     xlab = "USGS Irrigation Water Extraction (km3/year)",
     ylab = "WBM Irrigation Water Use (km3/year)",
     main = "County Irrigation Water")
#points(tot.merge$wbm.sd50.mean ~ tot.merge$total.usgs.mean, col='blue')
abline(a=0, b=1, lty=2, col='grey')
abline(lm(tot.merge.sub$wbm.sd00.mean ~ tot.merge.sub$total.usgs.mean), col='black')
text(x=3.2, y=0,  paste("R2 =", signif(summary(lm(tot.merge.sub$wbm.sd00.mean ~ tot.merge.sub$total.usgs.mean))$r.square,2)))
par(xpd=TRUE)
legend("topright", bty='n', cex=0.9, inset=c(-0.16,-0.05), legend=unique(tot.merge.sub$STATE), title="State", pch=rep(19,length(states)), col=c(unique(tot.merge.sub$col)))
dev.off()


gw.merge.sub$col = cols[unlist(lapply(gw.merge.sub$STATE, FUN = function(x) which(states == x)))]
png("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/USGS_GW_comparison_volume_States.png", 
    res=300, unit='in', width=6, height=6)
par(mar=c(5.1, 4.1, 4.1, 5.1), xpd=FALSE)
plot(gw.merge.sub$wbm.sd00.mean ~ gw.merge.sub$gw.usgs.mean, pch=19, col=gw.merge.sub$col,
     xlab = "USGS Groundwater Use (km3/year)",
     ylab = "WBM Groundwater Use (km3/year)",
     main = "County groundwater use")
points(gw.merge.sub$wbm.sd50.mean ~ gw.merge.sub$gw.usgs.mean, col=gw.merge.sub$col, pch=1)
abline(a=0, b=1, lty=2, col='grey')
abline(lm(gw.merge.sub$wbm.sd00.mean ~ gw.merge.sub$gw.usgs.mean), col='black')
abline(lm(gw.merge.sub$wbm.sd50.mean ~ gw.merge.sub$gw.usgs.mean), col='blue')
text(x=1.8, y=2.9,  paste("R2 =", signif(summary(lm(gw.merge.sub$wbm.sd00.mean ~ gw.merge.sub$gw.usgs.mean))$r.square,2)))
text(x=1.8, y=2.75, paste("R2 =", signif(summary(lm(gw.merge.sub$wbm.sd50.mean ~ gw.merge.sub$gw.usgs.mean))$r.square,2)), col='blue')
legend("topleft", legend=c("Search Dist 0 km", "Search Dist 50 km"), pch=c(19, 1), col=c("black", "blue"))
par(xpd=TRUE)
legend("topright", bty='n', cex=0.9, inset=c(-0.16,-0.05), legend=unique(gw.merge.sub$STATE), title="State", pch=rep(19,length(states)), col=c(unique(gw.merge.sub$col)))
dev.off()


# compare gw as fraction of total
frac.merge.sub$col = cols[unlist(lapply(frac.merge.sub$STATE, FUN = function(x) which(states == x)))]
png("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/USGS_GW_comparison_fraction_states.png", 
    res=300, unit='in', width=6, height=6)
par(mar=c(5.1, 4.1, 4.1, 5.1), xpd=FALSE)
plot(frac.merge.sub$frac.mean.sd00 ~ frac.merge.sub$frac.mean.usgw, pch=19, col=frac.merge.sub$col, xlim=c(0,1), ylim=c(0,1),
     xlab = "USGS Groundwater Use (fraction)",
     ylab = "WBM Groundwater Use (fraction)",
     main = "County groundwater use as fraction of total")
points(frac.merge.sub$frac.mean.sd50 ~ frac.merge.sub$frac.mean.usgw, col=frac.merge.sub$col)
abline(a=0, b=1, lty=2, col='grey')
abline(lm(frac.merge.sub$frac.mean.sd00 ~ frac.merge.sub$frac.mean.usgw), col='black')
abline(lm(frac.merge.sub$frac.mean.sd50 ~ frac.merge.sub$frac.mean.usgw), col='blue')
text(x=0.93, y=0.06,  paste("R2 =", signif(summary(lm(frac.merge.sub$frac.mean.sd00 ~ frac.merge.sub$frac.mean.usgw))$r.square,2)))
text(x=0.93, y=0.01, paste("R2 =", signif(summary(lm(frac.merge.sub$frac.mean.sd50 ~ frac.merge.sub$frac.mean.usgw))$r.square,2)), col='blue')
legend("bottomleft", legend=c("Search Dist 0 km", "Search Dist 50 km"), pch=c(19, 1), col=c("black", "blue"))
par(xpd=TRUE)
legend("topright", bty='n', cex=0.9, inset=c(-0.16,-0.05), legend=unique(frac.merge.sub$STATE), title="State", pch=rep(19,length(states)), col=c(unique(frac.merge.sub$col)))
dev.off()



# each state individually
states = c("WA", "OR", "ID", "AZ", "CA", "NV", "UT")
par(xpd=FALSE)
for(s in states){
  # plot
  tot.merge.sub = subset(tot.merge, tot.merge$STATE == s)
  gw.merge.sub = subset(gw.merge, gw.merge$STATE  == s)
  frac.merge.sub = subset(frac.merge, frac.merge$STATE  == s)
  
  png(paste("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/USGS_GrossIrr_comparison_volume_", s, ".png", sep=""),
      res=300, unit='in', width=6, height=6)
  plot(tot.merge.sub$wbm.sd00.mean ~ tot.merge.sub$total.usgs.mean, pch=19, ylim=c(0, max(tot.merge.sub$wbm.sd00.mean)),
       xlab = "USGS Irrigation Water Extraction (km3/year)",
       ylab = "WBM Irrigation Water Use (km3/year)",
       main = paste("County Irrigation Water", s),
       sub =  paste("R2 =", signif(summary(lm(tot.merge.sub$wbm.sd00.mean ~ tot.merge.sub$total.usgs.mean))$r.square,2))
  )
  #points(tot.merge.sub$wbm.sd50.mean ~ tot.merge.sub$total.usgs.mean, col='blue')
  abline(a=0, b=1, lty=2, col='grey')
  abline(lm(tot.merge.sub$wbm.sd00.mean ~ tot.merge.sub$total.usgs.mean), col='black')
  dev.off()
  
  png(paste("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/USGS_GW_comparison_volume_", s, ".png", sep=""), 
      res=300, unit='in', width=6, height=6)
  plot(gw.merge.sub$wbm.sd00.mean ~ gw.merge.sub$gw.usgs.mean, pch=19,
       xlab = "USGS Groundwater Use (km3/year)",
       ylab = "WBM Groundwater Use (km3/year)",
       main = paste("County groundwater use", s),
       sub =  paste("SD 0 R2 =", signif(summary(lm(gw.merge.sub$wbm.sd00.mean ~ gw.merge.sub$gw.usgs.mean))$r.square,2), ";",
                    "SD 50 R2 =", signif(summary(lm(gw.merge.sub$wbm.sd50.mean ~ gw.merge.sub$gw.usgs.mean))$r.square,2) )
  )
  points(gw.merge.sub$wbm.sd50.mean ~ gw.merge.sub$gw.usgs.mean, col='blue')
  abline(a=0, b=1, lty=2, col='grey')
  abline(lm(gw.merge.sub$wbm.sd00.mean ~ gw.merge.sub$gw.usgs.mean), col='black')
  abline(lm(gw.merge.sub$wbm.sd50.mean ~ gw.merge.sub$gw.usgs.mean), col='blue')
  legend("topleft", legend=c("Search Dist 0 km", "Search Dist 50 km"), pch=c(19, 1), col=c("black", "blue"))
  dev.off()
  
  
  # compare gw as fraction of total
  png(paste("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/USGS_GW_comparison_fraction_", s, ".png", sep=""), 
      res=300, unit='in', width=6, height=6)
  plot(frac.merge.sub$frac.mean.sd00 ~ frac.merge.sub$frac.mean.usgw, pch=19, xlim=c(0,1), ylim=c(0,1),
       xlab = "USGS Groundwater Use (fraction)",
       ylab = "WBM Groundwater Use (fraction)",
       main = paste("County groundwater use as fraction of total", s),
       sub =  paste("SD 0 R2 =", signif(summary(lm(frac.merge.sub$frac.mean.sd00 ~ frac.merge.sub$frac.mean.usgw))$r.square,2), ";",
                    "SD 50 R2 =", signif(summary(lm(frac.merge.sub$frac.mean.sd50 ~ frac.merge.sub$frac.mean.usgw))$r.square,2) ))
  points(frac.merge.sub$frac.mean.sd50 ~ frac.merge.sub$frac.mean.usgw, col='blue')
  abline(a=0, b=1, lty=2, col='grey')
  abline(lm(frac.merge.sub$frac.mean.sd00 ~ frac.merge.sub$frac.mean.usgw), col='black')
  abline(lm(frac.merge.sub$frac.mean.sd50 ~ frac.merge.sub$frac.mean.usgw), col='blue')
  legend("topleft", legend=c("Search Dist 0 km", "Search Dist 50 km"), pch=c(19, 1), col=c("black", "blue"))
  dev.off()
  
}
