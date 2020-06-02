# WBM results figures for DOE PCHES presentation
# Project: DOE 1.2
# Danielle S Grogan

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(rasterVis)

source("/net/home/eos/dgrogan/git_repos/WBMr/wbm_load.R")
source("/net/home/eos/dgrogan/git_repos/WBMr/spatial_aggregation.R")


##################################################################################################################################
# Mapping

# load state shapefile for spatial aggregation
states = readOGR(dsn   = "/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_state_500k/", 
                 layer = "cb_2015_us_state_500k")

# States in the "West of Continental Divide" region (WoCD)
WoCD_state_names = c("AZ","CA","ID","NV","OR","UT","WA")

# subset to states in WoCD
states.WoCD = states[states$STUSPS %in% WoCD_state_names,]

##################################################################################################################################
baseline.path = "/net/nfs/squam/raid/data/DOE_PCHES/WBM_output/WECC_MERRA2_baseline/"
result.path = "/net/nfs/squam/raid/data/DOE_PCHES/WBM_output/WECC_MERRA2_scaled/"
figure.path = "/net/nfs/squam/raid/data/DOE_PCHES/Figures/"
##################################################################################################################################

# Compare irrigationGross
irrGross.b = raster(file.path(baseline.path, "climatology", "wbm_irrigationGross_yc.nc"))
irrGross.r = raster(file.path(result.path,   "climatology", "wbm_irrigationGross_yc.nc"))

irrGross.b = crop(irrGross.b, extent(states.WoCD))
irrGross.r = crop(irrGross.r, extent(states.WoCD))

#plot(irrGross.b)
#plot(irrGross.r)

# percent diff
irrGross.diff = 100*(irrGross.r - irrGross.b)/irrGross.b  # percent change
irrGross.diff[irrGross.b < 0.000001] = NA   # mask out very small irrigation grid cells

cols = colorRampPalette(colors = rev(brewer.pal(9, "YlGnBu"))[3:9], interpolate = "linear")(100)  
plot(irrGross.diff, col = cols, bty='n', box=F)
plot(states.WoCD, add=T, lwd=2)

png(file.path(figure.path, "IrrDemand_percent_diff.png"), width = 1000, height=1300, res=180)
plot(irrGross.diff, col = cols, bty='n', box=F)
plot(states.WoCD, add=T, lwd=2.1)
dev.off()

# mm diff
irrGross.diff = 365*(irrGross.r - irrGross.b)  # mm change
irrGross.diff[irrGross.b < 0.000001] = NA   # mask out very small irrigation grid cells

cols = colorRampPalette(colors = rev(brewer.pal(9, "PuBuGn")), interpolate = "linear")(100)  
plot(irrGross.diff, col = cols, bty='n', box=F)
plot(states.WoCD, add=T, lwd=2)

png(file.path(figure.path, "IrrDemand_mmYr_diff.png"), width = 1000, height=1300, res=180)
plot(irrGross.diff, col = cols, bty='n', box=F)
plot(states.WoCD, add=T, lwd=2.1)
dev.off()

#### irrigation Extra

irrExtra.b = raster(file.path(baseline.path, "climatology", "wbm_irrigationExtra_yc.nc"))
irrExtra.r = raster(file.path(result.path,   "climatology", "wbm_irrigationExtra_yc.nc"))

irrExtra.b = crop(irrExtra.b, extent(states.WoCD))
irrExtra.r = crop(irrExtra.r, extent(states.WoCD))

#plot(irrExtra.b)
#plot(irrExtra.r)

# percent diff
irrExtra.diff = 100*(irrExtra.r - irrExtra.b)/irrExtra.b  # percent change
irrExtra.diff[irrExtra.b < 0.000001] = NA   # mask out very small irrigation grid cells
irrExtra.diff[irrExtra.diff > 0] = NA # there are like 2 grid cells with very high values
cols = colorRampPalette(colors = rev(brewer.pal(9, "YlGnBu"))[3:9], interpolate = "linear")(100)  
plot(irrExtra.diff, col = cols, bty='n', box=F)
plot(states.WoCD, add=T, lwd=2)

png(file.path(figure.path, "UGWdemand_percent_diff.png"), width = 1000, height=1300, res=180)
plot(irrExtra.diff, col = cols, bty='n', box=F)
plot(states.WoCD, add=T, lwd=2.1)
dev.off()

# mm diff
irrExtra.diff = 365*(irrExtra.r - irrExtra.b)  # mm change
irrExtra.diff[irrExtra.b < 0.000001] = NA   # mask out very small irrigation grid cells

cols = colorRampPalette(colors = rev(brewer.pal(9, "PuBuGn")), interpolate = "linear")(100)  
plot(irrExtra.diff, col = cols, bty='n', box=F)
plot(states.WoCD, add=T, lwd=2)

png(file.path(figure.path, "UGWdemand_mmYr_diff.png"), width = 1000, height=1300, res=180)
plot(irrExtra.diff, col = cols, bty='n', box=F)
plot(states.WoCD, add=T, lwd=2.1)
dev.off()

plot(irrExtra.r)
