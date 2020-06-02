# Summary and evaluation if v0 WBM water institution simulations
# Project: DOE PCHES 1.2
# Danielle S Grogan
# 2020-04-09

library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(lubridate)

source("~/git_repos/WBMr/wbm_load.R")
source("~/git_repos/WBMr/spatial_aggregation.R")
source("~/git_repos/WBMr/temporal_aggregation.R")
source("~/git_repos/WBMr/vars_from_init.R")
source("~/git_repos/WBMr/mouth_ts.R")
source("~/git_repos/WBMr/FDC.R")

base.path = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/"
plot.path = file.path(base.path, "Analysis/Figures")
table.path= file.path(base.path, "Analysis/Tables")
scen = c("baseline", "marketModified_stps_nonL_wInd_wetScenario")
netwk = raster("/net/nfs/zero/data3/WBM_TrANS/data/WECC_6min_v3b.asc")
wma.poly = readOGR("/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/simpleWMAs_ID/", "simpleWMAs_ID")
crs(wma.poly) = crs(states)
states = readOGR(dsn   = "/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_state_500k/", layer = "cb_2015_us_state_500k")
#states = crop(states, extent(wma.poly))
states.sub = states[states$NAME == "Idaho" | 
                  states$NAME == "Washington" | 
                  states$NAME == "Oregon" | 
                  states$NAME == "Colorado" | 
                  states$NAME == "Arizona" |
                  states$NAME == "California" |
                  states$NAME == "Montana" |
                  states$NAME == "Nevada" |
                  states$NAME == "New Mexico" |
                  states$NAME == "Utah" |
                  states$NAME == "Wyoming",]
wma.poly.sub = wma.poly[wma.poly$state == "Idaho" | 
                          wma.poly$state == "Washington" | 
                          wma.poly$state == "Oregon" | 
                          wma.poly$state == "Colorado" | 
                          wma.poly$state == "Arizona" |
                          wma.poly$state == "California"|
                          wma.poly$state == "Montana" |
                          wma.poly$state == "Nevada" |
                          wma.poly$state == "NewMexico" |
                          wma.poly$state == "Utah" |
                          wma.poly$state == "Wyoming",]

# plotting baseline for states and WMAs
plot(states, col='grey88', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
plot(states.sub, add=T, col='white')
plot(wma.poly.sub, add=T, lwd=0.7, border='grey')
plot(states.sub, add=T)
#######################################################################################################################################################################################
# Analysis
# baseline vs marketModified_stps_nonL_wInd_wetScenario

# 1. Change in irrigation water use
# 1a. Grid cell: climatology
baseline.irr.yc = 365*raster(file.path(base.path, "WatInst_baseline_v1/climatology/wbm_irrigationGross_yc.nc"))
baseline.irr.yc = mask(baseline.irr.yc, states.sub)

baseline.irr = baseline.irr.yc
baseline.irr[baseline.irr == 0] = NA
# plot baseline irr
# mar: bottom, left, top, right
png(file.path(plot.path, "IrrigationGross_Baseline_v1_yc.png"), res=100, width = 600, height=600)
par(mar=c(5.1, 1.1, 2.1, 2.1))
plot(states, col='grey90', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
plot(states.sub, add=T, col='white')
plot(baseline.irr, add=T, col=(brewer.pal(9, "YlGnBu")), legend=F)
plot(wma.poly.sub, add=T, lwd=0.7, border='grey')
plot(states.sub, add=T)
plot(baseline.irr, col=(brewer.pal(9, "YlGnBu")), legend.only=TRUE, horizontal = TRUE, legend.args = list(text='Baseline Irrigation, mm/yr'))
dev.off()

marketWet.irr.yc = 365*raster(file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/climatology/wbm_irrigationGross_yc.nc"))
marketWet.irr.yc = mask(marketWet.irr.yc, states.sub)

# marketWet.irr.yc = 365*raster(file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_dryScenario_v1/climatology/wbm_irrigationGross_yc.nc"))
# marketWet.irr.yc = mask(marketWet.irr.yc, states.sub)


diff.irr = (marketWet.irr.yc - baseline.irr.yc)
diff.irr.p = 100*diff.irr/baseline.irr.yc

diff.irr.p[diff.irr.p > 100] = 100 # saturate the color scale

# mar: bottom, left, top, right
png(file.path(plot.path, "Irrigation_percent_chg_MarketWet_minus_Baseline_v1_yc.png"), res=100, width = 600, height=600)
par(mar=c(5.1, 1.1, 2.1, 2.1))
plot(states, col='grey90', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
plot(states.sub, add=T, col='white')
plot(diff.irr.p, add=T, col=rev(brewer.pal(11, "RdBu")), legend=F)
plot(wma.poly.sub, add=T, lwd=0.7, border='grey')
plot(states.sub, add=T)
plot(diff.irr.p, col=rev(brewer.pal(11, "RdBu")), legend.only=TRUE, horizontal = TRUE, legend.args = list(text='% Change Irrigation Water'))
dev.off()

# 1b. WMA summary (not run)
# 1c. State summary

# use annual values: calculate mean and stdev for table summary
baseline.irr.state = spatial_aggregation(raster.data = wbm_load(path = file.path(base.path, "WatInst_baseline_v1/yearly/irrigationGross"), 
                                                                varname = "irrigationGross"), 
                                        shapefile = states.sub, 
                                        s = 1)
marketWet.irr.state = spatial_aggregation(raster.data = wbm_load(path = file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/yearly/irrigationGross"), 
                                                                 varname = "irrigationGross"), 
                                          shapefile = states.sub, 
                                          s = 1)

diff.irr.state = (marketWet.irr.state@data[,10:14] - baseline.irr.state@data[,10:14])
diff.irr.state.p = 100*diff.irr.state/baseline.irr.state@data[,10:14]
diff.irr.state.p.mean = rowMeans(diff.irr.state.p)
diff.irr.state.p.stdev = apply(diff.irr.state.p, c(1), sd)
diff.irr.state.table = cbind(as.character(baseline.irr.state$NAME), signif(diff.irr.state.p.mean,2), signif(diff.irr.state.p.stdev,2))
colnames(diff.irr.state.table) = c("State", "Mean % Difference", "Stdev")
write.csv(diff.irr.state.table, file.path(table.path, "Irrigation_diff_percent_states_MarketWet_minus_Baseline.csv"), row.names = F, quote=F)


# 5. Change in surface water use and UGW use (to explain CA)
baseline.sw.yc = 365*raster(file.path(base.path, "WatInst_baseline_v1/climatology/wbm_irrigationFlow_yc.nc"))
baseline.sw.yc = mask(baseline.sw.yc, states.sub)

marketWet.sw.yc = 365*raster(file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/climatology/wbm_irrigationFlow_yc.nc"))
marketWet.sw.yc = mask(marketWet.sw.yc, states.sub)

# marketWet.sw.yc = 365*raster(file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_dryScenario_v1/climatology/wbm_irrigationFlow_yc.nc"))
# marketWet.sw.yc = mask(marketWet.sw.yc, states.sub)


diff.sw = (marketWet.sw.yc - baseline.sw.yc)
diff.sw.p = 100*diff.sw/baseline.sw.yc

diff.sw.p[diff.sw.p > 100] = 100 # saturate the color scale

# mar: bottom, left, top, right
png(file.path(plot.path, "SW_Irrigation_percent_chg_MarketWet_minus_Baseline_v1_yc.png"), res=100, width = 600, height=600)
par(mar=c(5.1, 1.1, 2.1, 2.1))
plot(states, col='grey90', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
plot(states.sub, add=T, col='white')
plot(diff.sw.p, add=T, col=rev(brewer.pal(11, "RdBu")), legend=F)
plot(wma.poly.sub, add=T, lwd=0.7, border='grey')
plot(states.sub, add=T)
plot(diff.sw.p, col=rev(brewer.pal(11, "RdBu")), legend.only=TRUE, horizontal = TRUE, legend.args = list(text='% Change Surface Water Irrigation'))
dev.off()


# use annual values: calculate mean and stdev for table summary
baseline.sw.state = spatial_aggregation(raster.data = wbm_load(path = file.path(base.path, "WatInst_baseline_v1/yearly/irrigationFlow"), 
                                                                varname = "irrigationFlow"), 
                                         shapefile = states.sub, 
                                         s = 1)
marketWet.sw.state = spatial_aggregation(raster.data = wbm_load(path = file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/yearly/irrigationFlow"), 
                                                                 varname = "irrigationFlow"), 
                                          shapefile = states.sub, 
                                          s = 1)

diff.sw.state = (marketWet.sw.state@data[,10:14] - baseline.sw.state@data[,10:14])
diff.sw.state.p = 100*diff.sw.state/baseline.sw.state@data[,10:14]
diff.sw.state.p.mean = rowMeans(diff.sw.state.p)
diff.sw.state.p.stdev = apply(diff.sw.state.p, c(1), sd)
diff.sw.state.table = cbind(as.character(baseline.sw.state$NAME), signif(diff.sw.state.p.mean,2), signif(diff.sw.state.p.stdev,2))
colnames(diff.sw.state.table) = c("State", "Mean % Difference", "Stdev")
write.csv(diff.sw.state.table, file.path(table.path, "GW_Irrigation_diff_percent_states_MarketWet_minus_Baseline.csv"), row.names = F, quote=F)



# 6. Change in groundwater use and UGW use (to explain CA)
baseline.gw.yc  = 365*raster(file.path(base.path, "WatInst_baseline_v1/climatology/wbm_irrigationGrwt_yc.nc"))    # sustainable
baseline.ugw.yc = 365*raster(file.path(base.path, "WatInst_baseline_v1/climatology/wbm_irrigationExtra_yc.nc"))   # unsustainable
baseline.tgw.yc = baseline.gw.yc + baseline.ugw.yc   # total
baseline.tgw.yc = mask(baseline.tgw.yc, states.sub)

marketWet.gw.yc = 365*raster(file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/climatology/wbm_irrigationGrwt_yc.nc"))
marketWet.ugw.yc = 365*raster(file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/climatology/wbm_irrigationExtra_yc.nc"))   # unsustainable

# marketWet.gw.yc = 365*raster(file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_dryScenario_v1/climatology/wbm_irrigationGrwt_yc.nc"))
# marketWet.ugw.yc = 365*raster(file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_dryScenario_v1/climatology/wbm_irrigationExtra_yc.nc"))   # unsustainable


marketWet.tgw.yc = marketWet.gw.yc + marketWet.ugw.yc   # total
marketWet.tgw.yc = mask(marketWet.tgw.yc, states.sub)

diff.gw = (marketWet.tgw.yc - baseline.tgw.yc)
diff.gw.p = 100*diff.gw/baseline.tgw.yc
diff.gw.p[is.infinite(diff.gw.p)] = NA
diff.gw.p[diff.gw.p > 100] = 100 # saturate the color scale

# mar: bottom, left, top, right
png(file.path(plot.path, "TGW_Irrigation_percent_chg_MarketWet_minus_Baseline_v1_yc.png"), res=100, width = 600, height=600)
par(mar=c(5.1, 1.1, 2.1, 2.1))
plot(states, col='grey90', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
plot(states.sub, add=T, col='white')
plot(diff.gw.p, add=T, col=rev(brewer.pal(11, "RdBu")), legend=F)
plot(wma.poly.sub, add=T, lwd=0.7, border='grey')
plot(states.sub, add=T)
plot(diff.gw.p, col=rev(brewer.pal(11, "RdBu")), legend.only=TRUE, horizontal = TRUE, legend.args = list(text='% Change Groundwater Irrigation'))
dev.off()


# ugw only
baseline.ugw.yc = mask(baseline.ugw.yc, states.sub)
marketWet.ugw.yc = mask(marketWet.ugw.yc, states.sub)

diff.ugw = (marketWet.ugw.yc - baseline.ugw.yc)
diff.ugw.p = 100*diff.ugw/baseline.ugw.yc
diff.ugw.p[is.infinite(diff.ugw.p)] = NA
diff.ugw.p[diff.ugw.p > 100] = 100 # saturate the color scale

# mar: bottom, left, top, right
png(file.path(plot.path, "UGW_Irrigation_percent_chg_MarketWet_minus_Baseline_v1_yc.png"), res=100, width = 600, height=600)
par(mar=c(5.1, 1.1, 2.1, 2.1))
plot(states, col='grey90', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
plot(states.sub, add=T, col='white')
plot(diff.ugw.p, add=T, col=rev(brewer.pal(11, "RdBu")), legend=F)
plot(wma.poly.sub, add=T, lwd=0.7, border='grey')
plot(states.sub, add=T)
plot(diff.gw.p, col=rev(brewer.pal(11, "RdBu")), legend.only=TRUE, horizontal = TRUE, legend.args = list(text='% Change Unsustainable Groundwater Irrigation'))
dev.off()


# use annual values: calculate mean and stdev for table summary
baseline.gw.state = spatial_aggregation(raster.data = wbm_load(path = file.path(base.path, "WatInst_baseline_v1/yearly/irrigationGrwt"), 
                                                               varname = "irrigationGrwt"), 
                                        shapefile = states.sub, 
                                        s = 1)
baseline.ugw.state = spatial_aggregation(raster.data = wbm_load(path = file.path(base.path, "WatInst_baseline_v1/yearly/irrigationExtra"), 
                                                               varname = "irrigationExtra"), 
                                        shapefile = states.sub, 
                                        s = 1)
marketWet.gw.state = spatial_aggregation(raster.data = wbm_load(path = file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/yearly/irrigationGrwt"), 
                                                                varname = "irrigationGrwt"), 
                                         shapefile = states.sub, 
                                         s = 1)
marketWet.ugw.state = spatial_aggregation(raster.data = wbm_load(path = file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/yearly/irrigationExtra"), 
                                                                varname = "irrigationExtra"), 
                                         shapefile = states.sub, 
                                         s = 1)

baseline.ugw.state@data[1,10:11] = NA  # custum fix to avoid INF
diff.ugw.state = (marketWet.ugw.state@data[,10:14] - baseline.ugw.state@data[,10:14])
diff.ugw.state.p = 100*diff.ugw.state/baseline.ugw.state@data[,10:14]
diff.ugw.state.p.mean = rowMeans(diff.ugw.state.p, na.rm=T)
diff.ugw.state.p.stdev = apply(diff.ugw.state.p, c(1), sd)
diff.ugw.state.table = cbind(as.character(baseline.ugw.state$NAME), signif(diff.ugw.state.p.mean,2), signif(diff.ugw.state.p.stdev,2))
colnames(diff.ugw.state.table) = c("State", "Mean % Difference", "Stdev")
write.csv(diff.ugw.state.table, file.path(table.path, "UGW_Irrigation_diff_percent_states_MarketWet_minus_Baseline.csv"), row.names = F, quote=F)

baseline.tgw.state = baseline.ugw.state@data[,10:14] + baseline.gw.state@data[,10:14] 
baseline.tgw.state[1,1:2] = baseline.gw.state@data[1,10:11]  # custum fix to avoid INF
marketWet.tgw.state = marketWet.ugw.state@data[,10:14] + marketWet.gw.state@data[,10:14] 

diff.tgw.state = (marketWet.tgw.state - baseline.tgw.state)
diff.tgw.state.p = 100*diff.tgw.state/baseline.tgw.state
diff.tgw.state.p.mean = rowMeans(diff.tgw.state.p)
diff.tgw.state.p.stdev = apply(diff.tgw.state.p, c(1), sd)
diff.tgw.state.table = cbind(as.character(baseline.ugw.state$NAME), signif(diff.tgw.state.p.mean,2), signif(diff.tgw.state.p.stdev,2))
colnames(diff.tgw.state.table) = c("State", "Mean % Difference", "Stdev")
write.csv(diff.tgw.state.table, file.path(table.path, "TGW_Irrigation_diff_percent_states_MarketWet_minus_Baseline.csv"), row.names = F, quote=F)

# box plots
baseline.tgw.mean = rowMeans(baseline.tgw.state, na.rm=T)
market.tgw.mean = rowMeans(marketWet.tgw.state, na.rm=T)
names(baseline.tgw.mean) = names(market.tgw.mean) = as.character(baseline.ugw.state$NAME)
tgw.state = 365*rbind(baseline.tgw.mean, market.tgw.mean)

png(file.path(plot.path, "TGW_Irrigation_MarketWet_vs_Baseline_yc_v1_barplot.png"), res=100, width = 800, height=400)
par(mar = c(4.1, 4.5, 3.1, 2.1))
barplot(tgw.state, beside=T, ylim=c(0,13), 
        legend=T, args.legend = list(x="topleft", legend=c("Baseline", "Market")), 
        ylab = "")
title(ylab=expression("Groundwater Use (km"^3~"yr"^-1~")"), line=1.9, cex.lab=1.2, family="Calibri Light")
dev.off()




# box plots
baseline.ugw.mean = rowMeans(baseline.ugw.state@data[,10:14], na.rm=T)
market.ugw.mean = rowMeans(marketWet.ugw.state@data[,10:14], na.rm=T)
names(baseline.ugw.mean) = names(market.ugw.mean) = as.character(baseline.ugw.state$NAME)
ugw.state = 365*rbind(baseline.ugw.mean, market.ugw.mean)


png(file.path(plot.path, "UGW_Irrigation_MarketWet_vs_Baseline_yc_v1_barplot.png"), res=100, width = 800, height=400)
par(mar = c(4.1, 4.5, 3.1, 2.1))
barplot(ugw.state, beside=T, ylim=c(0,13), 
        legend=T, args.legend = list(x="topleft", legend=c("Baseline", "Market")), 
        ylab = "")
title(ylab=expression("UGW Use (km"^3~"yr"^-1~")"), line=1.9, cex.lab=1.2, family="Calibri Light")
dev.off()

# 2. Change in domestic water use
# 2a. Grid cell
# 2b. WMA
# 2c. State summary


# 3. River discharge and temperature and power plant intake points
# pts = as.data.frame(read.delim("/net/nfs/squam/raid/userdata/stanley/WECC_Point_Extraction_2019_08/Wecc_One_Cell_Search.txt"))
# coordinates(pts) = ~ search_Lon + search_Lat
# pts = crop(pts, extent(states.sub))
# crs(pts) = crs(states.sub)
# pts = pts[states.sub,]
# 
# png(file.path(plot.path, "Power_point_locations_subset.png"), res=100, width = 600, height=600)
# par(mar=c(5.1, 1.1, 2.1, 2.1))
# plot(states, col='grey90', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
# plot(states.sub, add=T, col='white', lwd=0.7)
# plot(wma.poly.sub, add=T, lwd=0.7, border='grey')
# plot(states.sub, add=T, lwd=0.7)
# plot(pts, add=T, pch=24, bg='darkorange2', col='black', lwd=0.3, cex=0.5)
# dev.off()
# 
# # 3a. focus on changes in low flows
# Q.baseline  = wbm_load(path = file.path(base.path, "WatInst_baseline_v1/daily"), varname = "discharge")
# Q.marketWet = wbm_load(path = file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/daily"), varname = "discharge")
# 
# find.q10 = function(fdc){
#   d10 = abs(fdc$Ep - 0.10)
#   q10.row = which(d10 == min(d10))
#   out = fdc[q10.row,1]
#   out
# }
# 
# q10.summary = data.frame(matrix(nr=nrow(pts), nc=3))
# q10.summary[,1] = pts$station_ID
# 
# for(i in 1:length(pts)){
#   Q.baseline.fdc = FDC(as.numeric(raster::extract(Q.baseline,  pts[i,])))
#   Q.marketWt.fdc = FDC(as.numeric(raster::extract(Q.marketWet, pts[i,])))
# 
#   Q.baseline.10 = find.q10(Q.baseline.fdc)
#   Q.marketWt.10 = find.q10(Q.marketWt.fdc)
#   diff = Q.marketWt.10 - Q.baseline.10
#   diff.p = 100*(diff/Q.baseline.10)
#   
#   q10.summary[i,2] = diff
#   q10.summary[i,3] = diff.p
#   
#   print(i)
#   print(diff.p)
# }
# 
# pts@data$diff.q = q10.summary[,2]
# pts@data$diff.q.p = q10.summary[,3]
# 
# pts.pos = pts[pts$diff.q.p > 0,]
# pts.neg = pts[pts$diff.q.p < 0,]
# pts.zero = pts[pts$diff.q.p == 0,]
# 
# red.pal <- rev(brewer.pal(9, "Reds"))
# for(i in 1:nrow(pts.neg)){
#   if(pts.neg$diff.q.p[i] < -50){ pts.neg$Col[i] = red.pal[1]}
#   if(pts.neg$diff.q.p[i] >= -50 & pts.neg$diff.q.p[i]< -40){ pts.neg$Col[i] = red.pal[2]}
#   if(pts.neg$diff.q.p[i] >= -40 & pts.neg$diff.q.p[i]< -30){ pts.neg$Col[i] = red.pal[3]}
#   if(pts.neg$diff.q.p[i] >= -30 & pts.neg$diff.q.p[i]< -20){ pts.neg$Col[i] = red.pal[4]}
#   if(pts.neg$diff.q.p[i] >= -20 & pts.neg$diff.q.p[i]< -10){ pts.neg$Col[i] = red.pal[5]}
#   if(pts.neg$diff.q.p[i] >= -10 & pts.neg$diff.q.p[i]< 0  ){ pts.neg$Col[i] = red.pal[6]}
# }
# #pts.neg$Col <- red.pal[as.numeric(cut(pts.neg@data$diff.q.p, breaks = 9))]
# 
# blue.pal <- rev(brewer.pal(9, "Blues"))
# for(i in 1:nrow(pts.pos)){
#   if(pts.pos$diff.q.p[i] > 50){ pts.pos$Col[i] = blue.pal[1]}
#   if(pts.pos$diff.q.p[i] <= 50 & pts.pos$diff.q.p[i]> 40){ pts.pos$Col[i] = blue.pal[2]}
#   if(pts.pos$diff.q.p[i] <= 40 & pts.pos$diff.q.p[i]> 30){ pts.pos$Col[i] = blue.pal[3]}
#   if(pts.pos$diff.q.p[i] <= 30 & pts.pos$diff.q.p[i]> 20){ pts.pos$Col[i] = blue.pal[4]}
#   if(pts.pos$diff.q.p[i] <= 20 & pts.pos$diff.q.p[i]> 10){ pts.pos$Col[i] = blue.pal[5]}
#   if(pts.pos$diff.q.p[i] <= 10 & pts.pos$diff.q.p[i]> 0  ){ pts.pos$Col[i] = blue.pal[6]}
# }
# #pts.pos$Col <- blue.pal[as.numeric(cut(pts.pos@data$diff.q.p, breaks = 9))]
# 
# plot(pts.pos, pch = 24, bg = pts.pos$Col, add=T)
# plot(pts.neg, pch = 25, bg = pts.neg$Col, add=T)
# plot(pts.zero, pch=1, add=T)
# 
# 
# # mar: bottom, left, top, right
# png(file.path(plot.path, "Q_PowerPlant_percent_chg_MarketWet_minus_Baseline_yc.png"), res=100, width = 600, height=600)
# par(mar=c(5.1, 1.1, 2.1, 2.1))
# plot(states, col='grey90', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
# plot(states.sub, add=T, col='white')
# plot(wma.poly.sub, add=T, lwd=0.7, border='grey')
# plot(states.sub, add=T)
# plot(pts.pos, pch = 24, bg = pts.pos$Col, col='darkgrey', lwd=0.5, add=T, cex=0.9)
# plot(pts.neg, pch = 25, bg = adjustcolor(pts.neg$Col, alpha=0.9), col='darkgrey', lwd=0.5, add=T, cex=0.9)
# plot(pts.zero, pch=2, cex=0.9, add=T)
# 
# legend("bottomleft", cex=0.5,
#       title="% Change River Discharge",
#       legend=seq(-50, 0, by=10),
#       fill =red.pal[1:6],
#       bty='n')
# dev.off()
# 
# # legend
# blue.pal[6] = 'white'
# png(file.path(plot.path, "Q_PowerPlant_percent_chg_MarketWet_minus_Baseline_yc_LEGEND.png"), res=100, width = 600, height=600)
# par(mar=c(5.1, 7.1, 2.1, 2.1))
# plot(states, col='grey90', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
# 
# legend("bottomleft", cex=1.2,
#        title="% Change Low Flow",
#        legend=rev(seq(-50, 50, by=10)),
#        fill = c(blue.pal[1:6], rev(red.pal[1:6]))
# )
# dev.off()
# 
# histogram(pts.pos$diff.q.p, nint=20, type='count')
# histogram(pts.neg$diff.q.p, nint=20, type='count')
# histogram(pts$diff.q.p, nint=80, type='count')
# 
# # hydrographs
# 
# pts.max = pts[which(pts$diff.q.p == max(pts$diff.q.p))[1],]
# pts.min = pts[which(pts$diff.q.p == min(pts$diff.q.p))[1],]
# 
# pt.mod = pts[-c(91),]
# pts.min2 = pts[which(pt.mod$diff.q.p == min(pt.mod$diff.q.p))[1],]
# 
# 
# plot(states, col='grey90', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
# plot(states.sub, add=T, col='white')
# plot(pts.min2, add=T, pch=20)
# 
# pts.max.q = raster::extract(Q.baseline, pts.max)
# pts.max.q.market = raster::extract(Q.marketWet, pts.max)
# 
# pts.min.q = raster::extract(Q.baseline, pts.min2)
# pts.min.q.market = raster::extract(Q.marketWet, pts.min2)
# 
# dates.y = seq(from=ymd("2005-01-01"), to=ymd("2009-12-31"), by="day")
# plot(dates.y, pts.max.q, type='l')
# lines(dates.y, pts.max.q.market, col='darkcyan')
# 
# fdc.basline = FDC(pts.max.q)
# fdc.market = FDC(pts.max.q.market)
# 
# png(file.path(plot.path, "FDC_GriffithCCPlant_Basline_vs_Market.png"), res=120, width = 800, height=650)
# plot(fdc.basline[,1], fdc.basline[,2], type='l', log='xy', lwd=1.5,
#      xlab = expression("River Discharge (m"^3~"sec"^-1~")"),
#      ylab = c("Percentile"))
# abline(h=0.1, v=0, lty=2, col='grey')
# lines(fdc.market[,1], fdc.market[,2],  col='royalblue2', lwd=1.5)
# legend("bottomright", lty=c(1,1,2), col=c("black", "royalblue2", "grey"), legend = c("Baseline", "Market", "Q10"))
# dev.off()
# 
# 
# fdc.basline = FDC(pts.min.q)
# fdc.market = FDC(pts.min.q.market)
# 
# png(file.path(plot.path, "FDC_LittleFalls-1_Basline_vs_Market.png"), res=120, width = 800, height=650)
# plot(fdc.basline[,1], fdc.basline[,2], type='l', log='xy', lwd=1.5, 
#      xlab = expression("River Discharge (m"^3~"sec"^-1~")"),
#      ylab = c("Percentile"))
# abline(h=0.1, v=0, lty=2, col='grey')
# lines(fdc.market[,1], fdc.market[,2],  col='red3', lwd=1.5)
# legend("bottomright", lty=c(1,1,2), col=c("black", "red3", "grey"), legend = c("Baseline", "Market", "Q10"))
# dev.off()
# 
# # 3b. focus on changes in high temperatures
# T.baseline  = wbm_load(path = file.path(base.path, "WatInst_baseline_v1/daily"), varname = "discharge_twt")
# T.marketWet = wbm_load(path = file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/daily"), varname = "discharge_twt")
# 
# find.q90 = function(fdc){
#   d90 = abs(fdc$Ep - 0.90)
#   q90.row = which(d90 == min(d90))[1]
#   out = fdc[q90.row,1]
#   out
# }
# 
# T90.summary = data.frame(matrix(nr=nrow(pts), nc=3))
# T90.summary[,1] = pts$station_ID
# 
# for(i in 1:length(pts)){
#   T.baseline.fdc = FDC(as.numeric(raster::extract(T.baseline,  pts[i,])))
#   T.marketWt.fdc = FDC(as.numeric(raster::extract(T.marketWet, pts[i,])))
#   
#   T.baseline.90 = find.q90(T.baseline.fdc)
#   T.marketWt.90 = find.q90(T.marketWt.fdc)
#   diff = T.marketWt.90 - T.baseline.90
#   diff.p = 100*(diff/T.baseline.90)
#   
#   T90.summary[i,2] = diff
#   T90.summary[i,3] = diff.p
#   
#   print(i)
#   print(diff.p)
# }
# 
# T90.summary = read.csv("/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/Analysis/Tables/T90.summary.csv")
# max(T90.summary[,2])
# min(T90.summary[,2])
# 
# # 4. Change in discharge_irr --> water quality indicator
# q.irr.baseline = raster(file.path(base.path, "WatInst_baseline_v1/climatology/wbm_discharge_irr_yc.nc"))
# q.irr.market = raster(file.path(base.path, "WatInst_marketModified_stps_nonL_wInd_wetScenario_v1/climatology/wbm_discharge_irr_yc.nc"))
# 
# diff = q.irr.market - q.irr.baseline
# diff.p = 100*(diff/q.irr.baseline)
# 
# plot(q.irr.baseline)
# plot(q.irr.market)
# plot(diff.p)
# diff.p = mask(diff.p, states.sub)
# 
# png(file.path(plot.path, "Q_irr_percent_chg_MarketWet_minus_Baseline_yc.png"), res=100, width = 600, height=600)
# par(mar=c(5.1, 1.1, 2.1, 2.1))
# plot(states, col='grey90', border='white', xlim=c(-124.8, -102.1), ylim=c(31.3, 49), lwd=0.8)
# plot(states.sub, add=T, col='white')
# plot(diff.p, add=T, col=rev(brewer.pal(11, "RdBu")), legend=F)
# plot(wma.poly.sub, add=T, lwd=0.7, border='grey')
# plot(states.sub, add=T)
# plot(diff.p, col=rev(brewer.pal(11, "RdBu")), legend.only=TRUE, horizontal = TRUE, legend.args = list(text='% Change Irr water Re-use'))
# dev.off()


#######################################################################################################################################################################################
