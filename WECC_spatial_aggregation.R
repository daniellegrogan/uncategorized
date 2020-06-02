# Spatial aggregation of WBM results for WECC irrigation "shock"
# Project: DOE 1.2
# Danielle S Grogan

library(raster)
library(rgdal)
library(rgeos)

source("/net/home/eos/dgrogan/git_repos/WBMr/wbm_load.R")
source("/net/home/eos/dgrogan/git_repos/WBMr/vars_from_init.R")
source("/net/home/eos/dgrogan/git_repos/WBMr/spatial_aggregation.R")


# load state shapefile for spatial aggregation
states = readOGR(dsn   = "/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_state_500k/", 
                 layer = "cb_2015_us_state_500k")

##################################################################################################################################


##################################################################################################################################


# spatial aggregation
wbm.path = "/net/nfs/squam/raid/data/DOE_PCHES/WBM_output/WECC_MERRA2_baseline/climatology/"
out.path = "/net/nfs/squam/raid/data/DOE_PCHES/WBM_state_agg/WECC_MERRA2_baseline"

shp.agg = function(wbm.path, var, shp, colnm, out.path, out.nm){
  wbm.clim = raster(paste(wbm.path, "wbm_", var, "_yc.nc", sep=""), varname = var)
  wbm.shp = spatial_aggregation(wbm.clim*365, shp)
  out = wbm.shp@data
  colnames(out)[10] = colnm
  write.csv(out, file.path(out.path, out.nm))
  print(file.path(out.path, out.nm))
}

#spatial aggregation of irrigationExtra (UGW)
shp.agg(wbm.path, 
        var = "irrigationExtra", 
        shp = states, 
        colnm = "UGW_km3_year", 
        out.path, 
        out.nm = "UGW_states.csv")

#spatial aggregation of irrigationGross (UGW)
shp.agg(wbm.path, 
        var = "irrigationGross", 
        shp = states, 
        colnm = "irrGross_km3_year", 
        out.path, 
        out.nm = "irrGross_states.csv")

# calcuate fraction of irr from UGW
ugw = read.csv(file.path(out.path, "UGW_states.csv"))
irr = read.csv(file.path(out.path, "irrGross_states.csv"))
ugw.frac = ugw$UGW_km3_year/irr$irrGross_km3_year
ugw.frac = cbind(as.character(ugw$NAME), ugw.frac)
ugw.frac = na.omit(ugw.frac)
write.csv(ugw.frac, file.path(out.path, "UGW_fraction_states.csv"))












###########################################################################################
agg_crop = function(wbm.path, var, states, years, out.path){
  wbm.data = wbm_load(file.path(wbm.path, var), varname = var, years)
  wbm.data.states = spatial_aggregation(wbm.data*365, states)
  wbm.data.states = na.exclude(wbm.data.states@data[,c(6,10:19)])
  colnames(wbm.data.states) = c("State", years)

  write.table(wbm.data.states, 
              paste(out.path, var, "_state_", min(years), "-", max(years), ".csv", sep=""),
              sep=",")
  print(var)
}
###########################################################################################
mean_stdev = function(data.table){
  # assumes the data.table has rows as states and columns as years.  First columns is state names.
  # output format: one row, two columns per state. Odd-columns (1,3,5,...) mean, even columns stdev across years
  
  data.mean  = rowMeans(data.table[,2:ncol(data.table)])
  data.stdev = apply(data.table[,2:ncol(data.table)], c(1), sd)
  state.names = as.character(data.table$State)
  
  data.out = append(data.mean, data.stdev)
  
  names(data.out) = c(paste(state.names, "mean", sep="_"), paste(state.names, "stdev", sep="_"))
  data.out
}
###########################################################################################

# list all output variables from WBM simulation
init.file = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/WECC_MERRA2_sd0/init_files/WECC_MERRA2_sd0.init"
vars = vars_from_init(init.file)

# identify crop vars: irrigation demand (net) and irrigation amount (net) by crop
irr.demand.vars = vars[which(grepl("_irrDemand", vars))]
irrigation.vars = vars[which(grepl("_irrigation", vars))]

#spatial aggregation of irrigationExtra (UGW)
# var = "irrigationExtra"
# 
# ugw = wbm_load(file.path(wbm.path, var), varname = var)
# ugw.states = spatial_aggregation(ugw*365, states)
# write.csv(ugw.states@data, file.path(out.path, "UGW_states.csv"))

var = "irrigationGross"
irr.gross= wbm_load(file.path(wbm.path, var), varname = var)
irr.gross.states = spatial_aggregation(irr.gross*365, states)
write.csv(irr.gross.states@data, file.path(out.path, "IrrGross_states.csv"))

ugw.frac = ugw.states@data[,10:25]/irr.gross.states@data[,10:25]


var = "irrDemandTotal"
irr.demand= wbm_load(file.path(wbm.path, var), varname = var)
irr.demand.states = spatial_aggregation(irr.demand*365, states)


diff = irr.demand.states$layer.16 - irr.gross.states$layer.16
diff.frac = diff/irr.demand.states$layer.16

years = seq(2006, 2015)
lapply(irr.demand.vars, FUN = function(x) agg_crop(wbm.path, x, states, years, out.path))
lapply(irrigation.vars, FUN = function(x) agg_crop(wbm.path, x, states, years, out.path))

# calculate irrigation water shortage as a fraction
file.list = list.files(path = out.path, full.names=T)
crops = strsplit(irr.demand.vars, "_irrDemand")

for(i in 1:length(crops)){
  crop.files = file.list[which(grepl(crops[i], file.list))]
  irr.demand = read.csv(crop.files[1])
  irrigation = read.csv(crop.files[2])

  frac.deficit = (irr.demand[,2:11] - irrigation[,2:11])/irr.demand[,2:11]
  frac.deficit = cbind(irr.demand[,1], frac.deficit)
  colnames(frac.deficit)[1] = "State"
  stat.row = mean_stdev(frac.deficit)
  
  if(i == 1){
    stats.out = t(as.data.frame(stat.row))
    rownames(stats.out)[i] = crops[i]
  }else{
    stats.out = rbind(stats.out, stat.row)
    rownames(stats.out)[i] = crops[i]
  }
  stats.out = signif(stats.out, 2)
  print(crops[i])
}

write.table(stats.out, file.path(out.path, "Irrigation_deficit_stats_2006_2015.csv"), sep=",")


###########################################################################################
### County aggregations

###########################################################################################
agg_crop.c = function(wbm.path, var, counties, state.fp.list, years, out.path){
  wbm.data = wbm_load(file.path(wbm.path, var), varname = var, years)
  wbm.data.counties = spatial_aggregation(wbm.data*365, counties)
  wbm.data.counties = wbm.data.counties@data[,c(1,6,10:19)]
  wbm.data.counties = subset(wbm.data.counties, 
                             as.numeric(as.character(wbm.data.counties$STATEFP)) %in% as.numeric(as.character(state.fp.list[,1]))) # subset to states of interest
  colnames(wbm.data.counties) = c("STATEFP", "County", years)
  
  write.table(wbm.data.counties, 
              paste(out.path, var, "_counties_", min(years), "-", max(years), ".csv", sep=""),
              sep=",")
  print(var)
}
###########################################################################################

counties = readOGR(dsn   = "/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_county_500k/", 
                   layer = "cb_2015_us_county_500k")

wbm.path = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/WECC_MERRA2_sd0/yearly"
out.path = "/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/v5/NoUGW/"

years = seq(2006, 2015)

state.fps = states@data[,c(1,6)]
state.fp.list = subset(state.fps, state.fps$NAME %in% c("Washington", "Oregon", "Idaho", "Arizona", "California", "Nevada", "Utah"))

lapply(irr.demand.vars, FUN = function(x) agg_crop.c(wbm.path, x, counties, state.fp.list, years, out.path))
lapply(irrigation.vars, FUN = function(x) agg_crop.c(wbm.path, x, counties, state.fp.list, years, out.path))

################################################################################################################################################################################################
# sandbox

maize_irrigation = read.csv(file.path(out.path, "Maize_1_irrigation_counties_2006-2015.csv"))
maize_demand = read.csv(file.path(out.path, "Maize_1_irrDemand_counties_2006-2015.csv"))

diff = maize_demand[,3:12] - maize_irrigation[,3:12]
fraction = diff/maize_demand[,3:12]


grossIrr = raster("/net/nfs/squam/raid/data/WBM_TrANS/DOE/WECC_MERRA2_NoUGW/yearly/irrigationGross/wbm_2006.nc")
irrDemand = raster("/net/nfs/squam/raid/data/WBM_TrANS/DOE/WECC_MERRA2_NoUGW/yearly/irrDemandTotal/wbm_2006.nc")

plot(grossIrr)
plot(irrDemand)
diff = irrDemand - grossIrr
plot(diff)

diff.p = diff/irrDemand
plot(diff.p)

plot(diff*(diff < 0))


#### compare test runs
irrDemand_noUGW = raster("/net/nfs/bog/raid/data/WBM_TrANS/DOE/WECC_NoUGW_test/yearly/irrDemandTotal/wbm_2006.nc")
irrGross_noUGW  = raster("/net/nfs/bog/raid/data/WBM_TrANS/DOE/WECC_NoUGW_test/yearly/irrigationGross/wbm_2006.nc")


irrDemand_ysUGW = raster("/net/nfs/bog/raid/data/WBM_TrANS/DOE/WECC_YesUGW_IBT_test/yearly/irrDemandTotal/wbm_2006.nc")
irrGross_ysUGW  = raster("/net/nfs/bog/raid/data/WBM_TrANS/DOE/WECC_YesUGW_IBT_test/yearly/irrigationGross/wbm_2006.nc")
irrExtra_ysUGW  = raster("/net/nfs/bog/raid/data/WBM_TrANS/DOE/WECC_YesUGW_IBT_test/yearly/irrigationExtra/wbm_2006.nc")

diff = irrDemand_ysUGW - irrDemand_noUGW
diff.gross = irrGross_ysUGW - irrGross_noUGW


# old wbm results:
# path to WBM results
old_irrGross = raster("/net/nfs/bog/raid/data/dgrogan/Irr_return_flow/WBM_results/30min/yearly/global_return_012016_merra_irrigationGross_2006.nc")
old_irrGross = crop(old_irrGross, extent(irrGross_ysUGW))

old_irrExtra = raster("/net/nfs/bog/raid/data/dgrogan/Irr_return_flow/WBM_results/30min/yearly/global_return_012016_merra_irrigationExtra_2006.nc")
old_irrExtra = crop(old_irrExtra, extent(irrGross_ysUGW))

irrGross_ysUGW = 365*irrGross_ysUGW
irrExtra_ysUGW = 365*irrExtra_ysUGW

states = readOGR(dsn   = "/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_state_500k/", 
                 layer = "cb_2015_us_state_500k")
states = states[states$NAME %in% c("Washington", "Oregon", "Idaho", "Arizona", "California", "Nevada", "Utah"), ]


state_old_irrGross = spatial_aggregation(old_irrGross, states, s=1, poly.out = F)
state_new_irrGross = spatial_aggregation(irrGross_ysUGW, states, s=1, poly.out = F)

state_old_irrExtra = spatial_aggregation(old_irrExtra, states, s=1, poly.out = F)
state_new_irrExtra = spatial_aggregation(irrExtra_ysUGW, states, s=1, poly.out = F)


comp = cbind(state_old_irrExtra, state_new_irrExtra)
colnames(comp) = c("old", "new")
rownames(comp) = states$NAME


# IBT vs no IBT
irrExtra_ysUGW            = raster("/net/nfs/bog/raid/data/WBM_TrANS/DOE/WECC_YesUGW_test/yearly/irrigationExtra/wbm_2006.nc")*365
irrExtra_ysUGW_IBT        = raster("/net/nfs/bog/raid/data/WBM_TrANS/DOE/WECC_YesUGW_IBT_test/yearly/irrigationExtra/wbm_2006.nc")*365
irrExtra_ysUGW_IBT_noVIS  = raster("/net/nfs/bog/raid/data/WBM_TrANS/DOE/WECC_YesUGW_noVIS_test/yearly/irrigationExtra/wbm_2006.nc")*365

state_irrExtra_ysUGW  = spatial_aggregation(irrExtra_ysUGW, states, s=1, poly.out = F)
state_irrExtra_ysUGW_IBT = spatial_aggregation(irrExtra_ysUGW_IBT, states, s=1, poly.out = F)
state_irrExtra_ysUGW_IBT_noVIS = spatial_aggregation(irrExtra_ysUGW_IBT_noVIS, states, s=1, poly.out = F)

comp = cbind(state_irrExtra_ysUGW, state_irrExtra_ysUGW_IBT, state_irrExtra_ysUGW_IBT_noVIS)

diff.1 = comp[,1] - comp[,2]
diff.1.frac = diff.1/comp[,1]

diff.2 = comp[,1] - comp[,3]
diff.2.frac = diff.2/comp[,1]

comp = cbind(comp, diff.1, diff.1.frac, diff.2, diff.2.frac)

colnames(comp) = c("no_IBT", "with_IBT", "with_IBT_noVIS", "difference_AB", "diff_AB_frac", "difference_AC", "diff_AC_frac")
rownames(comp) = states$NAME

write.csv(comp, "/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_UGW_comparison.csv")
