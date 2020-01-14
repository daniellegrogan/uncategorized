# Spatial aggregation of WBM results for WECC irrigation "shock"
# Project: DOE 1.2
# Danielle S Grogan

library(raster)
library(rgdal)
library(rgeos)

source("/net/home/eos/dgrogan/git_repos/WBMr/wbm_load.R")
source("/net/home/eos/dgrogan/git_repos/WBMr/vars_from_init.R")
source("/net/home/eos/dgrogan/git_repos/WBMr/spatial_aggregation.R")

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
init.file = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/WECC_MERRA2_NoUGW/init_files/WECC_MERRA2_NoUGW.init"
vars = vars_from_init(init.file)

# identify crop vars: irrigation demand (net) and irrigation amount (net) by crop
irr.demand.vars = vars[which(grepl("_irrDemand", vars))]
irrigation.vars = vars[which(grepl("_irrigation", vars))]

# load state shapefile for spatial aggregation
states = readOGR(dsn   = "/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_state_500k/", 
                 layer = "cb_2015_us_state_500k")

# spatial aggregation
wbm.path = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/WECC_MERRA2_NoUGW/yearly"
out.path = "/net/nfs/squam/raid/userdata/dgrogan/DOE_1.2/WECC_irr/v3/"

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
