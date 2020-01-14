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
              paste(out.path, var, "_state_", min(years), "-", max(years), ".csv", sep=""))
  print(var)
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


