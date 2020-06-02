# Make mm/month climatology files from WBM output for FAO GLEAM v2

# Danielle S Grogan
# 2020-05-15

##################################################################################################################
### LIBRARIES AND SOURCE FILES ###

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

source("~/git_repos/WBMr/wbm_load.R")
source("~/git_repos/WBMr/nc_vars_meta.R")
source("~/git_repos/WBMr/vars_from_init.R")

##################################################################################################################
# Function: check unit in netcdf file, then output a vector of scaling factors (either days in months, or all 1's if already mm/month)
set_mm_month = function(filepath){
  data_unit = nc_vars_meta(filepath)$units[3]
  if(data_unit == "mm/day"){
    return(month.days$days)
  }else if(data_unit == "mm/month"){
    return(rep(1, 12))
  }else{
    print(data_unit)
  }
}
##################################################################################################################
# path to input
path = "/net/nfs/squam/raid/data/GLEAM/WBM/GLEAM_v2"

### Days per month
month.days = read.csv("/net/home/eos/dgrogan/git_repos/WBMr/data/days_in_months.csv")

### list all output variables
var.list = vars_from_init(file.path(path, "init_files/GLEAM_v2.init"))
var.list = var.list[-which(var.list == "discharge")]  # remove discharge, as this variable can remain in units of m3/s

# variables to modify: need to subtract addedWater from these vars
mod.vars = c("irrigationGross", "irrigationNet")
scale_unit = set_mm_month(file.path(path, "/climatology/wbm_addedWater_mc.nc"))
mod.apply = brick(file.path(path, "/climatology/wbm_addedWater_mc.nc"), varname = "addedWater")*scale_unit

mc.files = list.files(file.path(path, "climatology"), pattern = "_mc", full.names = T)
# mc.files = mc.files[70:133]
for(i in 1:length(mc.files)){
  
  # path to data
  filepath = mc.files[i]
  
  # get variable name
  varname = nc_vars_meta(filepath)$name[2]
  
  # check if output file exists
  if(!file.exists(sub("climatology", "climatology_mm_month", mc.files[i]))){
    # get scale for unit: 1 for mm/month, # days in each month for mm/day, report unit if other
    scale_unit = set_mm_month(filepath)
    
    # load data, and multiply by scale unit to get mm/month
    var_data = brick(filepath)*scale_unit
  
    # if it is a mod.var, then subtract the mod.apply values
    if(varname %in% mod.vars){
      var_data = var_data - mod.apply
    }
    
    writeRaster(var_data, paste(path, "/climatology_mm_month/wbm_", varname, "_mc.nc", sep=""), format="CDF",
                varname = varname, varunit = "mm/month", zname = "time", zunit="monthly_clim", overwrite=T)
  }
 
  print(varname)
}


for(i in 1:length(mc.files)){
  
  # path to data
  filepath = mc.files[i]
  
  # get variable name
  varname = nc_vars_meta(filepath)$name[2]
  
  data_unit = nc_vars_meta(filepath)$units[3]
  
  if(data_unit != "mm/day"){
    print(paste(varname, data_unit))
    
  }
}


