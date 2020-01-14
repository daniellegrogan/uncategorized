# average winter temperature

library(raster)
library(lubridate)

load_files = function(path, varname, years){
  file.list.full = list.files(path = path, full.names = T, pattern=".nc")
  file.list = file.list.full[sapply(years, FUN = function(x) grep(pattern=x, file.list.full))]
  brk = do.call(stack,
                lapply(file.list, 
                       raster::brick, 
                       varname = varname))
  return(brk)
                  
}

####################################################################################################################################################
### US data only

path = "/net/nfs/swift/raid/data/prism/daily/tmean/"
years = seq(1981, 2010)
varname    = 'temperature'

# load data 
clim.data = load_files(path = path, 
                       varname = varname,
                       years   = years)


# read layer names
layer_names = names(clim.data)

# format layer names as dates
layer_dates = sub("X", "", layer_names)
layer_dates = sub("\\.", "-", layer_dates)
layer_dates = sub("\\.", "-", layer_dates)

# idenfity layers to keep
layers_keep = which(month(ymd(layer_dates)) %in% c(12, 1, 2))

# subset raster data
data.winter = subset(clim.data,  as.list(layers_keep))

# average temperature
mean.temp = calc(data.winter, fun = mean, na.rm=T)

writeRaster(mean.temp, "/net/nfs/squam/raid/userdata/dgrogan/US_winter_mean_temperature_1981-2010.nc",
            format="CDF", varname = "temperature", varunit = "C", longname = "mean winter temperature")

####################################################################################################################################################
### Global data (monthly)
path = "/net/nfs/yukon/raid5/data/NCEP/ncep_1deg_data/air.2m/monthly/"
varname = "air"
years = seq(1981, 2010)

# load data 
clim.data = load_files(path = path, 
                       varname = varname,
                       years   = years)

# read layer names
layer_names = names(clim.data)

# format layer names as dates
layer_dates = sub("X", "", layer_names)
layer_dates = sub("\\.", "-", layer_dates)
layer_dates = sub("\\.", "-", layer_dates)

layers_keep = which(month(ymd(layer_dates)) %in% c(12, 1, 2))

# subset raster data
data.winter = subset(clim.data,  as.list(layers_keep))

# average temperature
mean.temp = calc(data.winter, fun = mean, na.rm=T)
mean.temp = mean.temp -273.15 # convert K to C
plot(mean.temp)

writeRaster(mean.temp, "/net/nfs/squam/raid/userdata/dgrogan/Global_winter_mean_temperature_1981-2010.nc",
            format="CDF", varname = "temperature", varunit = "C", longname = "mean winter temperature", overwrite=T)



####################################################################################################################################################
# average winter SWE
source("~/git_repos/WBMr/wbm_load.R")
mod.data = wbm_load(path    = "/net/nfs/merrimack/raid/data/WBM_USNE/livneh/daily",
                    varname = "snowPack",
                    years   = seq(from = 1981, to = 2010))

layer_names = names(mod.data)

# format layer names as dates
layer_dates = sub("X", "", layer_names)
layer_dates = sub("\\.", "-", layer_dates)
layer_dates = sub("\\.", "-", layer_dates)

# idenfity layers to keep
layers_keep = which(month(ymd(layer_dates)) %in% c(12, 1, 2))

# subset raster data
data.winter = subset(mod.data,  as.list(layers_keep))

# average temperature
mean.swe = calc(data.winter, fun = mean, na.rm=T)

plot(mean.swe)

writeRaster(mean.swe, "/net/nfs/squam/raid/userdata/dgrogan/Winter_mean_SWE_1981-2010.nc",
            format="CDF", varname = "SWE", varunit = "mm", longname = "mean winter snow water equivalent from WBM")


