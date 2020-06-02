# center of volume: discharge from WBM
# INFEWS-Purdue
# Shan Zuidema, Danielle Grogan
# 2020-05-12

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
source("/net/home/eos/dgrogan/git_repos/WBMr/wbm_load.R")

#######################################################################################################
cdf = function(x){
  cs = cumsum(x)
  cs.frac = (cs/max(cs))
  cs.frac[is.nan(cs.frac)] = 0
  cs.frac
}
#######################################################################################################
cov = function(x, na.rm=T){
  dist = cdf(x)
  
  # idenfity value(s) closest to 0.5
  cdf_diff = abs(dist - 0.5)
  
  # find smallest value in cdf_diff
  cdf_diff_min = min(cdf_diff, na.rm=T)
  
  # idenfity the row of the minimum difference
  doy_cov = which(cdf_diff == cdf_diff_min)[1]
  
  doy_cov
}
#######################################################################################################
cov_doy_grid = function(path, 
                        varname = 'discharge',
                        year,
                        out.path
){
  
  q = wbm_load(path, varname, years = year)

  # calculate COV DOY
  q.cov = stackApply(x = q, 
                     indices = rep(1, 365), 
                     fun = cov, 
                     na.rm = T)
  
  writeRaster(q.cov, paste(out.path, "/wbm_", varname, "_cov_", year, ".nc", sep = ""), format = "CDF", 
              overwrite = T, varname = "DOY", varunit = "Julian_day")
} # end function
#######################################################################################################

year.list = seq(1997, 2007)
for(year in year.list){
  cov_doy_grid(path = "/net/nfs/saco/raid2/data/infewsgl/output/MSag_v1z/daily/", 
               varname = 'discharge',
               year = year,
               out.path = "/net/nfs/saco/raid2/data/infewsgl/proc/NremovalValue/discharge_cov"
  )
  print(year)
}

