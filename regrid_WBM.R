# Calcuate % irrigation water from unsustainable groundwater
# Re-grid from 6 min to 5 min spatial resolution

# Project: PCHES 1.1
# 2020-05-26

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)


wbm.path = "/net/nfs/squam/raid/data/DOE_PCHES/WBM_output/WECC_MERRA2_baseline/climatology/" 
irr = raster(file.path(wbm.path, "wbm_irrigationGross_yc.nc"))
ugw = raster(file.path(wbm.path, "wbm_irrigationExtra_yc.nc"))
frac.ugw = ugw/irr

### make a raster: global, 5 min
r = raster(nr = 180*12, nc= 360*12, res = (1/12))

# crop to extent of WBM output
grid.5 = crop(r, extent(frac.ugw))

# resample
frac.ugw.5 = resample(frac.ugw, grid.5, method="bilinear")

# write output
writeRaster(frac.ugw.5, "/net/nfs/squam/raid/userdata/dgrogan/DOE_1.1/WBM_UnsustGW_frac.nc", 
            format="CDF", varname = "UGW_fraction", varunit = "fraction")
