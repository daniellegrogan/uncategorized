# HiMAT: make aquifer IDs based on basin IDs
# Danielle S Grogan
# 2019-11-06

library(raster)
library(rgdal)
library(rgeos)

basins = readOGR("/net/nfs/squam/raid/userdata/dgrogan/HiMAT/basins_hma/", "basins_hma")

aquifers = raster("/net/nfs/yukon/raid0/data/WBM_data/defaults/Aquifers/Aqf_Thick/estimated_aqThick_05min.map")

aquifers = mask(aquifers,basins)
aquifers = crop(aquifers, extent(basins))

aquifers[aquifers == 0] = NA
aquifers[aquifers > 0] = 1


basins.raster = rasterize(basins, aquifers, field="Basin_ID")
aquifer.id = aquifers * basins.raster

writeRaster(aquifer.id, filename = "/net/nfs/yukon/raid0/data/WBM_data/HiMAT/Aquifers/Aquifer_IDs.tif",
            format = "GTiff")
