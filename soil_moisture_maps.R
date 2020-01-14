# soil moisture maps for PCHES Nov. 2019 brainstorming session

library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)

us = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_state_500k/", "cb_2015_us_state_500k")
plot(us)
us = crop(us, extent(soilm))

soilm.2015 = brick("/net/nfs/squam/raid/userdata/dgrogan/wbm/DOE_1.1/US_CDL_v3/daily/wbm_2015.nc", varname="soilMoist")
us = crop(us, extent(soilm.2015))
soilm.2015.May1  = subset(soilm.2015, 121)
soilm.2015.May1  = mask(soilm.2015.May1, us)

soilm.2012 = brick("/net/nfs/squam/raid/userdata/dgrogan/wbm/DOE_1.1/US_CDL_v3/daily/wbm_2012.nc", varname="soilMoist")
soilm.2012.May1 = subset(soilm.2012, 121)
soilm.2012.May1  = mask(soilm.2012.May1, us)

soil.diff = soilm.2015.May1 - soilm.2012.May1
rdb = brewer.pal(n = 11, name = "RdYlBu")
plot(soil.diff, col = rdb)

png("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.1/Figures/soilmoisture_Diff_2015-05-01_2012-05-01.png", res=100, height=500, width=1000)
plot(soil.diff, col = rdb, bty='n', axes=F, box=FALSE, ylim=c(25,50))
plot(us, add=T, lwd=0.6)
dev.off()

png("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.1/Figures/soilmoisture_2012-05-01.png", res=100, height=500, width=1000)
plot(soilm.2012.May1, bty='n', axes=F, box=FALSE, ylim=c(25,50))
plot(us, add=T, lwd=0.6)
dev.off()

png("/net/nfs/squam/raid/userdata/dgrogan/DOE_1.1/Figures/soilmoisture_2015-05-01.png", res=100, height=500, width=1000)
plot(soilm.2015.May1, bty='n', axes=F, box=FALSE, ylim=c(25,50))
plot(us, add=T, lwd=0.6)
dev.off()


