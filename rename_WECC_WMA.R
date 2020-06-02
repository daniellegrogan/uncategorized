# revert rasterized WMA names to state WMA names
# Project: DOE PCHES 1.2

library(raster)
library(rgdal)
library(rgeos)

source("~/git_repos/WBMr/create_dir.R")

rename_wma = function(id.map, wma.path, out.dir){
  create_dir(out.dir)
  in.files = list.files(wma.path, full.names=F, pattern="wma")
  in.files.full = list.files(wma.path, full.names=T, pattern="wma")
  in.nm = sub(".csv", "", in.files)
  in.nm = sub("wma_", "", in.nm)
  for(i in 1:length(in.nm)){
    row.match = which(id.map[,2] == in.nm[i])
    new.nm = paste(as.character(id.map[row.match,1]), ".csv", sep="")
    file.copy(in.files.full[i], file.path(out.dir, new.nm))
  }
}


wma.shp.id = readOGR("/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/simpleWMAs_ID/", "simpleWMAs_ID")
p.baseline = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/WatInst_baseline_v0/WMA"
p.marketWt = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/WatInst_marketModified_stps_nonL_wInd_wetScenario_v0/WMA"

# rewrite baseline
rename_wma(id.map = subset(wma.shp.id@data, select=c("uniID", "ID")),
           wma.path = p.baseline,
           out.dir = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/WatInst_baseline_v0/WMA_uniID")

# rewrite market scenario
rename_wma(id.map = subset(wma.shp.id@data, select=c("uniID", "ID")),
           wma.path = p.marketWt,
           out.dir = "/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/WatInst_marketModified_stps_nonL_wInd_wetScenario_v0/WMA_uniID")


####

dams = read.delim("/net/nfs/zero/home/WBM_TrANS/spreadsheets/GRanD_dams_v1_1.csv")
dams.nv = subset(dams, dams$ADMIN_UNIT == "Nevada")
dams.tailings = subset(dams.nv, grepl("tailings", as.character(dams.nv$DAM_NAME), ignore.case = T))
write.csv(dams.tailings, "/net/nfs/squam/raid/userdata/dgrogan/GRanD_NV_Mine_Tailings_Dams.csv", row.names=F)
