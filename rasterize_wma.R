# rasterize WMA shapefile for WECC states, v0
# Project: DOE PCHES 1.2

library(raster)
library(rgdal)
library(rgeos)

# use river network to define spatial resolution
netwk = raster("/net/nfs/zero/home/WBM_TrANS/data/WECC_6min_v3b.asc")
wma.shp = readOGR("/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/simpleWMAs/", "simpleWMAs")
wma.shp$ID = seq(1,976)

wma.raster = raster::rasterize(wma.shp, netwk, field = "ID")
writeRaster(wma.raster, "/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/simpleWMAs_raster.tif", format="GTiff")

test = raster("/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/simpleWMAs_raster.tif")
writeOGR(wma.shp, "/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/simpleWMAs_ID/", "simpleWMAs_ID", driver="ESRI Shapefile")

###########################################################################################################################
# rename all water rights files to use the unique number ids "ID"

wma.shp.id = readOGR("/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/simpleWMAs_ID/", "simpleWMAs_ID")

in.path  = "/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/sectorCumuSummaries"
out.path = "/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/water_rights_ID"
# states = c("idaho", "washington", "oregon", "colorado", "arizona", "california")
states = c("montana", "nevada", "newMexico", "utah", "wyoming")

scen = c("unmodified", "marketModified_stps_nonL_wInd")
src  = c("surfaceWater", "groundWater")

for(s in scen){
  for(w in src){
    for(t in states){
      if(s == "unmodified"){
        f.list = list.files(file.path(in.path, t, w, "percentages", s))
      }else{
        f.list = list.files(file.path(in.path, t, w, "percentages", s, "dryScenerio"))
      }
      
      
      for(f in f.list){
        match = which(wma.shp.id$uniID == sub(".csv", "", f))
        if(length(match)>0){
          new.id = wma.shp.id$ID[match]
          if(length(new.id)>0){ new.id = new.id[1]}
          if(w == "surfaceWater"){
            W = "SW"
          }else{
            W = "GW"
          }
          new.nm = paste("WMA_", new.id, "_", W, ".csv", sep="")
          
          if(s == "unmodified"){
            to.path   = file.path(out.path, s)
            from.path = file.path(in.path, t, w, "percentages", s, f)
          }else{
            to.path = file.path(out.path, s, "dryScenario")
            from.path = file.path(in.path, t, w, "percentages", s, "dryScenerio", f)
          }
          file.copy(from = from.path, to = to.path)
          file.rename(from = file.path(to.path, f), to = file.path(to.path, new.nm))
        }
      }
    }
  }
}


# copy names to donor reservoir mapping file as well

library(tidyr)
library(readr)  # use this to define columns as characters when reading
res.file = read_tsv("/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/DonorReservoirMapping.txt", 
                    col_types = list(col_character(), col_character(), col_character()))

#new.res.id = mat.or.vec(nr=nrow(res.file), nc=1)
new.res.file = res.file
for(i in 1:length(res.file$BasinNumber)){
  b = gsub("[[:space:]]", "", as.character(res.file$BasinNumber[i]))
  match = which(wma.shp.id$uniID == b)[1]
  new.id = wma.shp.id$ID[match]
  #new.res.id[i] = paste("WMA_", new.id, sep="")
  new.res.file[i,1] = paste("WMA_", new.id, sep="")
}

new.res.file = cbind(new.res.id, as.character(res.file[,2:3]))
#colnames(new.res.file)[1] = colnames(res.file)[1]
write.table(new.res.file, "/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/DonorReservoirMapping_ID.csv", quote=F, row.names = F, sep="\t")


# modify california groundwater rights
ca.shape.id = wma.shp.id[wma.shp.id$state == "California",]
#gw.files = wr.files[grepl("GW", c(wr.files))]
for(i in ca.shape.id$ID){
  gw = paste("/net/nfs/zero/data3/WBM_TrANS/data/WECC_WMA/water_rights_ID/marketModified_stps_nonL_wInd/dryScenario/WMA_", i, "_GW.csv", sep="")
  if(file.exists(gw)){
    gw.file = read.csv(gw)
    gw.file$CUML = rep(7000000, nrow(gw.file))
    gw.file[,3:9] = rep(1/7, nrow(gw.file))
    write.csv(gw.file, gw, row.names = F, quote = F)
  }
}




