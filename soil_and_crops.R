library(raster)
library(rgdal)
library(rgeos)
library(viridis)
library(foreign)

rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory

# states
states = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_state_500k/")


# cdl x MIRCA cropland data
spr.wheat.irr = raster("/net/nfs/yukon/raid0/data/CDL/National/CDL_MC_crop_frac/2017/IrrCrop1_sub2.tif")
win.wheat.irr = raster("/net/nfs/yukon/raid0/data/CDL/National/CDL_MC_crop_frac/2017/IrrCrop1_sub1.tif")

spr.wheat.rf = raster("/net/nfs/yukon/raid0/data/CDL/National/CDL_MC_crop_frac/2017/RfdCrop27_sub2.tif")
win.wheat.rf = raster("/net/nfs/yukon/raid0/data/CDL/National/CDL_MC_crop_frac/2017/RfdCrop27_sub1.tif")

spr.wheat.tot = (spr.wheat.irr + spr.wheat.rf)

png("/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/Spring_wheat_map.png", 
    units = "in", height=4, width=6, res=300)
plot(spr.wheat.tot)
plot(states, add=T)
dev.off()

win.wheat.tot = (win.wheat.irr + win.wheat.rf)
png("/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/Winter_wheat_map.png", 
    units = "in", height=4, width=6, res=300)
plot(win.wheat.tot)
plot(states, add=T)
dev.off()


######################################################################################################################
# gSSURGO data

# note: column names and other metadata are from here: https://data.nal.usda.gov/system/files/SSURGO_Metadata_-_Table_Column_Descriptions.pdf

ssurgo.crop = read.delim("/net/nfs/swift/raid/userdata/shan/SmallStreams/Soil/gssurgo/tabular/ccrpyd.txt", sep="|", header=F)
ssurgo.soil = read.delim("/net/nfs/swift/raid/userdata/shan/SmallStreams/Soil/gssurgo/tabular/chunifie.txt", sep="|", header=F)

# need to use the component table to link crops and soils to the mapunit table
ssurgo.comp = read.delim("/net/nfs/swift/raid/userdata/shan/SmallStreams/Soil/gssurgo/tabular/comp.txt", sep="|", header=F)
colnames(ssurgo.comp) = c("comppct_l", "comppct_r", "comppct_h",
                          "compname", "compkind", "majcompflag", "otherph", "localphase",
                          "slope_l", "slope_r", "slope_h",
                          "slopelenusle_l", "slopelenusle_r", "slopelenusle_h",
                          "runoff", "tfact", "wei", "weg", "erocl", "earthcovkind1", "earthcovkind2", "hydricon", "hydricrating", "drainagecl",
                          "elev_l", "elev_r", "elev_h", "aspectccwise", "aspectrep", "aspectcwise", "geomdesc", 
                          "albedodry_l", "albedodry_r", "albedodry_h", 
                          "airtempa_l", "airtempa_r", "airtempa_h",
                          "map_l", "map_r", "map_h", "reannualprecip_l", "reannualprecip_r", "reannualprecip_h",
                          "ffd_l", "ffd_r", "ffd_h", 
                          "nirrcapcl", "nirrcapscl", "nirrcapunit", "irrcapcl", "irrcapscl", "irrcapunit", "cropprodindex", "cropprodindex", "wndbrksuitgrp",
                          "rsprod_l", "rsprod_r", "rsprod_h",
                          "foragesuitgrpid", "wlgrain", "wlgrass", "wlherbaceous", "wlshrub", "wlconiferous", "wlhardwood", "wlwetplant", 
                          "wlshallowwat", "wlrangeland", "wlopenland", "wlwoodland", "wlwetland", "soilslippot", "frostact",
                          "initsub_l", "initsub_r", "initsub_h", "totalsub_l", "totalsub_r", "totalsub_h", "hydgrp", 
                          "corcon", "corsteel", 
                          "taxclname", "taxorder", "taxsuborder", "taxgrtgroup", "taxsubgrp", "taxpartsize", "taxpartsizemod", "taxceactcl", "taxreaction", "taxtempcl", "taxmoistscl", 
                          "taxtempregime", "taxtempregime", "castorieindex", "flecolcomnum", "flhe", "flphe", "flsoilleachpot", "flsoirunoffpot", "fltemik2use", "fltriumph2use", 
                          "indraingrp", "innitrateleachi", "misoimgmtgrp", "vasoimgtgrp", 
                          "mukey", 
                          "cokey")

mapunit = read.delim("/net/nfs/swift/raid/userdata/shan/SmallStreams/Soil/gssurgo/tabular/mapunit.txt", sep="|", header=F)
colnames(mapunit) = c("interpfocus", 
                      "invesintens",
                      "iacornsr",
                      "nhiforsoigrp",
                      "nhspiagr",
                      "vtsepticsyscl", 
                      "mucertstat", 
                      "lkey", 
                      "mukey")         # Mapunit Key

# subset crops to just wheat
ssurgo.wheat = subset(ssurgo.crop, grepl("wheat", ssurgo.crop[,1]))
ssurgo.wheat = subset(ssurgo.wheat, !grepl("grass", ssurgo.wheat[,1]))
colnames(ssurgo.wheat) = c("cropname", "yldunits", 
                          "nonirryield_l", "nonirryield_r", "nonirryield_h", 
                          "irryield_l",       "irryield_r",    "irryield_h",
                          "cropprodindex", "vasoiprdgrp", "cokey", "cocropyldkey")  # cokey links this table tot he component table

# join component table and mapunit
map.comp = merge(ssurgo.comp, mapunit, "mukey", all=T)

# join map.comp with wheat data
wheat.join = merge(ssurgo.wheat, map.comp, by="cokey", all.x=T)
spr.wheat.join = subset(wheat.join, grepl("Spring", wheat.join$cropname))
wnt.wheat.join = subset(wheat.join, grepl("Winter", wheat.join$cropname))

# map data shapefile
map = readOGR("/net/nfs/merrimack/raid2/data/SSURGO", "gsmsoilmu_a_us")
map = crop(map, extent(spr.wheat.irr))

map.wheat = map[map$MUKEY %in% wheat.join$mukey,] # subset shapefile to mukeys with wheat

map.spr.wheat = map[map$MUKEY %in% spr.wheat.join$mukey,] # subset shapefile to mukeys with spring wheat
map.wnt.wheat = map[map$MUKEY %in% wnt.wheat.join$mukey,] # subset shapefile to mukeys with winter wheat

png("/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/Spring_wheat_map_SSURGO.png", 
    units = "in", height=8, width=11, res=800)
plot(map.spr.wheat, col='bisque', border='bisque4', lwd=0.15, xlim=c(-124, -62), ylim=c(22, 52))
plot(states, lwd=0.6, add=T)
dev.off()

png("/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/Winter_wheat_map_SSURGO.png", 
    units = "in", height=8, width=11, res=800)
plot(map.wnt.wheat, col='cadetblue1', border='cadetblue4', lwd=0.15, xlim=c(-124, -62), ylim=c(22, 52))
plot(states, lwd=0.6, add=T)
dev.off()


# identify soil properties of interest
spr.wheat.sub = subset(spr.wheat.join, select=c(
  "mukey", "cropname",
  "majcompflag", # identifies if component is the major component in the mapunit
  "drainagecl",  # drainage class
  "taxclname",   # A concatenation of the Soil Taxonomy subgroup and family for a soil (long name).
  "taxorder",    # The highest level in Soil Taxonomy.
  "taxsuborder", # The second level of Soil Taxonomy. The suborder is below the order and above the great group.
  "taxgrtgroup", # The third level of Soil Taxonomy. The category is below the suborder and above the subgroup.
  "taxsubgrp",   # The fourth level of Soil Taxonomy. The subgroup is below great group and above family.
  "taxmoistscl", # Soil moisture subclasses are taxonomic subgroup criteria, whether included or not in the name of the subgroup. The definition of each subclass is dependent upon the specific taxonomic great group to which it is attached.
  "taxpartsize"  # Particle Size
))

# subset to only components that are the major component in the map unit
spr.wheat.sub = subset(spr.wheat.sub, spr.wheat.sub$majcompflag == "Yes")

write.csv(spr.wheat.sub, "/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/SSURGO_spring_wheat_soil_data.csv", row.names = F)
