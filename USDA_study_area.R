# Map river networks for USDA 2020 proposal

library(raster)
library(rgdal)
library(rgeos)

source("~/git_repos/WBMr/Flowlines.R")
source("~/git_repos/WBMr/mouth_ts.R")

# load network ids
network = raster("/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/MinnMetro.asc")
network.id = raster("/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/Minneapolis_upstream_v1_IDs.asc")

crs(network.id) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
network.id.poly = rasterToPolygons(network.id, dissolve=T)

# make flowlines
# flowlines(flowdir = network, 
#           basinID=network.id, 
#           uparea=raster("/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/MNRiver_v1_upstrArea.asc"), 
#           region=NA, 
#           out.loc = "/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020", 
#           out.name = "MNRiver_v1_flowlines")

# read in flowlines, plot
netwk.flow = readOGR("/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/Minneapolis_upstream_v1_flowlines/", "Minneapolis_upstream_v1_flowlines")
# plot(netwk.mn.flow, add=T, col='blue')

# identify downstream point
up.area=raster("/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/Minneapolis_upstream_v1_upstrArea.asc")
mouth = id_mouth(network.id, 1, up.area)
# plot(mouth.mn, pch=20, col='red', add=T)


up.area.b = up.area > 10000
up.area.b[up.area.b==0] = NA

pts.mouth = data.frame(matrix(nr=5, nc=3))


plot(netwk.flow)
plot(up.area.b, add=T, legend=F, col='blue')
plot(mouth, add=T, pch=19, col='black')


# characterize network 
n.cells = sum(as.matrix(network > -1), na.rm=T)
cell.sz = raster::area(network)
total.area = sum(as.matrix(cell.sz), na.rm=T)

test = mask(cell.sz, metro.counties)

# minnesota
minn.city = as.data.frame(cbind(-93.2650, 44.9778))
colnames(minn.city) = c("lon", "lat")
coordinates(minn.city) <- ~ lon + lat

# counties
counties = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_county_500k/", "cb_2015_us_county_500k")
counties.MN = counties[counties$STATEFP == 27, ]
metro.counties = counties.MN[counties.MN$NAME == "Anoka" |
                               counties.MN$NAME == "Hennepin" |
                               counties.MN$NAME == "Carver" |
                               counties.MN$NAME == "Scott" |
                               counties.MN$NAME == "Dakota" |
                               counties.MN$NAME == "Washington" |
                               counties.MN$NAME == "Ramsey",]
metro.counties = spTransform(metro.counties, "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")

# ag area

# wheat
spr.wheat.irr = raster("/net/nfs/yukon/raid0/data/CDL/National/CDL_MC_crop_frac/2017/IrrCrop1_sub2.tif")
win.wheat.irr = raster("/net/nfs/yukon/raid0/data/CDL/National/CDL_MC_crop_frac/2017/IrrCrop1_sub1.tif")
spr.wheat.rf = raster("/net/nfs/yukon/raid0/data/CDL/National/CDL_MC_crop_frac/2017/RfdCrop27_sub2.tif")
win.wheat.rf = raster("/net/nfs/yukon/raid0/data/CDL/National/CDL_MC_crop_frac/2017/RfdCrop27_sub1.tif")
spr.wheat.tot = (spr.wheat.irr + spr.wheat.rf)
win.wheat.tot = (win.wheat.irr + win.wheat.rf)
wheat.tot = (spr.wheat.tot + win.wheat.tot)

wheat.tot = mask(wheat.tot, network.id.poly)
wheat.tot = crop(wheat.tot, extent(network.id.poly))
wheat.tot[wheat.tot==0] = NA

wheat.tot = mask(wheat.tot, metro.counties)
wheat.tot = crop(wheat.tot, extent(metro.counties))
wheat.tot[wheat.tot==0] = NA


# all crops
cropland = brick("/net/nfs/yukon/raid0/data/CDL/National/CDL_crop_frac/cdl_cropland_US.nc")
cropland = subset(cropland, 8)
#cropland = crop(cropland, extent(network.id.poly))
#cropland = mask(cropland, network.id.poly)

cropland.metro = mask(cropland, metro.counties)

# map
# plot(cropland.metro, legend=F, bty='n', box=F)
# plot(metro.counties, add=T)
# plot(network.id.poly, add=T)
# plot(wheat.tot>0, add=T, col='darkgreen', legend=F)
# plot(netwk.flow, add=T, col='darkblue', lwd=0.2)
# plot(mouth, add=T, col='darkblue', pch=19)
# plot(minn.city, add=T, col='Red', pch=8)
# 
# plot(basin.id.poly)
# plot(netwk.flow, add=T, col='darkblue', lwd=0.2)
# plot(minn.city, add=T, col='Red', pch=8)

# read geodatabase of WWTP
fgdb <- "/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/FRS_Wastewater/CWA_summaries_060314.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="D_ILRTSP_CWA_SUMMARIES")
fc = spTransform(fc, CRSobj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")
crs(network) = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"

# crop to study area
wwtp = crop(fc, extent(metro.counties))
#wwtp = spTransform(wwtp, "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")
wwtp = wwtp[metro.counties,]
wwtp.public = wwtp[wwtp$CWP_FACILITY_TYPE_INDICATOR == "POTW",]

##################################################################################################
# try mapview
#install.packages("mapview")
#library(mapview)




## try ggmap
#install.packages("ggmap")
library(ggmap)
library(broom)
library(dplyr)

wwtp.public.df = data.frame(cbind(wwtp.public@data, coordinates(wwtp.public)))

sbbox <- make_bbox(lon = c(extent(metro.counties)[1], extent(metro.counties)[2]), 
                   lat = c(extent(metro.counties)[3], extent(metro.counties)[4]), 
                   f = .1)

test_map = get_map(location = sbbox, maptype = "watercolor", source = "stamen")
ggmap(test_map) + geom_point(data = wwtp.public.df, mapping = aes(x = coords.x1, y = coords.x2), color = "red")


metro.counties_df.1 <- fortify(metro.counties[1,])
metro.counties_df.2 <- fortify(metro.counties[2,])
metro.counties_df.3 <- fortify(metro.counties[3,])
metro.counties_df.4 <- fortify(metro.counties[4,])
metro.counties_df.5 <- fortify(metro.counties[5,])
metro.counties_df.6 <- fortify(metro.counties[6,])
metro.counties_df.7 <- fortify(metro.counties[7,])

###
gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x)))
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as.tbl(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]],
                            by = c("value" = "ID"))
  }
  dat
}
###

riv = readOGR("/net/nfs/merrimack/raid2/data/localdb/biophys/vectormaps/network/global/arcworld/shape", "arc_world_rivers")
rivers = crop(riv, extent(sbbox[1], sbbox[3], sbbox[2], sbbox[4]))
rivers = rivers[rivers$TYPE == 1,]   # remove lakes
rivers = spTransform(rivers, crs(metro.counties))
rivers = rivers[metro.counties,]

rivers_df = tidy(rivers, FNODE_ = "id")
rivers$id <- rownames(rivers@data)
rivers_df <- left_join(rivers_df,
                       rivers@data,
                      by = "id")

cropland = brick("/net/nfs/yukon/raid0/data/CDL/National/CDL_crop_frac/cdl_cropland_US.nc")
cropland = subset(cropland, 8)
cropland.metro = mask(cropland, metro.counties)
cropland.metro = crop(cropland.metro, extent(metro.counties))
cropland.metro[cropland==0] = NA
cropland.metro[cropland<0.01] = NA

cropland_data <- gplot_data(cropland.metro, maxpixels = 100000) # Choose number of pixels
cropland_data = na.omit(cropland_data)

# minnesota
minn.city = as.data.frame(cbind(-93.2650, 44.9778))
colnames(minn.city) = c("lon", "lat")
coordinates(minn.city) <- ~ lon + lat
minn.city$id = 1
minn.city.df = data.frame(cbind(minn.city@data, coordinates(minn.city)))

study_area_map = get_stamenmap(bbox = sbbox, maptype = "terrain")

out.map = 
  ggmap(study_area_map) +                                                                           # base map
  geom_tile(data = cropland_data, aes(x, y, fill = value), alpha = 0.3) +                           # cropland raster
  scale_fill_gradient(low = "greenyellow", high = "green4") +                                       # color cropland raster
 geom_path(data = rivers_df, aes(x = long, y = lat, group = group),                                # river lines
           color='royalblue3', alpha = 0.4, lwd=1.1) +
  geom_polygon(data = metro.counties_df.1, aes(x=long, y = lat), fill=NA, color='slategrey') +      # county borders
  geom_polygon(data = metro.counties_df.2, aes(x=long, y = lat), fill=NA, color='slategrey') +
  geom_polygon(data = metro.counties_df.3, aes(x=long, y = lat), fill=NA, color='slategrey') +
  geom_polygon(data = metro.counties_df.4, aes(x=long, y = lat), fill=NA, color='slategrey') +
  geom_polygon(data = metro.counties_df.5, aes(x=long, y = lat), fill=NA, color='slategrey') +
  geom_polygon(data = metro.counties_df.6, aes(x=long, y = lat), fill=NA, color='slategrey') +
  geom_polygon(data = metro.counties_df.7, aes(x=long, y = lat), fill=NA, color='slategrey') +
  geom_point(data = wwtp.public.df, mapping = aes(x = coords.x1, y = coords.x2),                  # WWTP points
             color = "firebrick2", bg="yellow", shape = 24, cex=2)  +
  geom_point(data = minn.city.df, mapping = aes(x = lon, y = lat),                                # Minneapolis
             color = "black", cex=2.1) 


ggsave(filename = "/net/nfs/squam/raid/userdata/dgrogan/Proposals/USDA_2020/study_area_riv.png",
       device = "png",
       plot = out.map,
       dpi = 300)



# 
# 
# 
# sq_map <- get_map(location = sbbox, maptype = "toner-lines", source = "osm")
# ggmap(sq_map) + 
#   inset_raster(as.raster(cropland.metro), xmin = cropland.metro@extent[1], xmax = cropland.metro@extent[2],
#                ymin = cropland.metro@extent[3], ymax = cropland.metro@extent[4]) +
#   geom_point(data = wwtp.public.df, mapping = aes(x = coords.x1, y = coords.x2), color = "red") +
#   geom_polygon(data = metro.counties_df.1, aes(x=long, y = lat), fill=NA, color='darkblue') +
#   geom_polygon(data = metro.counties_df.2, aes(x=long, y = lat), fill=NA, color='darkblue') +
#   geom_polygon(data = metro.counties_df.3, aes(x=long, y = lat), fill=NA, color='darkblue') +
#   geom_polygon(data = metro.counties_df.4, aes(x=long, y = lat), fill=NA, color='darkblue') +
#   geom_polygon(data = metro.counties_df.5, aes(x=long, y = lat), fill=NA, color='darkblue') +
#   geom_polygon(data = metro.counties_df.6, aes(x=long, y = lat), fill=NA, color='darkblue') +
#   geom_polygon(data = metro.counties_df.7, aes(x=long, y = lat), fill=NA, color='darkblue') 
# 
# 
# # leaflet
# library(leaflet)
# library(RColorBrewer)
# 
# 
# ## create map
# m <- leaflet() %>% addTiles()
# 
# ## save html to png
# saveWidget(m, "temp.html", selfcontained = FALSE)
# webshot("temp.html", file = "Rplot.png",
#         cliprect = "viewport")
# 
# cropland[cropland==0] = NA
# cropland[cropland<0] = NA
# 
# pal <- colorNumeric(c("#FFFFCC", "#0C2C84", "#41B6C4"), values(cropland),
#                     na.color = "transparent")
# pal = colorNumeric(brewer.pal(9, "Greens"), values(cropland),
#                    na.color = "transparent")
# 
# 
# # point
# minn.city = as.data.frame(cbind(-93.2650, 44.9778))
# colnames(minn.city) = c("lon", "lat")
# coordinates(minn.city) <- ~ lon + lat
# 
# g1 = as.data.frame(cbind(-92.8514, 44.7443))
# colnames(g1) = c("lon", "lat")
# coordinates(g1) <- ~ lon + lat
# 
# 
# leaflet() %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   addRasterImage(cropland.metro, colors = pal, opacity = 0.6) %>%
#   addPolygons(data = metro.counties, weight = 2, color='darkred',fillColor = "transparent") %>%
#   #addPolygons(data = network.id.poly, weight = 3, fillColor = "grey") %>%
#   addCircleMarkers(lng = coordinates(wwtp.public)[,1], lat = coordinates(wwtp.public)[,2], radius = 0.1, color='darkblue') %>%
#   addCircleMarkers(lng = coordinates(mouth)[,1], lat = coordinates(mouth)[,2], radius = 2, color='darkred') %>%
#   addLegend(pal = pal, values = values(cropland.metro),
#             title = "Cropland Area")
# 
# 
# cropland.cell.area = raster::area(cropland.metro)
# cropland.area = sum(as.matrix(cropland.cell.area * cropland.metro), na.rm=T)
# cropland.area/total.area
# 
# wheat.cell.area = raster::area(wheat.tot)
# wheat.area = sum(as.matrix(wheat.cell.area * wheat.tot), na.rm=T)
# wheat.area/total.area
# wheat.area/cropland.area
