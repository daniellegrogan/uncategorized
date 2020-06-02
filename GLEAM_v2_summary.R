# WBM processing for GAO GLEAM project, v2
# Danielle S Grogan
# 2020-05-07

##################################################################################################################
### LIBRARIES AND SOURCE FILES ###

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

source("~/git_repos/WBMr/wbm_load.R")
source("~/git_repos/WBMr/nc_vars_meta.R")
source("~/git_repos/WBMr/spatial_aggregation.R")

##################################################################################################################

var_summary_table = function(country.brick, country.names, cell.area, file.path, path.out){
  # scheme: 1 summary table file per variable.  
  # The file lists each country, and global value (rows)
  # NB: each country has 2 rows, (1) m3/month; (2) mm/month
  # Columns: months (1-12), and annual total
  
  # unit conversion factor
  mm_to_km = 10^-6
  
  # make empty data frame to hold results
  var_summary = data.frame(matrix(nr = (nrow(country.names)+1), nc=16))
  var_summary[,1]  = c(country.names$ADM0_CODE, "Global")
  
  var_summary[,2]  = c(as.character(country.names$ADM0_NAME), "Global")
  var_summary[,3] = "km3"
  
  colnames(var_summary) = c("ADM0_CODE", "ADM0_NAME", 
                            "unit",
                            "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", 
                            "Annual")
  
  # load gridded data
  var_data = brick(file.path)
  varname = nc_vars_meta(file.path)$name[2]
  
  # test out multiplication options
  for(c in 1:nlayers(country.brick)){
    
    # make a single country layer
    country.raster = subset(country.brick, c) 
    country.raster[country.raster == 0] = NA
    grid_mult = var_data * country.raster  # multiply monthly data times a single country layer
    
    # m3 sum
    cell.area =  crop(cell.area, extent(grid_mult))
    data.km3 = grid_mult * cell.area * mm_to_km
    
    country_sum = mat.or.vec(nr=12,nc=1)
    for(m in 1:12){
      country_sum[m]  = sum(as.matrix(subset(data.km3, m)), na.rm=T)
    }

    
    # mm mean (area-weighted)
    # cell_area_mult = cell.area*(subset(country.brick, c) > 0)  # make all grid cells within country = 1, as not to scale to coverage_fraction twice
    # cell_area_weight = cell_area_mult/(max(as.matrix(cell_area_mult), na.rm=T))
    # country_mean = cellStats((grid_mult*cell_area_weight), stat='mean', na.rm=TRUE)
    
    # fill in summary table
    # rows = c(2*c - 1, 2*c)
    var_summary[c, 4:15] = country_sum
    
    print(c)
    removeTmpFiles(h=1) # remove temporary files older than 1 hour. Keeps raster temp folder from filling when processing large raster data
  }
  
  # global
  # sum km3
  data.km3 = var_data * cell.area * mm_to_km
  global_sum = cellStats(data.km3, stat='sum', na.rm=TRUE)
  
  # mm mean (area-weighted)
  # cell_area_weight = cell.area/(max(as.matrix(cell.area), na.rm=T))
  # global_mean = cellStats((var_data*cell_area_weight), stat='mean', na.rm=TRUE)
  
  # fill in global value in summary table
  var_summary[nrow(var_summary), 4:15] = global_sum
  #var_summary[nrow(var_summary),   4:15] = global_mean
  
  # annual sum of both m3 and mm
  var_summary$Annual = rowSums(var_summary[,4:15], na.rm=T)

  write.csv(var_summary, paste(path.out, "/", varname, "_Country_km3_summary_table_mc.csv", sep=""), row.names=F)
  print(varname)
}

##################################################################################################################

### FILE PATHS ###

path = "/net/nfs/squam/raid/data/GLEAM/WBM/GLEAM_v2/climatology_mm_month/"
#path.figures = "/net/nfs/squam/raid/data/GLEAM/Figures/v2"
path.out = "/net/nfs/squam/raid/data/GLEAM/WBM/GLEAM_v2/Tables"

#mc.files = list.files(path, pattern = "_mc", full.names=T)[1:65]
mc.files = list.files(path, pattern = "_mc", full.names=T)

### Input Data ###

# country brick
country.brick = brick("/net/nfs/squam/raid/data/GLEAM/FAO_country_raster.nc")

# country data - link country.brick later names (which are ADM0 codes) to "human readable" country names
cell.fraction = read.delim("/net/nfs/squam/raid/userdata/stanley/projects/2020/GLEAM_Crops_2015/ExtractedCellTableRAWv3.csv", sep="\t")
country.names = subset(cell.fraction, select=c("ADM0_CODE", "ADM0_NAME", "DISP_AREA"))
d = duplicated(country.names)
country.names = country.names[d==F,]

# slight modification to names and extent of country brick
names(country.brick) = country.names$ADM0_CODE

# area of each cell (m2)
cell.area = raster::area(subset(country.brick, 1))  # area in km2

### Make summary tables for each variable
lapply(mc.files, FUN = function(x) var_summary_table(country.brick, country.names, cell.area, x, path.out))


##################################################################################################################

# test with smaller country brick
test_country_brick = subset(country.brick, 1:3)
country.names.sub = subset(country.names, country.names$ADM0_CODE %in% c(170, 160, 152))
mc.files.sub = mc.files[38:41]
lapply(mc.files.sub, FUN = function(x) var_summary_table(country.brick = test_country_brick,
                                                     country.names = country.names.sub,
                                                     cell.area, x, path.out))

##################################################################################################################


bw = brick("/net/nfs/squam/raid/data/GLEAM/WBM/GLEAM_v2/climatology/wbm_Wheat_GW_ET_mc.nc")
file.list = list.files("/net/nfs/squam/raid/data/GLEAM/WBM/GLEAM_v2/monthly/2012/", full.names = T)
wbm.brk = do.call(stack,
                  lapply(file.list, 
                         raster::raster, 
                         varname = "Wheat_BW_ET.nc"))
