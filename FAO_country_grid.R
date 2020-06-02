# FAO Country Maps
# Processing for GLEAM project
# Danielle S Grogan
# 2020-05-14

##################################################################################################################
### LIBRARIES AND SOURCE FILES ###

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

### Country data from FAO

### Country ID for each grid cell:
cell.fraction = read.delim("/raid/userdata/stanley/projects/2020/GLEAM_Crops_2015/ExtractedCellTableRAWv3.csv", sep="\t")
countries = unique(cell.fraction$ADM0_CODE)  # list of country admin codes

#cell.fraction = read.delim("/net/nfs/squam/raid/data/GLEAM/cellid_fraction_adm0.txt", sep="\t")
#countries = unique(cell.fraction$ADM0_CODE)  # list of country admin codes

### make a raster: global, 5 min, labeled with consecutive id numbers
r = raster(nr = 180*12, nc= 360*12, res = (1/12))
values(r) = seq(1, ncell(r))

# ### list the x and y index in the grid for each cell ID.  Make 0-value column as placeholder for coverage fraction
y.indices = rep(seq(1,360*12), 180*12)
x.indices = unlist(lapply(seq(1,180*12), function(x) rep(x,360*12)))
#coverage_fraction = mat.or.vec(nr=length(x.indices), nc=1)
cell = seq(1, ncell(r))
#value.xy = as.data.frame(cbind(cell, x.indices, y.indices, "coverage_fraction"))
#colnames(value.xy) = c("cell", "x", "y", "coverage_fraction")
value.xy = as.data.frame(cbind(cell, x.indices, y.indices))
colnames(value.xy) = c("cell", "x", "y")

## loop through each country
for(c in countries){
  cell.fraction.c = subset(cell.fraction, cell.fraction$ADM0_CODE == c)
  
  # assign values of 1 to raster grids in the country
  r.country = r %in% cell.fraction.c$cell
  
  # find which grid cells have coverage_fraction < 1
  split.cells = subset(cell.fraction.c, cell.fraction.c$coverage_fraction < 1)
  
  # identify if fraction is split with ocean, or with other country
  # only reduce fraction if split with other country 
  cell.country.notc = subset(cell.fraction, cell.fraction$ADM0_CODE != c)
  match.rows = which(cell.country.notc$cell %in% split.cells$cell)
  split.cell.ids = split.cells$cell
  
  if(length(match.rows) > 0){  # if  == 0, then all the splits are between the country and the ocean (coastline)
    for(i in split.cell.ids){
      index = which(value.xy$cell == i)
      v = split.cells$coverage_fraction[which(split.cells$cell == i)]
      r.country[value.xy$x[index], value.xy$y[index]] = v
      
      # a = which(split.cell.ids == i)
      # b = length(split.cell.ids)
      # print(paste(a, "out of", b))
    }
  }
  #plot(r.country)
  #sum(as.matrix(r.country < 1 & r.country > 0), na.rm=T)
  
  if(c == countries[1]){
    country.stack = r.country
    names(country.stack) = as.character(c)
  }else{
    country.stack = stack(country.stack, r.country)
    names(country.stack)[which(countries == c)] = as.character(c)
  }
  print(c)
}

writeRaster(country.stack, "/net/nfs/squam/raid/data/GLEAM/FAO_country_raster.nc", 
            format="CDF", varname = "coverage_fraction", varunit = "fraction", overwrite=T)

