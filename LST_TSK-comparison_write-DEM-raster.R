rm(list = ls())

library(raster)
library(rgdal)

library(plyr)
library(dplyr)
library(tidyr) #this masks 'extract' from raster
library(ggplot2)
library(gridExtra)
library(data.table)
library(lubridate)

####Arctic DEm retrieved from: http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/1km/
  ##This is 1km resolution, but much finer resolution available...maybe work with those later

#####Get DEM files
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/')

##Too big for rgdal?
arctic <- raster('arcticdem_mosaic_1km_v3.0.tif')

##Check CRS
crs(arctic)
    #CRS arguments:
    #+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 

####Reproject to match MODIS/WRF

##Get a file with the target CRS
lst_mod <- brick('lst_tsk_multiband/MOD11A2_multiband.grd')

##Check CRS
crs(lst_mod)

##arctic to lst_mod crs
arctic2 <- projectRaster(arctic, crs = crs(lst_mod))

##Check CRS
crs(arctic2)

##Resolution also matches now....not sure if/how this will matter yet, but make one that also has 1km?
res(arctic)
res(arctic2)
res(lst_mod)

arctic3 <- projectRaster(arctic, crs = crs(lst_mod), res = 1000)

##Visualize
plot(arctic2)
plot(arctic3) #extent is different

####Crop extent of arctic2 and arctic3 to match extent (study area of 'basin.out') used in LST_TSK-comparison_getdata-crop-tidy-rasters.R

##Define new extent
new_extent <- extent(c(200653.8, 519875.3, 1572108, 1763107)) 

extent(new_extent)
#xmin        : 200653.8 
#xmax        : 519875.3 
#ymin        : 1572108 
#ymax        : 1763107

##Crop
crop_arctic2 <- crop(arctic2, new_extent)

crop_arctic3 <- crop(arctic3, new_extent)

##Visualize
#Load shapfile for study area (merge buffered basins (used HUC12 for Chena...) from NetMap files, dissolve to one shapefile)
setwd('F:/NetMap/FishFire_NetMap/')
basin.out <- readOGR(dsn = '.', layer = "basin_chnHUC12")


plot(crop_arctic2)
  plot(basin.out, add = T)

plot(crop_arctic3)
  plot(basin.out, add = T)
  
####Go grab a higher resolution and see if differences (at 20 km)?
##Get DEM files
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/')

arc500 <- raster('arcticdem_mosaic_500m_v3.0.tif')

crs(arc500)  

#arc500b <- projectRaster(arc500, crs = crs(lst_mod)) ##Too big...need to clip extent first?

extent(arc500)

#look at approx extent of AK
plot(arc500)

#ymin = ~0, ymax = ~1.5e06
#xmin = ~-3.5e06, xmax = ~-2.5e06

big_extent <- extent(c(-3500000, -2500000, 0, 1500000)) ##Should go back and do for the other at 1km resolution

crop_arc500 <- crop(arc500, big_extent)

#visualize
plot(crop_arc500)

arc500b <- projectRaster(crop_arc500, crs = crs(lst_mod)) ##Still too big, do for others to free space?
