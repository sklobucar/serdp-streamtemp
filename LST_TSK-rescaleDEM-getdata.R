rm(list = ls())

library(raster)
library(rgdal)

library(plyr)
library(dplyr)
library(tidyr) 
library(ggplot2)
library(gridExtra)
library(data.table)
library(lubridate)


setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/')

##This study area comes from merging the DEMs of four watersheds of the study area in ArcMap, rescaled to 1km resolution and
##merged with 1km DEM of larger extent buffering the study area see: LST_TSK-comparison_write-DEM-raster

study.area.1km.a <-  raster('filled_1km_AK-DEM.tif')

#plot(study.area)
#plot(basin.out, add = T)

plot(study.area.1km.a)

crs(study.area)
res(study.area)

crs(study.area.1km.a)
res(study.area.1km.a)

study.area.1km <- projectRaster(study.area.1km.a, crs = crs(study.area), res = 1000)
study.area.20km <- projectRaster(study.area.1km.a, crs = crs(study.area), res = 20000)

crs(study.area.1km)
res(study.area.1km)
plot(study.area.1km)

crs(study.area.20km)
res(study.area.20km)
plot(study.area.20km)

new_extent <- extent(c(200653.8, 519875.3, 1572108, 1763107)) 
study.area.1km.crop <- crop(study.area.1km, new_extent)
study.area.20km.crop <- crop(study.area.20km, new_extent)

plot(study.area.1km.crop)
plot(basin.out, add = T)

plot(study.area.20km.crop)
plot(basin.out, add = T)

##compare to the one that is missing pixel. 
missing <-  raster('study-basin_1km_res20km_AK-DEM.tif')

par(mfrow = c(2, 1))
plot(study.area.20km.crop)
plot(basin.out, add = T)

plot(missing)
plot(basin.out, add = T)

###not too bad...compare values, looks like newer is higher elevations but pretty similar (should make sense from the orgiinal resolution?)

##anyways write out these rasters, add to a clean script, and do some damn analyes. 

####Write raster out##
#setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/')

#writeRaster(study.area.20km.crop, filename = 'mergedNetMap_filled_20km-res_AK-DEM.grd', overwrite = T)
#writeRaster(study.area.20km.crop, filename = 'mergedNetMap_filled_20km-res_AK-DEM.tif', overwrite = T)

##Get values
elevation.20km <- as.data.frame(values(study.area.20km.crop))

setDT(elevation.20km, keep.rownames = T)

colnames(elevation.20km) <- c('cell', 'elevation')

##Write data frame to add to full dataframe...

#clean up cells to add to full df
prefix <-  'V'
suffix <-  elevation.20km$cell

elevation.20km$cell <- paste(prefix, suffix, sep = '')

save(elevation.20km, file = 'study-area_elevation-20km.Rda')
