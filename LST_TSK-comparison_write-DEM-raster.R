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

####Arctic DEM retrieved from: http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/1km/ #This is 1km specifically, others there...

####Get DEM files
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/')

##This is 1km resolution, but much finer resolution available...maybe work with those later
#Too big for rgdal?
arc1k <- raster('arcticdem_mosaic_1km_v3.0.tif')

##Trying finer res as well...from previous attempts will crop both 1km and 500m by a rough extent (memory issues)...
#May need to creat this separately and only read in the cropped raster...
arc500 <- raster('arcticdem_mosaic_500m_v3.0.tif')

#Look at approx extent of AK
plot(arc1k)

#Approximately...
#ymin = ~0, ymax = ~1.5e06
#xmin = ~-3.5e06, xmax = ~-2.5e06

big_extent <- extent(c(-3500000, -2500000, 0, 1500000)) 

crop_arc1k <- crop(arc1k, big_extent)
crop_arc500 <- crop(arc500, big_extent)

#Visualize
plot(crop_arc1k)

##Write out rasters (and maybe start new script reading those in only?)...and new geotiff?
writeRaster(crop_arc1k, filename = 'cropped_1km_AK-DEM.grd', overwrite = T)
writeRaster(crop_arc500, filename = 'cropped_500m_AK-DEM.grd', overwrite = T)

writeRaster(crop_arc1k, filename = 'cropped_1km_AK-DEM.tif', overwrite = T)
writeRaster(crop_arc500, filename = 'cropped_500m_AK-DEM.tif', overwrite = T)

########################################################################