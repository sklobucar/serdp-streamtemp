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

###########################################################################
####BELOW HERE-- adding from another script...may have memory issues...####

##Clearing out above to preserve memory
rm(list = ls())
gc()

setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/')

##1km resolution
arc1k <- raster('cropped_1km_AK-DEM.tif')

##500m...maybe checkout finer later (but memory issues would need to be addressed (e.g., cropping and creating smaller file/extent))
arc500 <- raster('cropped_500m_AK-DEM.tif')

####Reproject to match MODIS/WRF
##Check CRS
crs(arc1k)
#CRS arguments:
#+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 

crs(arc500) #same

##Get a file with the target CRS
lst_mod <- brick('lst_tsk_multiband/MOD11A2_multiband.grd')

##Check CRS
crs(lst_mod)

##Reproject but keep resolution (both pretty large...might need to read out)
arc1k.2 <- projectRaster(arc1k, crs = crs(lst_mod), res = 1000)
arc500.2 <- projectRaster(arc500, crs = crs(lst_mod), res = 500)

##Reproject but go to resolution of MODIS/WRF (not sure if this step matters here...or would differ from doing later)
#But, these are much smaller so maybe easier to work with if ordering of steps doesnt matter..
arc1k.3 <- projectRaster(arc1k, crs = crs(lst_mod), res = 20000)
arc500.3 <- projectRaster(arc500, crs = crs(lst_mod), res = 20000)

##Check CRS
crs(arc1k.2)
crs(arc1k.3)
crs(arc500.2)
crs(arc500.3)
crs(lst_mod)

##Visualize
plot(arc1k.2)
plot(arc1k.3) 
plot(arc500.2)
plot(arc500.3)
#extent is different

####Crop extent to match extent (study area of 'basin.out') used in LST_TSK-comparison_getdata-crop-tidy-rasters.R
##Define new extent
new_extent <- extent(c(200653.8, 519875.3, 1572108, 1763107)) 

##Crop
arc1k.a <- crop(arc1k.2, new_extent)
arc1k.b <- crop(arc1k.3, new_extent)

arc500.a <- crop(arc500.2, new_extent)
arc500.b <- crop(arc500.3, new_extent)

##Visualize
#Load shapfile for study area (merge buffered basins (used HUC12 for Chena...) from NetMap files, dissolve to one shapefile)
setwd('F:/NetMap/FishFire_NetMap/')
basin.out <- readOGR(dsn = '.', layer = "basin_chnHUC12")

plot(arc1k.a)
plot(basin.out, add = T)

plot(arc1k.b)
plot(basin.out, add = T)

plot(arc500.a)
plot(basin.out, add = T)

plot(arc500.b)  
plot(basin.out, add = T)

##MISSING VALUES FOR CELL WITHIN EXTENT (get from Netmap...)

#compare both at 20km
par(mfrow = c(2,1))

plot(arc1k.b)
plot(basin.out, add = T)

plot(arc500.b)  
plot(basin.out, add = T)
#look about the same visually, how about values?

#arc1k.b
#values      : 104.4397, 1365.219  (min, max)
#arc500.b
#104.7505, 1394.692  (min, max)

values1000 <- as.data.frame(values(arc1k.b))
colnames(values1000) <- c("1km_elevation")
values500 <- as.data.frame(values(arc500.b))
colnames(values500) <- c("500m_elevation")

elevation <- cbind(values1000, values500)

elevation$diff <- elevation$`1km_elevation` - elevation$`500m_elevation`

##okay for now...maybe do different later (finer resolution)

####Write raster out##
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/')

####Reprojected but not cropped to extent of study area

##1km resolution
#As raster
writeRaster(arc1k.2, filename = 'reproj_1km_res1km_AK-DEM.grd', overwrite = T)
writeRaster(arc1k.3, filename = 'reproj_1km_res20km_AK-DEM.grd', overwrite = T)
#As geotiff
writeRaster(arc1k.2, filename = 'reproj_1km_res1km_AK-DEM.tif', overwrite = T)
writeRaster(arc1k.3, filename = 'reproj_1km_res20km_AK-DEM.tif', overwrite = T)

##500m resolution
#As raster
writeRaster(arc500.2, filename = 'reproj_500m_res500m_AK-DEM.grd', overwrite = T)
writeRaster(arc500.3, filename = 'reproj_500m_res20km_AK-DEM.grd', overwrite = T)

writeRaster(arc500.2, filename = 'reproj_500m_res500m_AK-DEM.tif', overwrite = T)
writeRaster(arc500.3, filename = 'reproj_500m_res20km_AK-DEM.tif', overwrite = T)

####Reprojected AND cropped to study area

##1km resolution
#As raster
writeRaster(arc1k.a, filename = 'study-basin_1km_res1km_AK-DEM.grd', overwrite = T)
writeRaster(arc1k.b, filename = 'study-basin_1km_res20km_AK-DEM.grd', overwrite = T)
#As geotiff
writeRaster(arc1k.a, filename = 'study-basin_1km_res1km_AK-DEM.tif', overwrite = T)
writeRaster(arc1k.b, filename = 'study-basin_1km_res20km_AK-DEM.tif', overwrite = T)

##500m resolution
#As raster
writeRaster(arc500.a, filename = 'study-basin_500m_res500m_AK-DEM.grd', overwrite = T)
writeRaster(arc500.b, filename = 'study-basin_500m_res20km_AK-DEM.grd', overwrite = T)

writeRaster(arc500.a, filename = 'study-basin_500m_res500m_AK-DEM.tif', overwrite = T)
writeRaster(arc500.b, filename = 'study-basin_500m_res20km_AK-DEM.tif', overwrite = T)

#########################################################################################


