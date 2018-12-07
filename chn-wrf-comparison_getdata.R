####Script to compare temp logger values for Chena (Huntsman) to WRF, etc. 

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

####Get logger locations; downloaded from MODIS Dropbox (emailed from Jeff 16 Jul18)
##https://www.dropbox.com/sh/0h491dz049nnjj0/AAB6j5atebKY7jr1JrO9-1sga?dl=0

setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/huntsman-chena/')
logs <- readOGR(dsn = '.', layer = 'logger_location')

#Visual check
plot(logs)

#Check crs
crs(logs)

####Load study area and create extent for plotting
setwd('F:/NetMap/FishFire_NetMap/')
basin.out <- readOGR(dsn = '.', layer = "basin_chnHUC12")
setwd('F:/NetMap/FishFire_NetMap/chn/')
chena <- readOGR(dsn = '.', layer = 'Chena_HUC12')

plot(basin.out)
plot(logs.loc, add = T)

crs(basin.out)

##Transform loggers locations to basin.out CRS
logs.loc <- spTransform(logs, CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))

##Plot on grid
new_extent <- extent(c(200653.8, 519875.3, 1592108, 1763107))

grid <- raster(extent(new_extent))

res(grid) <- c(20000,20000)

crs(grid) <- crs(basin.out)

gridpolygon <- rasterToPolygons(grid)

plot(gridpolygon)
plot(basin.out, add = T)
plot(logs.loc, add = T)
plot(chena, add = T)
####nice (?)

####Logger locations to raster
logs.coords <- as.data.frame(coordinates(logs.loc))
logs.site <- as.data.frame(logs.loc$Site_Index)

logs.xy  <- cbind(logs.site, logs.coords)

###
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/huntsman-chena/')
sites <- read.csv(file = 'logger_location.csv')

sites2 <-  select(sites, Site, Site.Index, Site.Name)

loggers <- cbind(sites2, logs.coords)

####Bring in temp data
temps <- read.csv('all_temp.csv')









b <- t(temps)

temp_df <- as.data.frame(b)

setDT(temp_df, keep.rownames = T)

