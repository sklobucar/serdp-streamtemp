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

################################
#multiband files were created in .../LST_TSK-comparison_write-multiband-raster-geotiff.R
  ##because I need to get dates of layers, I am using the rasters (not Geotiffs) because names are preserved...see tidying below
################################

#####Get MODIS files
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/lst_tsk_multiband/')

lst_mod <- brick('MOD11A2_multiband.grd')
lst_myd <- brick('MYD11A2_multiband.grd')

####Get WRF files

tsk_era_mod <- brick("ERA-Interim_historical_MOD11A2_multiband.grd")
tsk_era_myd <- brick("ERA-Interim_historical_MYD11A2_multiband.grd")

tsk_ccsm_rcp_mod <-  brick('NCAR-CCSM4_rcp85_MOD11A2_multiband.grd')
tsk_ccsm_rcp_myd <-  brick('NCAR-CCSM4_rcp85_MYD11A2_multiband.grd')

tsk_ccsm_hist_mod <- brick('NCAR-CCSM4_historical_MOD11A2_multiband.grd')
tsk_ccsm_hist_myd <- brick('NCAR-CCSM4_historical_MYD11A2_multiband.grd')

tsk_gfdl_rcp_mod <- brick('GFDL-CM3_rcp85_MOD11A2_multiband.grd')
tsk_gfdl_rcp_myd <- brick('GFDL-CM3_rcp85_MYD11A2_multiband.grd')

tsk_gfdl_hist_mod <-  brick('GFDL-CM3_historical_MOD11A2_multiband.grd')
tsk_gfdl_hist_myd <-  brick('GFDL-CM3_historical_MYD11A2_multiband.grd')

###############################

#####Crop to study area extent

#Load shapfile for study area (merge buffered basins (used HUC12 for Chena...) from NetMap files, dissolve to one shapefile)
setwd('F:/NetMap/FishFire_NetMap/')
basin.out <- readOGR(dsn = '.', layer = "basin_chnHUC12")

extent(basin.out)

crop_lst_mod <- crop(lst_mod, basin.out)
crop_lst_myd <- crop(lst_myd, basin.out)

crop_tsk_era_mod <- crop(tsk_era_mod, basin.out)
crop_tsk_era_myd <- crop(tsk_era_myd, basin.out)

crop_tsk_ccsm_rcp_mod <- crop(tsk_ccsm_rcp_mod, basin.out)
crop_tsk_ccsm_rcp_myd <- crop(tsk_ccsm_rcp_myd, basin.out)

crop_tsk_ccsm_hist_mod <- crop(tsk_ccsm_hist_mod, basin.out)
crop_tsk_ccsm_hist_myd <- crop(tsk_ccsm_hist_myd, basin.out)

crop_tsk_gfdl_rcp_mod <- crop(tsk_gfdl_rcp_mod, basin.out)
crop_tsk_gfdl_rcp_myd <- crop(tsk_gfdl_rcp_myd, basin.out)

crop_tsk_gfdl_hist_mod <- crop(tsk_gfdl_hist_mod, basin.out)
crop_tsk_gfdl_hist_myd <- crop(tsk_gfdl_hist_myd, basin.out)

#Pick a layer and check via plot
lst300 <- raster(crop_lst_mod, layer = 300)
plot(lst300)
  plot(basin.out, add = T) ###Looks like a row should be added top, bottom, right to encompass total study area###
  
extent(basin.out)
#xmin        : 220653.8 
#xmax        : 499875.3 
#ymin        : 1592108 
#ymax        : 1743107

new_extent <- extent(c(200653.8, 519875.3, 1572108, 1763107)) ##Might as well buffer all around

extent(new_extent)
#xmin        : 200653.8 
#xmax        : 519875.3 
#ymin        : 1572108 
#ymax        : 1763107 

recrop_lst_mod <- crop(lst_mod, new_extent)

#Check plot with new extent
lst300b <- raster(recrop_lst_mod, layer = 300)
plot(lst300b)
  plot(basin.out, add = T) ###Looks good
  
###Crop to new extent

  crop_lst_mod <- crop(lst_mod, new_extent)
  crop_lst_myd <- crop(lst_myd, new_extent)
  
  crop_tsk_era_mod <- crop(tsk_era_mod, new_extent)
  crop_tsk_era_myd <- crop(tsk_era_myd, new_extent)
  
  crop_tsk_ccsm_rcp_mod <- crop(tsk_ccsm_rcp_mod, new_extent)
  crop_tsk_ccsm_rcp_myd <- crop(tsk_ccsm_rcp_myd, new_extent)
  
  crop_tsk_ccsm_hist_mod <- crop(tsk_ccsm_hist_mod, new_extent)
  crop_tsk_ccsm_hist_myd <- crop(tsk_ccsm_hist_myd, new_extent)
  
  crop_tsk_gfdl_rcp_mod <- crop(tsk_gfdl_rcp_mod, new_extent)
  crop_tsk_gfdl_rcp_myd <- crop(tsk_gfdl_rcp_myd, new_extent)
  
  crop_tsk_gfdl_hist_mod <- crop(tsk_gfdl_hist_mod, new_extent)
  crop_tsk_gfdl_hist_myd <- crop(tsk_gfdl_hist_myd, new_extent)
  
#####Get data and tidy
  
###############################################################
####LST_MOD####################################################
  
a <- getValues(crop_lst_mod) 
b <- t(a)

lst_mod_df <- as.data.frame(b)

#keep row names
setDT(lst_mod_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(lst_mod_df, rn)
colnames(dates) = "name"

#drop rownames 
lst_mod_df2 <- select(lst_mod_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 10, 13))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 14,16))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)
 
####combine extracted year and doy with cell values
new_lst_mod_df <- cbind(date2, lst_mod_df2)

####go from wide to tall format
new_lst_mod_df <- new_lst_mod_df %>% 
  gather(cell, lst_mod, -name, -yr, -doy)  


###################################################################
####LST_MYD########################################################

a <- getValues(crop_lst_myd) 
b <- t(a)

lst_myd_df <- as.data.frame(b)

#keep row names
setDT(lst_myd_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(lst_myd_df, rn)
colnames(dates) = "name"

#drop rownames 
lst_myd_df2 <- select(lst_myd_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 10, 13))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 14,16))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_lst_myd_df <- cbind(date2, lst_myd_df2)

####go from wide to tall format
new_lst_myd_df <- new_lst_myd_df %>% 
  gather(cell, lst_myd, -name, -yr, -doy) 


#####################################################################
####TSK_ERA_MOD######################################################

a <- getValues(crop_tsk_era_mod) 
b <- t(a)

tsk_era_mod_df <- as.data.frame(b)

#keep row names
setDT(tsk_era_mod_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(tsk_era_mod_df, rn)
colnames(dates) = "name"

#drop rownames 
tsk_era_mod_df2 <- select(tsk_era_mod_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 53, 56))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 57,59))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_tsk_era_mod_df <- cbind(date2, tsk_era_mod_df2)

####go from wide to tall format
new_tsk_era_mod_df <- new_tsk_era_mod_df %>% 
  gather(cell, tsk_era_mod, -name, -yr, -doy) 


#####################################################################
####TSK_ERA_MYD######################################################

a <- getValues(crop_tsk_era_myd) 
b <- t(a)

tsk_era_myd_df <- as.data.frame(b)

#keep row names
setDT(tsk_era_myd_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(tsk_era_myd_df, rn)
colnames(dates) = "name"

#drop rownames 
tsk_era_myd_df2 <- select(tsk_era_myd_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 53, 56))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 57,59))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_tsk_era_myd_df <- cbind(date2, tsk_era_myd_df2)

####go from wide to tall format
new_tsk_era_myd_df <- new_tsk_era_myd_df %>% 
  gather(cell, tsk_era_myd, -name, -yr, -doy) 


#############################################################################
####tsk_ccsm_rcp_mod#########################################################

a <- getValues(crop_tsk_ccsm_rcp_mod) 
b <- t(a)

tsk_ccsm_rcp_mod_df <- as.data.frame(b)

#keep row names
setDT(tsk_ccsm_rcp_mod_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(tsk_ccsm_rcp_mod_df, rn)
colnames(dates) = "name"

#drop rownames 
tsk_ccsm_rcp_mod_df2 <- select(tsk_ccsm_rcp_mod_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 47, 50))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 51,53))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_tsk_ccsm_rcp_mod_df <- cbind(date2, tsk_ccsm_rcp_mod_df2)

####go from wide to tall format
new_tsk_ccsm_rcp_mod_df <- new_tsk_ccsm_rcp_mod_df %>% 
  gather(cell, tsk_ccsm_rcp_mod, -name, -yr, -doy) 


#############################################################################
####tsk_ccsm_rcp_myd#########################################################

a <- getValues(crop_tsk_ccsm_rcp_myd) 
b <- t(a)

tsk_ccsm_rcp_myd_df <- as.data.frame(b)

#keep row names
setDT(tsk_ccsm_rcp_myd_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(tsk_ccsm_rcp_myd_df, rn)
colnames(dates) = "name"

#drop rownames 
tsk_ccsm_rcp_myd_df2 <- select(tsk_ccsm_rcp_myd_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 47, 50))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 51,53))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_tsk_ccsm_rcp_myd_df <- cbind(date2, tsk_ccsm_rcp_myd_df2)

####go from wide to tall format
new_tsk_ccsm_rcp_myd_df <- new_tsk_ccsm_rcp_myd_df %>% 
  gather(cell, tsk_ccsm_rcp_myd, -name, -yr, -doy) 


#############################################################################
####tsk_ccsm_hist_mod#########################################################

a <- getValues(crop_tsk_ccsm_hist_mod) 
b <- t(a)

tsk_ccsm_hist_mod_df <- as.data.frame(b)

#keep row names
setDT(tsk_ccsm_hist_mod_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(tsk_ccsm_hist_mod_df, rn)
colnames(dates) = "name"

#drop rownames 
tsk_ccsm_hist_mod_df2 <- select(tsk_ccsm_hist_mod_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 52, 55))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 56,58))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_tsk_ccsm_hist_mod_df <- cbind(date2, tsk_ccsm_hist_mod_df2)

####go from wide to tall format
new_tsk_ccsm_hist_mod_df <- new_tsk_ccsm_hist_mod_df %>% 
  gather(cell, tsk_ccsm_hist_mod, -name, -yr, -doy) 


#############################################################################
####tsk_ccsm_hist_myd#########################################################

a <- getValues(crop_tsk_ccsm_hist_myd) 
b <- t(a)

tsk_ccsm_hist_myd_df <- as.data.frame(b)

#keep row names
setDT(tsk_ccsm_hist_myd_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(tsk_ccsm_hist_myd_df, rn)
colnames(dates) = "name"

#drop rownames 
tsk_ccsm_hist_myd_df2 <- select(tsk_ccsm_hist_myd_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 52, 55))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 56,58))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_tsk_ccsm_hist_myd_df <- cbind(date2, tsk_ccsm_hist_myd_df2)

####go from wide to tall format
new_tsk_ccsm_hist_myd_df <- new_tsk_ccsm_hist_myd_df %>% 
  gather(cell, tsk_ccsm_hist_myd, -name, -yr, -doy) 


#############################################################################
####tsk_gfdl_rcp_mod#########################################################

a <- getValues(crop_tsk_gfdl_rcp_mod) 
b <- t(a)

tsk_gfdl_rcp_mod_df <- as.data.frame(b)

#keep row names
setDT(tsk_gfdl_rcp_mod_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(tsk_gfdl_rcp_mod_df, rn)
colnames(dates) = "name"

#drop rownames 
tsk_gfdl_rcp_mod_df2 <- select(tsk_gfdl_rcp_mod_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 45, 48))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 49,51))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_tsk_gfdl_rcp_mod_df <- cbind(date2, tsk_gfdl_rcp_mod_df2)

####go from wide to tall format
new_tsk_gfdl_rcp_mod_df <- new_tsk_gfdl_rcp_mod_df %>% 
  gather(cell, tsk_gfdl_rcp_mod, -name, -yr, -doy) 


#############################################################################
####tsk_gfdl_rcp_myd#########################################################

a <- getValues(crop_tsk_gfdl_rcp_myd) 
b <- t(a)

tsk_gfdl_rcp_myd_df <- as.data.frame(b)

#keep row names
setDT(tsk_gfdl_rcp_myd_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(tsk_gfdl_rcp_myd_df, rn)
colnames(dates) = "name"

#drop rownames 
tsk_gfdl_rcp_myd_df2 <- select(tsk_gfdl_rcp_myd_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 45, 48))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 49,51))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_tsk_gfdl_rcp_myd_df <- cbind(date2, tsk_gfdl_rcp_myd_df2)

####go from wide to tall format
new_tsk_gfdl_rcp_myd_df <- new_tsk_gfdl_rcp_myd_df %>% 
  gather(cell, tsk_gfdl_rcp_myd, -name, -yr, -doy) 


#############################################################################
####tsk_gfdl_hist_mod#########################################################

a <- getValues(crop_tsk_gfdl_hist_mod) 
b <- t(a)

tsk_gfdl_hist_mod_df <- as.data.frame(b)

#keep row names
setDT(tsk_gfdl_hist_mod_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(tsk_gfdl_hist_mod_df, rn)
colnames(dates) = "name"

#drop rownames 
tsk_gfdl_hist_mod_df2 <- select(tsk_gfdl_hist_mod_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 50, 53))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 54,56))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_tsk_gfdl_hist_mod_df <- cbind(date2, tsk_gfdl_hist_mod_df2)

####go from wide to tall format
new_tsk_gfdl_hist_mod_df <- new_tsk_gfdl_hist_mod_df %>% 
  gather(cell, tsk_gfdl_hist_mod, -name, -yr, -doy) 


#############################################################################
####tsk_gfdl_hist_myd#########################################################

a <- getValues(crop_tsk_gfdl_hist_myd) 
b <- t(a)

tsk_gfdl_hist_myd_df <- as.data.frame(b)

#keep row names
setDT(tsk_gfdl_hist_myd_df, keep.rownames = T)

#separate rownames to extract date (year + doy)
dates <- select(tsk_gfdl_hist_myd_df, rn)
colnames(dates) = "name"

#drop rownames 
tsk_gfdl_hist_myd_df2 <- select(tsk_gfdl_hist_myd_df, -rn)

####extract year and doy from layer name in dates
yr <- as.data.frame(substr(dates$name, 50, 53))
colnames(yr) = "yr"
doy <- as.data.frame(substr(dates$name, 54,56))
colnames(doy) = 'doy'

date2 <- cbind(dates, yr, doy)

####combine extracted year and doy with cell values
new_tsk_gfdl_hist_myd_df <- cbind(date2, tsk_gfdl_hist_myd_df2)

####go from wide to tall format
new_tsk_gfdl_hist_myd_df <- new_tsk_gfdl_hist_myd_df %>% 
  gather(cell, tsk_gfdl_hist_myd, -name, -yr, -doy) 


###############################################
###############################################

##Write indiviudal dataframes out as Rdata..just in case shit gets boogered
#setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/lst_tsk_multiband/TidyData/')

#save(new_lst_df, file = 'lst_mod.Rdata')
#fwrite(new_lst_df, file = 'lst_mod.csv')

#save(new_lst_myd_df, file = 'lst_myd.Rdata')
#fwrite(new_lst_myd_df, file = 'lst_myd.csv')

#save(new_tsk_ccsm_hist_mod_df, file = 'tsk_ccsm_hist_mod.Rdata')
#fwrite(new_tsk_ccsm_hist_mod_df, file = 'tsk_ccsm_hist_mod.csv')

#save(new_tsk_ccsm_hist_myd_df, file = 'tsk_ccsm_hist_myd.Rdata')
#fwrite(new_tsk_ccsm_hist_myd_df, file = 'tsk_ccsm_hist_myd.csv')

#save(new_tsk_ccsm_rcp_mod_df, file = 'tsk_ccsm_rcp_mod.Rdata')
#fwrite(new_tsk_ccsm_rcp_mod_df, file = 'tsk_ccsm_rcp_mod.csv')

#save(new_tsk_ccsm_rcp_myd_df, file = 'tsk_ccsm_rcp_myd.Rdata')
#fwrite(new_tsk_ccsm_rcp_myd_df, file = 'tsk_ccsm_rcp_myd.csv')

#save(new_tsk_era_mod_df, file = 'tsk_era_mod.Rdata')
#fwrite(new_tsk_era_mod_df, file = 'tsk_era_mod.csv')

#save(new_tsk_era_myd_df, file = 'tsk_era_myd.Rdata')
#fwrite(new_tsk_era_myd_df, file = 'tsk_era_myd.csv')

#save(new_tsk_gfdl_hist_mod_df, file = 'tsk_gfdl_hist_mod.Rdata')
#fwrite(new_tsk_gfdl_hist_mod_df, file = 'tsk_gfdl_hist_mod.csv')

#save(new_tsk_gfdl_hist_myd_df, file = 'tsk_gfdl_hist_myd.Rdata')
#fwrite(new_tsk_gfdl_hist_myd_df, file = 'tsk_gfdl_hist_myd.csv')

#save(new_tsk_gfdl_rcp_mod_df, file = 'tsk_gfdl_rcp_mod.Rdata')
#fwrite(new_tsk_gfdl_rcp_mod_df, file = 'tsk_gfdl_rcp_mod.csv')

#save(new_tsk_gfdl_rcp_myd_df, file = 'tsk_gfdl_rcp_myd.Rdata')
#fwrite(new_tsk_gfdl_rcp_myd_df, file = 'tsk_gfdl_rcp_myd.csv')

####################################################################

#############combine to master data set!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
modis_lst_mod <- select(new_lst_mod_df, -name)
modis_lst_myd <- select(new_lst_myd_df, -name)

wrf1 <- select(new_tsk_ccsm_hist_mod_df, -name)
wrf2 <- select(new_tsk_ccsm_hist_myd_df, -name)

wrf3 <- select(new_tsk_ccsm_rcp_mod_df, -name)
wrf4 <- select(new_tsk_ccsm_rcp_myd_df, -name)

wrf5 <- select(new_tsk_era_mod_df, -name)
wrf6 <- select(new_tsk_era_myd_df, -name)

wrf7 <- select(new_tsk_gfdl_hist_mod_df, -name)
wrf8 <- select(new_tsk_gfdl_hist_myd_df, -name)

wrf9 <- select(new_tsk_gfdl_rcp_mod_df, -name)
wrf10 <- select(new_tsk_gfdl_rcp_myd_df, -name)

modis_df <- full_join(modis_lst_mod, modis_lst_myd, by = c('yr', 'doy', 'cell'))

wrf_df <- full_join(wrf1, wrf2, by = c('yr', 'doy', 'cell')) %>%
  full_join(., wrf3, by = c('yr', 'doy', 'cell')) %>%
  full_join(., wrf4, by = c('yr', 'doy', 'cell')) %>%
  full_join(., wrf5, by = c('yr', 'doy', 'cell')) %>%
  full_join(., wrf6, by = c('yr', 'doy', 'cell')) %>%
  full_join(., wrf7, by = c('yr', 'doy', 'cell')) %>%
  full_join(., wrf8, by = c('yr', 'doy', 'cell')) %>%
  full_join(., wrf9, by = c('yr', 'doy', 'cell')) %>%
  full_join(., wrf10, by = c('yr', 'doy', 'cell'))

full_df <- full_join(modis_df, wrf_df, by = c('yr', 'doy', 'cell'))

#####SAVE dataframes
#save(full_df, file = 'full_df.Rdata')
#fwrite(full_df, file = 'full_df.csv')


####Check column classes and such....
sapply(full_df, class)

full_df$yr <- as.numeric(full_df$yr) 
full_df$doy <- as.character(full_df$doy)
full_df$doy <- as.numeric(full_df$doy)

####Create new column for date (month and day first) from doy...not sure what, if anything, to do with leap years yet

#full_df$date <- if(full_df$year = 2000 | 2004 | 2008 | 2012 | 2016) {
#    as.Date(full_df$doy, format = c('%Y-%m-%d'), origin = '1999-12-31')
#} else 
#    {(as.Date(full_df$doy, format = c('%Y-%m-%d'), origin = '2000-12-31'))}
  
full_df$date <- as.Date(full_df$doy, format = c('%Y-%m-%d'), origin = '1999-12-31')

#Drop year from date
full_df$monthday <- format(full_df$date, format = '%m-%d')

#Combine with year column to get "true" date (see above re: leap years)
full_df <- full_df %>% 
  select(-date) %>% 
  unite(ymd, yr, monthday, sep = '-', remove = F) %>% 
  select(ymd, yr, monthday, doy, cell, lst_mod, lst_myd, tsk_ccsm_hist_mod, tsk_ccsm_hist_myd,
         tsk_ccsm_rcp_mod, tsk_ccsm_hist_myd, tsk_era_mod, tsk_era_myd, tsk_gfdl_hist_mod, tsk_gfdl_hist_myd,
         tsk_gfdl_rcp_mod, tsk_gfdl_rcp_myd)

####Get lat long centers for each grid cell
crop_coords = as.data.frame(xyFromCell(crop_lst_mod, cell = 1:144))

#Test
plot(lst300b)
plot(basin.out, add = T) 
points(crop_coords) ###Looks good

#Create vector of cell numbers and join with full_df
prefix <- 'V'
n <- 144
suffix <- seq(1:n)

crop_coords$cell <- paste(prefix, suffix, sep = '')

colnames(crop_coords) <- c('lon', 'lat', 'cell')  

full_df <- left_join(full_df, crop_coords, by = 'cell')
 
full_df <-select(full_df, ymd, yr, monthday, doy, cell, lon, lat, lst_mod, lst_myd, tsk_ccsm_hist_mod, tsk_ccsm_hist_myd,
       tsk_ccsm_rcp_mod, tsk_ccsm_hist_myd, tsk_era_mod, tsk_era_myd, tsk_gfdl_hist_mod, tsk_gfdl_hist_myd,
       tsk_gfdl_rcp_mod, tsk_gfdl_rcp_myd)

  
#####SAVE dataframes
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/serdp-streamtemp/')

save(full_df, file = 'full_df.Rdata')
fwrite(full_df, file = 'full_df.csv')


