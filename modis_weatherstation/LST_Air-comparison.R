rm(list = ls())

library(dplyr)
library(tidyr) #this masks 'extract' from raster (?)
library(ggplot2)


##Load data
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/modis_weatherstation/')

snotel <- read.csv('modis_wrf_snotel_comparison_chena_river_huc.csv')
acis <- read.csv('modis_wrf_acis_comparison_chena_river_huc.csv')

##Check and clean data
names(snotel)
#[1] "X"           "snotel.min"  "snotel.max"  "snotel.mean" "tsk.min"     "tsk.max"     "tsk.mean"    "t2.min"     
#[9] "t2.max"      "t2.mean"     "MOD11A2"     "MYD11A2"   

snotel$date <- as.Date(snotel$X, format = '%m/%d/%Y')
snotel <- select(snotel, date, snotel.min, snotel.max, snotel.mean, tsk.min, tsk.max, tsk.mean, t2.min, 
                 t2.max, t2.mean, MOD11A2, MYD11A2)

names(acis)
#[1] "X"         "acis.min"  "acis.max"  "acis.mean" "tsk.min"   "tsk.max"   "tsk.mean"  "t2.min"    "t2.max"   
#[10] "t2.mean"   "MOD11A2"   "MYD11A2"

acis$date <- as.Date(acis$X, format = '%m/%d/%Y')
acis <- select(acis, date, acis.min, acis.max, acis.mean, tsk.min, tsk.max, tsk.mean, t2.min, 
                 t2.max, t2.mean, MOD11A2, MYD11A2)

####################
####################


##Plot n play
#Min
plot(MOD11A2 ~ snotel.min, data = snotel, pch = 16, col = alpha('dodgerblue4', 0.7),
     xlab = expression(paste("Air Temperature [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
  min1 <- lm(MOD11A2 ~ snotel.min, data = snotel) #R2 = 0.84
  summary(min1) #R2 = 0.84
  
points(MOD11A2 ~ acis.min, data = acis, pch = 17, col = alpha('darkslategray2', 0.7)) 
  min2 <- lm(MOD11A2 ~ acis.min, data = acis)
  summary(min2) #R2 = 0.96
  
  abline(min1, lwd = 2, col = 'dodgerblue4')
  abline(min2, lwd = 2, col = 'darkslategray2', lty = 2)
  
legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('dodgerblue4', 'darkslategray2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 10, y = 0, 'Snotel' ~R^2 ~ '= 0.84', col = 'dodgerblue4')
text(x = 10, y = -3, 'ACIS' ~R^2 ~ '= 0.96', col = 'darkslategray4')      

####

  
    