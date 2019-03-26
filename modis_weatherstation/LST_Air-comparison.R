rm(list = ls())

library(dplyr)
library(tidyr) #this masks 'extract' from raster (?)
library(ggplot2)


##Load data
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/serdp-streamtemp/modis_weatherstation/')

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

##Plot n play##
##MODIS vs AIR
par(mfrow =c(3, 1), mar = c(4, 4, 1, 1), oma = c(1, 1, 2, 0))
#Min
plot(MOD11A2 ~ snotel.min, data = snotel, pch = 16, col = alpha('dodgerblue4', 0.7),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Minimum Air Temperature [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
  min1 <- lm(MOD11A2 ~ snotel.min, data = snotel) #R2 = 0.84
  summary(min1) #R2 = 0.84
  
points(MOD11A2 ~ acis.min, data = acis, pch = 17, col = alpha('darkslategray2', 0.7)) 
  min2 <- lm(MOD11A2 ~ acis.min, data = acis)
  summary(min2) #R2 = 0.96
  
  abline(min1, lwd = 2, col = 'dodgerblue4')
  abline(min2, lwd = 2, col = 'darkslategray2', lty = 2)
  abline(0, 1, col = 'gray24', lty = 3, lwd = 2)
  
legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('dodgerblue4', 'darkslategray2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.84', col = 'dodgerblue4')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.96', col = 'darkturquoise')
mtext(side = 3, line = 1, font = 2, 'Observed Air Temp vs. MODIS LST')

#Max
plot(MOD11A2 ~ snotel.max, data = snotel, pch = 16, col = alpha('firebrick3', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Maximum Air Temperature [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
max1 <- lm(MOD11A2 ~ snotel.max, data = snotel) 
summary(max1) #R2 = 0.86

points(MOD11A2 ~ acis.max, data = acis, pch = 17, col = alpha('deeppink2', 0.5)) 
max2 <- lm(MOD11A2 ~ acis.max, data = acis)
summary(max2) #R2 = 0.98

abline(max1, lwd = 2, col = 'firebrick3')
abline(max2, lwd = 2, col = 'deeppink2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('firebrick3', 'deeppink2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.86', col = 'firebrick3')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.98', col = 'deeppink2') 

#Mean
plot(MOD11A2 ~ snotel.mean, data = snotel, pch = 16, col = alpha('forestgreen', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Mean Air Temperature [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
mean1 <- lm(MOD11A2 ~ snotel.mean, data = snotel) 
summary(mean1) #R2 = 0.71

points(MOD11A2 ~ acis.mean, data = acis, pch = 17, col = alpha('darkseagreen3', 0.5)) 
mean2 <- lm(MOD11A2 ~ acis.mean, data = acis)
summary(mean2) #R2 = 0.98

abline(mean1, lwd = 2, col = 'forestgreen')
abline(mean2, lwd = 2, col = 'darkseagreen3', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('forestgreen', 'darkseagreen3'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.71', col = 'forestgreen')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.98', col = 'darkseagreen3') 

##WRF vs AIR
#Min
plot(t2.min ~ snotel.min, data = snotel, pch = 16, col = alpha('dodgerblue4', 0.7),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Minimum Air Temperature [",degree,"C]")), 
     ylab = expression(paste("WRF-Predicted Temp [",degree,"C]")))
min1 <- lm(t2.min ~ snotel.min, data = snotel) 
summary(min1) #R2 = 0.85

points(t2.min ~ acis.min, data = acis, pch = 17, col = alpha('darkslategray2', 0.7)) 
min2 <- lm(t2.min ~ acis.min, data = acis)
summary(min2) #R2 = 0.96

abline(min1, lwd = 2, col = 'dodgerblue4')
abline(min2, lwd = 2, col = 'darkslategray2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('dodgerblue4', 'darkslategray2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.85', col = 'dodgerblue4')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.96', col = 'darkturquoise')
mtext(side = 3, line = 1, font = 2, 'Observed Air Temp vs. WRF-Predicted Air Temp')

#Max
plot(t2.min ~ snotel.max, data = snotel, pch = 16, col = alpha('firebrick3', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Maximum Air Temperature [",degree,"C]")), 
     ylab = expression(paste("WRF-Predicted Temp [",degree,"C]")))
max1 <- lm(t2.min ~ snotel.max, data = snotel) 
summary(max1) #R2 = 0.81

points(t2.min ~ acis.max, data = acis, pch = 17, col = alpha('deeppink2', 0.5)) 
max2 <- lm(t2.min ~ acis.max, data = acis)
summary(max2) #R2 = 0.92

abline(max1, lwd = 2, col = 'firebrick3')
abline(max2, lwd = 2, col = 'deeppink2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('firebrick3', 'deeppink2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.81', col = 'firebrick3')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.92', col = 'deeppink2') 

#mean
plot(t2.min ~ snotel.mean, data = snotel, pch = 16, col = alpha('forestgreen', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Mean Air Temperature [",degree,"C]")), 
     ylab = expression(paste("WRF-Predicted Temp [",degree,"C]")))
mean1 <- lm(t2.min ~ snotel.mean, data = snotel) 
summary(mean1) #R2 = 0.68

points(t2.min ~ acis.mean, data = acis, pch = 17, col = alpha('darkseagreen3', 0.5)) 
mean2 <- lm(t2.min ~ acis.mean, data = acis)
summary(mean2) #R2 = 0.95

abline(mean1, lwd = 2, col = 'forestgreen')
abline(mean2, lwd = 2, col = 'darkseagreen3', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('forestgreen', 'darkseagreen3'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.68', col = 'forestgreen')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.95', col = 'darkseagreen3') 

##MODIS vs WRF  
#Min
plot(MOD11A2 ~ t2.min, data = snotel, pch = 16, col = alpha('dodgerblue4', 0.7),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("WRF-Predicted Temp [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
min1 <- lm(MOD11A2 ~ t2.min, data = snotel) 
summary(min1) #R2 = 0.93

points(MOD11A2 ~ acis.min, data = acis, pch = 17, col = alpha('darkslategray2', 0.7)) 
min2 <- lm(MOD11A2 ~ acis.min, data = acis)
summary(min2) #R2 = 0.96

abline(min1, lwd = 2, col = 'dodgerblue4')
abline(min2, lwd = 2, col = 'darkslategray2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('dodgerblue4', 'darkslategray2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.93', col = 'dodgerblue4')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.96', col = 'darkturquoise')
mtext(side = 3, line = 1, font = 2, 'WRF-Predicted Temp vs. MODIS LST')

#Max
plot(MOD11A2 ~ t2.max, data = snotel, pch = 16, col = alpha('firebrick3', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("WRF-Predicted Temp [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
max1 <- lm(MOD11A2 ~ t2.max, data = snotel) 
summary(max1) #R2 = 0.90

points(MOD11A2 ~ acis.max, data = acis, pch = 17, col = alpha('deeppink2', 0.5)) 
max2 <- lm(MOD11A2 ~ acis.max, data = acis)
summary(max2) #R2 = 0.98

abline(max1, lwd = 2, col = 'firebrick3')
abline(max2, lwd = 2, col = 'deeppink2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('firebrick3', 'deeppink2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.90', col = 'firebrick3')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.98', col = 'deeppink2')

#Mean
plot(MOD11A2 ~ t2.mean, data = snotel, pch = 16, col = alpha('forestgreen', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("WRF-Predicted Temp [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
mean1 <- lm(MOD11A2 ~ t2.mean, data = snotel) 
summary(mean1) #R2 = 0.95

points(MOD11A2 ~ acis.mean, data = acis, pch = 17, col = alpha('darkseagreen3', 0.5)) 
mean2 <- lm(MOD11A2 ~ acis.mean, data = acis)
summary(mean2) #R2 = 0.98

abline(mean1, lwd = 2, col = 'forestgreen')
abline(mean2, lwd = 2, col = 'darkseagreen3', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('forestgreen', 'darkseagreen3'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.95', col = 'forestgreen')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.98', col = 'darkseagreen3') 

######################
######################
##explore for only period of interest (say, April 15 - Nov 1?)

#snotel
snotel.season <- separate(snotel, date, c("y","m","d"), remove = FALSE)
snotel.season <- unite(snotel.season, md, m, d, sep = '/')
class(snotel.season$md)
snotel.season$md <- as.Date(snotel.season$md, format = '%m/%d') #Adds a year, but doesn't matter for this
snotel.season <- filter(snotel.season, md > '2019-04-14' & md < '2019-11-02')

#acis
acis.season <- separate(acis, date, c("y","m","d"), remove = FALSE)
acis.season <- unite(acis.season, md, m, d, sep = '/')
class(acis.season$md)
acis.season$md <- as.Date(acis.season$md, format = '%m/%d') #Adds a year, but doesn't matter for this
acis.season <- filter(acis.season, md > '2019-04-14' & md < '2019-11-02')

###########
###########

##Plot n play#####SEASON DATA
##MODIS vs AIR
par(mfrow =c(3, 1), mar = c(4, 4, 1, 1), oma = c(1, 1, 2, 0))
#Min
plot(MOD11A2 ~ snotel.min, data = snotel.season, pch = 16, col = alpha('dodgerblue4', 0.7),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Minimum Air Temperature [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
min1 <- lm(MOD11A2 ~ snotel.min, data = snotel.season) #R2 = 0.84
summary(min1) #R2 = 0.77

points(MOD11A2 ~ acis.min, data = acis.season, pch = 17, col = alpha('darkslategray2', 0.7)) 
min2 <- lm(MOD11A2 ~ acis.min, data = acis.season)
summary(min2) #R2 = 0.90

abline(min1, lwd = 2, col = 'dodgerblue4')
abline(min2, lwd = 2, col = 'darkslategray2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('dodgerblue4', 'darkslategray2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.77', col = 'dodgerblue4')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.90', col = 'darkturquoise')
mtext(side = 3, line = 1, font = 2, 'Observed Seasonal Air Temp vs. MODIS LST')

#Max
plot(MOD11A2 ~ snotel.max, data = snotel.season, pch = 16, col = alpha('firebrick3', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Maximum Air Temperature [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
max1 <- lm(MOD11A2 ~ snotel.max, data = snotel.season) 
summary(max1) #R2 = 0.80

points(MOD11A2 ~ acis.max, data = acis.season, pch = 17, col = alpha('deeppink2', 0.5)) 
max2 <- lm(MOD11A2 ~ acis.max, data = acis.season)
summary(max2) #R2 = 0.97

abline(max1, lwd = 2, col = 'firebrick3')
abline(max2, lwd = 2, col = 'deeppink2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('firebrick3', 'deeppink2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.80', col = 'firebrick3')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.97', col = 'deeppink2') 

#Mean
plot(MOD11A2 ~ snotel.mean, data = snotel.season, pch = 16, col = alpha('forestgreen', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Mean Air Temperature [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
mean1 <- lm(MOD11A2 ~ snotel.mean, data = snotel.season) 
summary(mean1) #R2 = 0.52

points(MOD11A2 ~ acis.mean, data = acis.season, pch = 17, col = alpha('darkseagreen3', 0.5)) 
mean2 <- lm(MOD11A2 ~ acis.mean, data = acis.season)
summary(mean2) #R2 = 0.96

abline(mean1, lwd = 2, col = 'forestgreen')
abline(mean2, lwd = 2, col = 'darkseagreen3', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('forestgreen', 'darkseagreen3'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.52', col = 'forestgreen')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.96', col = 'darkseagreen3') 

##WRF vs AIR
#Min
plot(t2.min ~ snotel.min, data = snotel.season, pch = 16, col = alpha('dodgerblue4', 0.7),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Minimum Air Temperature [",degree,"C]")), 
     ylab = expression(paste("WRF-Predicted Temp [",degree,"C]")))
min1 <- lm(t2.min ~ snotel.min, data = snotel.season) 
summary(min1) #R2 = 0.76

points(t2.min ~ acis.min, data = acis.season, pch = 17, col = alpha('darkslategray2', 0.7)) 
min2 <- lm(t2.min ~ acis.min, data = acis.season)
summary(min2) #R2 = 0.94

abline(min1, lwd = 2, col = 'dodgerblue4')
abline(min2, lwd = 2, col = 'darkslategray2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('dodgerblue4', 'darkslategray2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.76', col = 'dodgerblue4')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.94', col = 'darkturquoise')
mtext(side = 3, line = 1, font = 2, 'Observed Seasonal Air Temp vs. WRF-Predicted Air Temp')

#Max
plot(t2.min ~ snotel.max, data = snotel.season, pch = 16, col = alpha('firebrick3', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Maximum Air Temperature [",degree,"C]")), 
     ylab = expression(paste("WRF-Predicted Temp [",degree,"C]")))
max1 <- lm(t2.min ~ snotel.max, data = snotel.season) 
summary(max1) #R2 = 0.73

points(t2.min ~ acis.max, data = acis.season, pch = 17, col = alpha('deeppink2', 0.5)) 
max2 <- lm(t2.min ~ acis.max, data = acis.season)
summary(max2) #R2 = 0.90

abline(max1, lwd = 2, col = 'firebrick3')
abline(max2, lwd = 2, col = 'deeppink2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('firebrick3', 'deeppink2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.73', col = 'firebrick3')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.90', col = 'deeppink2') 

#mean
plot(t2.min ~ snotel.mean, data = snotel.season, pch = 16, col = alpha('forestgreen', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("Mean Air Temperature [",degree,"C]")), 
     ylab = expression(paste("WRF-Predicted Temp [",degree,"C]")))
mean1 <- lm(t2.min ~ snotel.mean, data = snotel.season) 
summary(mean1) #R2 = 0.46

points(t2.min ~ acis.mean, data = acis.season, pch = 17, col = alpha('darkseagreen3', 0.5)) 
mean2 <- lm(t2.min ~ acis.mean, data = acis.season)
summary(mean2) #R2 = 0.94

abline(mean1, lwd = 2, col = 'forestgreen')
abline(mean2, lwd = 2, col = 'darkseagreen3', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('forestgreen', 'darkseagreen3'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.46', col = 'forestgreen')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.94', col = 'darkseagreen3') 

##MODIS vs WRF  
#Min
plot(MOD11A2 ~ t2.min, data = snotel.season, pch = 16, col = alpha('dodgerblue4', 0.7),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("WRF-Predicted Temp [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
min1 <- lm(MOD11A2 ~ t2.min, data = snotel.season) 
summary(min1) #R2 = 0.90

points(MOD11A2 ~ acis.min, data = acis.season, pch = 17, col = alpha('darkslategray2', 0.7)) 
min2 <- lm(MOD11A2 ~ acis.min, data = acis.season)
summary(min2) #R2 = 0.90

abline(min1, lwd = 2, col = 'dodgerblue4')
abline(min2, lwd = 2, col = 'darkslategray2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('dodgerblue4', 'darkslategray2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.90', col = 'dodgerblue4')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.90', col = 'darkturquoise')
mtext(side = 3, line = 1, font = 2, 'WRF-Predicted Seasonal Temp vs. MODIS LST')

#Max
plot(MOD11A2 ~ t2.max, data = snotel.season, pch = 16, col = alpha('firebrick3', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("WRF-Predicted Temp [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
max1 <- lm(MOD11A2 ~ t2.max, data = snotel.season) 
summary(max1) #R2 = 0.94

points(MOD11A2 ~ acis.max, data = acis.season, pch = 17, col = alpha('deeppink2', 0.5)) 
max2 <- lm(MOD11A2 ~ acis.max, data = acis.season)
summary(max2) #R2 = 0.97

abline(max1, lwd = 2, col = 'firebrick3')
abline(max2, lwd = 2, col = 'deeppink2', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('firebrick3', 'deeppink2'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.94', col = 'firebrick3')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.97', col = 'deeppink2')

#Mean
plot(MOD11A2 ~ t2.mean, data = snotel.season, pch = 16, col = alpha('forestgreen', 0.5),
     xlim = c(-40, 40), ylim = c(-40, 40),
     xlab = expression(paste("WRF-Predicted Temp [",degree,"C]")), 
     ylab = expression(paste("MODIS LST [",degree,"C]")))
mean1 <- lm(MOD11A2 ~ t2.mean, data = snotel.season) 
summary(mean1) #R2 = 0.94

points(MOD11A2 ~ acis.mean, data = acis.season, pch = 17, col = alpha('darkseagreen3', 0.5)) 
mean2 <- lm(MOD11A2 ~ acis.mean, data = acis.season)
summary(mean2) #R2 = 0.96

abline(mean1, lwd = 2, col = 'forestgreen')
abline(mean2, lwd = 2, col = 'darkseagreen3', lty = 2)
abline(0, 1, col = 'gray24', lty = 3, lwd = 2)

legend('topleft', 
       legend = c('Snotel', 'ACIS'),
       col = c('forestgreen', 'darkseagreen3'),
       pch = c(16, 17),
       bty = 'n')
text(x = 25, y = 0, 'Snotel' ~R^2 ~ '= 0.94', col = 'forestgreen')
text(x = 25, y = -9, 'ACIS' ~R^2 ~ '= 0.96', col = 'darkseagreen3') 
