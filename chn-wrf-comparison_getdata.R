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

colnames(loggers) = c('site', 'site.index', 'site.name', 'lon', 'lat')

####Bring in temp data
temps <- read.csv('all_temp.csv')

#Wide to tall
temps_df <- temps %>% 
  gather(site, stream.temp, -Date, -Year, -Julian)

colnames(temps_df) = c('ymd', 'yr', 'doy', 'site', 'stream.temp')

###get commonalities between loggers and temps_df to merge 
loggers$site.index[is.na(loggers$site.index)] <- as.character(loggers$site[is.na(loggers$site.index)])

temps_df$site <- gsub('X', '', temps_df$site)

unique(temps_df$site)
unique(loggers$site.index)

dups <-  as.data.frame(duplicated(loggers$site.index))
##there are dups of monument creek and jenny m creek...they are close enough spatially 
##can't figure out how to distinguish temp data to specific logger..just keep the first lcoation for each dup.
loggers <- cbind(loggers, dups)

loggers <- loggers %>% 
  filter(`duplicated(loggers$site.index)` == 'FALSE')

loggers <- select(loggers, -site, -`duplicated(loggers$site.index)`)

unique(temps_df$site)
#[1] "1100"    "1101"    "1102"    "1103"    "1104"    "1105"    "1107"    "1110"    "1113"    "1116"    "1117"    "1118"   
#[13] "1119"    "1120"    "1121"    "1122"    "1123"    "1124"    "1125"    "1126"    "1130"    "1136"    "1137"    "1139"   
#[25] "1140"    "1141"    "1142"    "1143"    "1144"    "1145"    "1146"    "1147"    "1148"    "1150"    "1157"    "1158"   
#[37] "C1.SIDE" "C2.SIDE" "C3.MS"   "C3.SIDE" "C4.MS"   "C4.SIDE" "C5.MS"   "C5.SIDE" "P2.SIDE" "P3.SIDE" "P4.SIDE" "P5.MS"  
#[49] "rkm11"   "rkm66"   "rkm76"   "rkm82"   "rkm86"   "rkm88"   "rkm90"   "rkm90V2" "rkm92"   "rkm104"  "rkm114"  "rkm128" 
#[61] "rkm140"  "rkm152"  "rkm164"  "rkm166"  "rkm167"  "rkm168"  "rkm170"  "rkm174"  "rkm180"  "rkm190"  "rkm233"

unique(loggers$site.index)
#[1] "1143"           "1124"           "1105"           "1125"           "1116"           "1102"           "1142"          
#[8] "1141"           "1122"           "1118"           "1145"           "1120"           "1119"           "1107"          
#[15] "1101"           "1123"           "1110"           "1100"           "1121"           "1117"           "1113"          
#[22] "1104"           "1130"           "1139"           "1146"           "1136"           "1150"           "1157"          
#[29] "1144"           "1137"           "1159"           "1148"           "1126"           "1147"           "1140"          
#[36] "11rkm"          "66rkm"          "76rkm"          "82rkm"          "86rkm"          "88rkm"          "90rkm"         
#[43] "90rkmV2"        "92rkm"          "104rkm"         "114rkm"         "128rkm"         "140rkm"         "152rkm"        
#[50] "164rkm"         "166rkm"         "167rkm"         "168rkm"         "170rkm"         "174rkm"         "180rkm"        
#[57] "190rkm"         "233rkm"         "C1-SIDE"        "C2-SIDE"        "C3-MS"          "C3-SIDE"        "C4-MS"         
#[64] "C4-SIDE"        "C5-MS"          "C5-SIDE"        "P2-SIDE"        "P3-SIDE"        "P4-SIDE"        "P5-MS"         
#[71] "TwoRiversGauge"

##Clean up site.index to match temps_df$site
loggers$site.index[loggers$site.index =='11rkm'] = 'rkm11'
loggers$site.index[loggers$site.index =='66rkm'] = 'rkm66'
loggers$site.index[loggers$site.index =='76rkm'] = 'rkm76'
loggers$site.index[loggers$site.index =='82rkm'] = 'rkm82'
loggers$site.index[loggers$site.index =='86rkm'] = 'rkm86'
loggers$site.index[loggers$site.index =='88rkm'] = 'rkm88'
loggers$site.index[loggers$site.index =='90rkm'] = 'rkm90'
loggers$site.index[loggers$site.index =='90rkmv2'] = 'rkm90v2'
loggers$site.index[loggers$site.index =='92rkm'] = 'rkm92'
loggers$site.index[loggers$site.index =='104rkm'] = 'rkm104'
loggers$site.index[loggers$site.index =='114rkm'] = 'rkm114'
loggers$site.index[loggers$site.index =='128rkm'] = 'rkm128'
loggers$site.index[loggers$site.index =='140rkm'] = 'rkm140'
loggers$site.index[loggers$site.index =='152rkm'] = 'rkm152'
loggers$site.index[loggers$site.index =='164rkm'] = 'rkm164'
loggers$site.index[loggers$site.index =='166rkm'] = 'rkm166'
loggers$site.index[loggers$site.index =='167rkm'] = 'rkm167'
loggers$site.index[loggers$site.index =='168rkm'] = 'rkm168'
loggers$site.index[loggers$site.index =='170rkm'] = 'rkm170'
loggers$site.index[loggers$site.index =='174rkm'] = 'rkm174'
loggers$site.index[loggers$site.index =='180rkm'] = 'rkm180'
loggers$site.index[loggers$site.index =='190rkm'] = 'rkm190'
loggers$site.index[loggers$site.index =='233rkm'] = 'rkm233'
loggers$site.index[loggers$site.index =='C1-SIDE'] = 'C1.SIDE'
loggers$site.index[loggers$site.index =='C2-SIDE'] = 'C2.SIDE'
loggers$site.index[loggers$site.index =='C3-SIDE'] = 'C3.SIDE'
loggers$site.index[loggers$site.index =='C4-SIDE'] = 'C4.SIDE'
loggers$site.index[loggers$site.index =='C5-SIDE'] = 'C5.SIDE'
loggers$site.index[loggers$site.index =='C3-MS'] = 'C3.MS'
loggers$site.index[loggers$site.index =='C5-MS'] = 'C5.MS'
loggers$site.index[loggers$site.index =='P2-SIDE'] = 'P2.SIDE'
loggers$site.index[loggers$site.index =='P3-SIDE'] = 'P3.SIDE'
loggers$site.index[loggers$site.index =='P4-SIDE'] = 'P4.SIDE'
loggers$site.index[loggers$site.index =='P5-MS'] = 'P5.MS'
  ##As per all-looger-chena.xls (huntsman)
loggers$site.index[loggers$site.index =='1159'] = '1158'

###join
temps_df2 <- left_join(temps_df, loggers, by = c('site' = 'site.index'))

####Clean it up mo'
temps_df3 <- select(temps_df2, -yr, -doy)
              
temps_df3 <-   temps_df3 %>% 
  separate(ymd, c('month', 'day', 'year')) 

temps_df4 <- temps_df3 %>% 
  unite(date, year, month, day, sep = '-', remove = F) %>% 
  unite(monthday, month, day, sep = '-', remove = F)

temps_df4$date <- as.Date(temps_df4$date)
temps_df4$monthday <- as.Date(temps_df4$monthday, "%m-%d") ###year added in...fix if needed, monthday is borrowed from previous script

#get DOY
tmp <- as.POSIXlt(temps_df4$date)

doy <- as.data.frame(tmp)
doy$doy <- tmp$yday
#check with doy in full_df...leap year? day off...POSIXlt is 0:365
doy$doy <- tmp$yday + 1
##day off after Feb28...for now
doy$doy2 <- ifelse(doy$doy>59, doy$doy +1, doy$doy)

#add to temp df
temps_df4$doy <- doy$doy2

#######################################################
#####Get grid cells for each point
###https://gis.stackexchange.com/questions/88830/overlay-a-spatial-polygon-with-a-grid-and-check-in-which-grid-element-specific-c

bb <- bbox(gridpolygon)

cs <- c(20000, 20000)

cc <- bb[,1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)

grd <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)
grd

sp_grd <- SpatialGridDataFrame(grd, data = data.frame(id=1:prod(cd)),
                               proj4string = CRS(proj4string(logs.loc)))
summary(sp_grd)


colnames(logs.coords) <- c('x', 'y')

coordinates(logs.coords) <- ~ x + y
proj4string(logs.coords) <- proj4string(logs.loc)

over(logs.coords, sp_grd)

library('lattice')

spplot(sp_grd, "id",
       panel = function(...) {
         panel.gridplot(..., border="black")
         sp.polygons(logs.loc)
         sp.polygons(basin.out)
         sp.points(logs.coords, cex=1.5)
         panel.text(...)
       })

plot(gridpolygon)
plot(basin.out, add = T)
plot(logs.loc, add = T)
#plot(chena, add = T)

#Looks good, now assign cells to df's


#####and so on and so forth to match full_df.
###Create raster?, assign cell to sites, 8 day averages to match full_df (or just compare)?
         