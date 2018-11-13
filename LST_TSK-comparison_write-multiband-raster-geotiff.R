rm(list = ls())

library(raster)
library(rgdal)

#####Get MODIS files
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/modis_lst_3338_singleband/')

#MOD
mod_brick <- do.call(brick, lapply(list.files(path = './', pattern = "MOD11A2*.*tif"), raster))

#MYD
myd_brick <- do.call(brick, lapply(list.files(path = './', pattern = "MYD11A2*.*tif"), raster))

####Write multiband for later use; .tif files lose names (?) where I would extract date?...raster keeps, doing both here. 
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/lst_tsk_multiband/')

#MOD
writeRaster(mod_brick, 'MOD11A2_multiband.tif', overwrite = T)
writeRaster(mod_brick, 'MOD11A2_multiband.grd', overwrite = T, format = "raster")

writeRaster(myd_brick, 'MYD11A2_multiband.tif', overwrite = T)
writeRaster(myd_brick, 'MYD11A2_multiband.grd', overwrite = T, format = "raster")

#Get WRF files
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/wrf_tsk_3338_singleband/')

#ERA_MOD
era_mod_brick <- do.call(brick, lapply(list.files(path = './', pattern = "*ERA-Interim_historical_MOD11A2*.*tif"), raster))

#ERA_MYD
era_myd_brick <- do.call(brick, lapply(list.files(path = './', pattern = "*ERA-Interim_historical_MYD11A2*.*tif"), raster))

#CCSM_RCP_MOD
ccsm_rcp85_mod_brick <- do.call(brick, lapply(list.files(path = './', pattern = "*NCAR-CCSM4_rcp85_MOD11A2*.*tif"), raster))

#CCSM_RCP_MYD
ccsm_rcp85_myd_brick <- do.call(brick, lapply(list.files(path = './', pattern = "*NCAR-CCSM4_rcp85_MYD11A2*.*tif"), raster))

#CCSM_HIST_MOD
ccsm_hist_mod_brick <- do.call(brick, lapply(list.files(path = './', pattern = "*NCAR-CCSM4_historical_MOD11A2*.*tif"), raster))

#CCSM_HIST_MYD
ccsm_hist_myd_brick <- do.call(brick, lapply(list.files(path = './', pattern = "*NCAR-CCSM4_historical_MYD11A2*.*tif"), raster))

#GFDL_RCP_MOD
gfdl_rcp_mod_brick <- do.call(brick, lapply(list.files(path = './', pattern = "*GFDL-CM3_rcp85_MOD11A2*.*tif"), raster))

#GFDL_RCP_MYD
gfdl_rcp_myd_brick <- do.call(brick, lapply(list.files(path = './', pattern = "*GFDL-CM3_rcp85_MYD11A2*.*tif"), raster))

#GFDL_HIST_MOD
gfdl_HIST_mod_brick <- do.call(brick, lapply(list.files(path = './', pattern = "*GFDL-CM3_historical_MOD11A2*.*tif"), raster))

#GFDL_HIST_MYD
gfdl_HIST_myd_brick <- do.call(brick, lapply(list.files(path = './', pattern = "*GFDL-CM3_historical_MYD11A2*.*tif"), raster))

####Write multiband for later use
setwd('C:/Users/slklobucar/Documents/PostDoc_UAF/BorealFishFire/LST/lst_tsk_multiband/')

#ERA
writeRaster(era_mod_brick, "ERA-Interim_historical_MOD11A2_multiband.tif", overwrite = T)
writeRaster(era_myd_brick, "ERA-Interim_historical_MYD11A2_multiband.tif", overwrite = T)

writeRaster(era_mod_brick, "ERA-Interim_historical_MOD11A2_multiband.grd", overwrite = T, format = "raster")
writeRaster(era_myd_brick, "ERA-Interim_historical_MYD11A2_multiband.grd", overwrite = T, format = "raster")

#CCSM_RCP
writeRaster(ccsm_rcp85_mod_brick, "NCAR-CCSM4_rcp85_MOD11A2_multiband.tif", overwrite = T)
writeRaster(ccsm_rcp85_myd_brick, "NCAR-CCSM4_rcp85_MYD11A2_multiband.tif", overwrite = T)

writeRaster(ccsm_rcp85_mod_brick, "NCAR-CCSM4_rcp85_MOD11A2_multiband.grd", overwrite = T, format = "raster")
writeRaster(ccsm_rcp85_myd_brick, "NCAR-CCSM4_rcp85_MYD11A2_multiband.grd", overwrite = T, format = "raster")

#CCSM_HIST
writeRaster(ccsm_hist_mod_brick, "NCAR-CCSM4_historical_MOD11A2_multiband.tif", overwrite = T)
writeRaster(ccsm_hist_myd_brick, "NCAR-CCSM4_historical_MYD11A2_multiband.tif", overwrite = T)

writeRaster(ccsm_hist_mod_brick, "NCAR-CCSM4_historical_MOD11A2_multiband.grd", overwrite = T, format = "raster")
writeRaster(ccsm_hist_myd_brick, "NCAR-CCSM4_historical_MYD11A2_multiband.grd", overwrite = T, format = "raster")

#GFDL_RCP
writeRaster(gfdl_rcp_mod_brick, "GFDL-CM3_rcp85_MOD11A2_multiband.tif", overwrite = T)
writeRaster(gfdl_rcp_myd_brick, "GFDL-CM3_rcp85_MYD11A2_multiband.tif", overwrite = T)

writeRaster(gfdl_rcp_mod_brick, "GFDL-CM3_rcp85_MOD11A2_multiband.grd", overwrite = T, format = "raster")
writeRaster(gfdl_rcp_myd_brick, "GFDL-CM3_rcp85_MYD11A2_multiband.grd", overwrite = T, format = "raster")

#GFDL_HIST
writeRaster(gfdl_HIST_mod_brick, "GFDL-CM3_historical_MOD11A2_multiband.tif", overwrite = T)
writeRaster(gfdl_HIST_myd_brick, "GFDL-CM3_historical_MYD11A2_multiband.tif", overwrite = T)

writeRaster(gfdl_HIST_mod_brick, "GFDL-CM3_historical_MOD11A2_multiband.grd", overwrite = T, format = "raster")
writeRaster(gfdl_HIST_myd_brick, "GFDL-CM3_historical_MYD11A2_multiband.grd", overwrite = T, format = "raster")
