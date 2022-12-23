library(targets)
library(econDV2)
source("R/daytourSupport.R")
tar_option_set(packages=c("ggplot2","dplyr","sf","osmdata"))

list(
  sf_dayTour%t=%getDayTourSfData(),
  gpxData %t=% importGPXdata(),
  gpxDataSummary %t=% getGpsDataSummary(gpxData),
  dfMeta %t=% getDfMetaFromPhotos(),
  # dfMeta4Kable %t=% prepareDf4KableExtra(dfMeta),
  flag_finish %t=% prepareDf4KableExtra(dfMeta),
  
  pltMap %t=% plotDaytourOverylayGoogleMap(gpxDataSummary, gpxData),
  plotDayTour0 %t=% plotGPXoverlaySf(sf_dayTour, gpxData),
  pltMapAddPhotoPoints %t=% addPhotoPointLayer(dfMeta, pltMap),
  pltMapFinal %t=% patchDaytourMapWithPhotoTable(flag_finish, pltMapAddPhotoPoints)
)
