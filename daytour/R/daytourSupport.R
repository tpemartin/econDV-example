getDayTourSfData <- function() {
  econDV2::Map() -> mp
  mp$sf$get_sf_taiwan_simplified() -> sf_taiwan
  sf_taiwan_simplified$台灣本島$鄉鎮區 -> sf_taiwanCounty
  sf_taiwanCounty |>
    dplyr::filter(
      name %in% paste0(c(
        "樹林","土城","板橋"
      ),"區")
    ) -> sf_dayTour
  return(sf_dayTour)
}

importGPXdata <- function() {
  gpxDataRaw <- gpx::read_gpx("./data/daytour.gpx")
  gpxData <- gpxDataRaw[["tracks"]][[1]]
  return(gpxData)
}
plotGPXoverlaySf <- function(sf_dayTour, gpxData) {
  plt = econDV2::Plot()
  plt$ggplot = ggplot()
  plt$geom = list(
    geom_sf(data=sf_dayTour,color="white")
  )
  plt$geom2 = list(
    geom_path(
      data=gpxData,
      mapping=aes(
        y=Latitude, x=Longitude
      ),
      color="red",
      inherit.aes = FALSE
    )
  )
}
getGpsDataSummary = function(gpxData){
  gpxData |>
    dplyr::summarise(
      # colnames must be lon and lat
      lon=mean(Longitude),
      lat=mean(Latitude)
    ) -> gpsDataSummary
  
  return(gpsDataSummary)
}
plotDaytourOverylayGoogleMap = function(gpsDataSummary, gpxData){
  ggmap::get_map(
    location = gpsDataSummary,
    source = "google",
    # Visit Google Map and look at z value
    zoom=14
  ) -> googleMap
  
  pltMap = econDV2::Plot()
  
  # just google map
  pltMap$ggplot = ggmap::ggmap(
    googleMap
  )
  
  # put day tour route
  ## Make ground map pale yellow
  pltMap$ggplot = ggmap::ggmap(
    ggmap = googleMap,
    darken = c(0.5, "#FCFFE7")
  ) 
  
  pltMap$geom = list(
    geom_path(
      # gps colnames must be lon, lat
      data=gpxData |> dplyr::rename("lon"="Longitude","lat"="Latitude"),
      linewidth=2, color="white"
    ),
    geom_path(
      # gps colnames must be lon, lat
      data=gpxData |> dplyr::rename("lon"="Longitude","lat"="Latitude"),
      linewidth=1, color="dodgerblue2"
    ))
  
  pltMap$theme = theme_void()
  return(pltMap)
}
getDfMetaFromPhotos_old = function(){
  list.files("./photos", full.names = T) |>
    econDV2::getMetaDataFromFiles(gpsOnly = T) -> dfMeta
  
  # prepare for ggmap overlay
  names(dfMeta)[c(2,3)] <- c("lat", "lon")
  dfMeta$number = 1:nrow(dfMeta)
  return(dfMeta)
}
getDfMetaFromPhotos = function(){
  googleDriveUrl = "https://drive.google.com/drive/u/0/folders/1ldcrE5XGgN96qImQ7oYslTQCG2ccohNd"
  ph = econDV2::Photos(googleDriveUrl)
  ph$getGPS() -> dfMeta
  
  names(dfMeta)[1:2] <- c("lat","lon")
  dfMeta$number = nrow(dfMeta):1
  return(dfMeta)
}
addPhotoPointLayer = function(dfMeta, pltMap){
  pltMap$geom2 = 
    list(
      geom_point(
        data=dfMeta,
        color="dodgerblue3",
        size=6 #input$size
      ),
      geom_text(
        data=dfMeta,
        mapping=aes(
          label=number
        ), color="white"
      ))
  return(pltMap)
}
prepareDf4KableExtra = function(dfMeta){
  
  dfMeta$filename |>
    stringr::str_replace("photos","png") |>
    stringr::str_replace("HEIC","png") ->
    dfMeta$pngfile
  
  file.path("./png",dfMeta$pngfile) -> dfMeta$pngfile
  
  return(dfMeta)
}
prepareExtraTable <- function(dfMeta) {
  dfMeta4Table <- data.frame(
    number = 5:1,
    picture = ""
  )
  dfMeta4Table |>
    kableExtra::kable()  |>
    kableExtra::kable_paper(full_width=F) |>
    kableExtra::column_spec(
      2, image = kableExtra::spec_image(
        normalizePath(dfMeta$pngfile), 400,400)
    ) |>
    kableExtra::save_kable(file="table1.png")
  return(TRUE)
}
patchDaytourMapWithPhotoTable <- function(flag_finish, pltMap) {
  library(patchwork)
  
  table1 <- png::readPNG('table1.png', native = TRUE)
  pltMap$make() + patchwork::plot_spacer() + 
    patchwork::inset_element(p = table1, left =0, bottom = 0, right = 0.4, top = 1) -> finalPlot
  return(finalPlot)

  }


