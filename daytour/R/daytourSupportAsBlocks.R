# import sf map data -------
{
  econDV2::Map() -> mp
  mp$sf$get_sf_taiwan_simplified() -> sf_taiwan
  sf_taiwan_simplified$台灣本島$鄉鎮區 -> sf_taiwanCounty
  sf_taiwanCounty |>
    dplyr::filter(
      name %in% paste0(c(
        "樹林","土城","板橋"
      ),"區")
    ) -> sf_dayTour
}
{
  gpxDataRaw <- gpx::read_gpx("./data/daytour.gpx")
  gpxData <- gpxDataRaw[["tracks"]][[1]]
}
{
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
  plt$make()
}
{
  # [Setup Google Map API](https://github.com/dkahle/ggmap#google-maps-api-key)
  # Save your api key for everyone who use your current computer:
  # Sys.setenv("googleMapApiKey"="xxxxxx")
  # Register your API key:
  # ggmap::register_google(key=Sys.getenv("googleMapApiKey"))
  dfLocations = 
    data.frame(
      address=c(
        "新北市三峽區大學路151號, Taiwan",
        "台北101, Taiwan")
    )
  
  pointAgps = ggmap::geocode(dfLocations$address[[1]])
  pointBgps = ggmap::geocode(dfLocations$address[[2]])
  
  dfLocations |>
    ggmap::mutate_geocode(
      address
    ) -> dfLocations
  
  distanceA2B_driving = 
    ggmap::mapdist(
      from=dfLocations$address[[1]], to=dfLocations$address[[2]], mode="driving"
    )
  
  distanceA2B_driving
}
{
  gpxData |>
    dplyr::summarise(
      # colnames must be lon and lat
      lon=mean(Longitude),
      lat=mean(Latitude)
    ) -> gpsDataSummary
  

  
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
  
}
pltMap$make()

# magick ------
{
  # https://photos.app.goo.gl/qpi6hjJsfCe9GxL67
  photoPath = "/Users/martin/Downloads/IMG_0195.HEIC"
  
  img = magick::image_read(photoPath)
  
  magick::image_info(img) 
  imgData = magick::image_data(img)
  
}
# photo meta ----------
{
  list.files("./photos", full.names = T) |>
    econDV2::getMetaDataFromFiles(gpsOnly = T) -> dfMeta
  
  # prepare for ggmap overlay
  names(dfMeta)[c(2,3)] <- c("lat", "lon")
  dfMeta$number = 1:nrow(dfMeta)
}
# ggmap overlay -------
{
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
  pltMap$make()
}
# table of images ----
## prepare png photo file path: must under project root
{
  library(kableExtra)
  dfMeta |> dplyr::glimpse()
  stringr::str_extract(
    dfMeta$filename, "\\.[^\\.]+(\\.HEIC)"
  ) |>
    stringr::str_replace("photos","png") |>
    stringr::str_replace("HEIC","png") ->
    dfMeta$pngfile
}
## Generate 
{
  dfMeta4Table <- data.frame(
   number = 1:5,
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
       
}

# table overlay -----
{
  patchDaytourMapWithPhotoTable <- function(patchwork, png, readPNG, pltMap, plot_spacer, inset_element) {
    library(patchwork)
  
    table1 <- png::readPNG('table1.png', native = TRUE)
    pltMap$make() + patchwork::plot_spacer() + 
      patchwork::inset_element(p = table1, left =0, bottom = 0, right = 0.4, top = 1)
  }
}
