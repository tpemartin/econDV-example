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
  ggmap::register_google(key=Sys.getenv("googleMapApiKey"))
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
  
  # just google map
  ggmap::ggmap(
    googleMap
  )
  
  # put day tour route
  ggmap::ggmap(
    ggmap = googleMap,
    darken = c(0.5, "#FCFFE7")
  ) +
    geom_path(
      # gps colnames must be lon, lat
      data=gpxData |> dplyr::rename("lon"="Longitude","lat"="Latitude"),
      linewidth=2, color="white"
    ) +
    geom_path(
      # gps colnames must be lon, lat
      data=gpxData |> dplyr::rename("lon"="Longitude","lat"="Latitude"),
      linewidth=1, color="dodgerblue3"
    ) +
    theme_void()
}
