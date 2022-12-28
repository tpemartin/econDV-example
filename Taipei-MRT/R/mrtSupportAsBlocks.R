# google base map
{  
  ggmap::get_map(
    location = c(lon=121.3677371, lat=24.9481239),
    source = "google",
    # Visit Google Map and look at z value
    zoom= 13,
    scale=2
  ) -> googleMapTiles
  googleMap <- ggmap::ggmap(googleMapTiles, darken = c(0.5, "#FCFFE7"))
}
{
  bbox = c(left = 121.3058, bottom = 24.9132, right = 121.4429, top = 24.9901)
}
# stamen: 
{
  stamenMapTiles <- ggmap::get_map(
    bbox,
    source="stamen", 
    maptype="toner-lite"
  )
  stamenMap <- ggmap::ggmap(stamenMapTiles, darken = c(0.5, "#FCFFE7"))
}
{
  saveRDS(googleMap, file="data/googleMap.Rds")
}
# create OSM query
{
  econDV2::Map() -> mp
  
  query <- osmdata::opq(
    bbox
  )
}
# get MRT data
{
  query |>
    osmdata::add_osm_feature(
      key="construction:route", value="subway"
    ) |>
    osmdata::osmdata_sf() -> sanYingMRT
}
# extract sf data
{
  sanYingMRT$osm_multilines |>
    dplyr::filter(name == "捷運三鶯線") -> sf_mrtSanYing
}
# overlay map tiles
{
  googleMap +
    geom_sf(
      data=sf_mrtSanYing,
      color="dodgerblue3",
      linewidth=1,
      inherit.aes = F
    ) +
    theme_void() -> ggOverlayGoogle
}
{
  stamenMap +
    geom_sf(
      data=sf_mrtSanYing,
      color="dodgerblue3",
      linewidth=1,
      inherit.aes = F
    ) +
    theme_void() -> ggOverlayStamen
}
{
  library(patchwork)
  (ggOverlayGoogle +
      theme(plot.margin = unit(c(0,10,0,0), "pt")) +
      labs(subtitle="Overlay Google Map")) + 
  (ggOverlayStamen +
      theme(plot.margin = unit(c(0,0,0,10), "pt"))+
      labs(subtitle="Overlay Stamen Map")) +
    labs(title="三鶯線")
  # save plot in Plots panel window
  rstudioapi::savePlotAsImage(file="sanYingMrt.png", format="png", 1200,500)
}