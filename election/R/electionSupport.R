importMajorCityMayor <- function(url) {
  library(readxl)
  destfile <- "majorCityMayor.xls"
  curl::curl_download(url, destfile)
  majorCityMayor <- read_excel(
    destfile,skip=1)
  return(majorCityMayor)
}
importMinorCityMayor <- function(url) {
  library(readxl)
  destfile <- "minorCityMayor.xls"
  curl::curl_download(url, destfile)
  minorCityMayor <- read_excel(
    destfile,skip = 1)
  return(minorCityMayor)
}
plotMakeup <- function(plt) {
  plt$makeup = list(
    labs(
      title="2022年縣市長選舉獲勝黨派得票率",
      subtitle="除連江縣為兩位國民黨候選人得票率加總,\n其餘縣市均為單一候選人之得票率。",
      caption="資料來源：中選會選舉及公投資料庫(https://db.cec.gov.tw/ElecTable/Election?type=Mayor)"
    ),
    xlab(NULL),ylab(NULL))
  plt
}

correctClass=function(majorCityMayor){
  majorCityMayor[-c(1,2),] -> majorCityMayor2
  majorCityMayor2[,-c(1,2)] |>
    purrr::map_dfc(as.numeric) -> majorCityMayor2[,-c(1,2)]
  majorCityMayor2 #|> dplyr::glimpse()
}
keepWinnersOnly <- function(listx) {
  row = listx$value |> which.max()
  listx[row, ]
}
keepOnlyCityMajorWinners <- function(cityMajor) {
  cityMajor |>
    na.omit() |>
    dplyr::select(-"type")-> cityMajor
  
  split(cityMajor, cityMajor$行政區別) -> listCityMajor
  
  listCityMajor |> purrr::map(keepWinnersOnly) -> listCityMajorWinner
  
  listCityMajorWinner |>
    purrr::map_dfr(dplyr::bind_rows) -> cityMajorWinners
  cityMajorWinners
}
convertToLongData = function(majorCityMayor2){
  majorCityMayor2 |> 
    tidyr::pivot_longer(
      cols= -c(1,2),
      names_to = "type",
      values_to = "value"
    )
}
splitCityMajorWinners = function(cityMajorWinnersArranged){
  cityMajorWinnersArranged$行政區別 %<>% factor()
  cityMajorWinnersArranged |> 
    split(cityMajorWinnersArranged$party) -> listByParty
  return(listByParty)
}
getPartyPalettes <- function() {
  list(
    國民黨=colorspace::sequential_hcl(n = 6, h = 257, c = c(63, NA, NA), l = c(40, 97), power = 1, rev = TRUE, register = "kmt"),
    民進黨=colorspace::sequential_hcl(n = 6, h = 138, c = c(32, NA, NA), l = c(40, 97), power = 1, rev = TRUE, register = "dpp"),
    民眾黨=colorspace::sequential_hcl(n = 6, h = 189, c = c(32, NA, NA), l = c(40, 97), power = 1, rev = TRUE, register = "tpp"),
    無=colorspace::sequential_hcl(n = 6, h = 236, c = c(3, NA, NA), l = c(40, 97), power = 1, rev = TRUE, register = "nop")
  )
}
plotBarChartWithPartyColors <- function(listByParty, palettes, cityMajorWinnersArranged) {
  plt = econDV2::Plot()
  plt$ggplot=ggplot(mapping = aes(x=行政區別,y=value, fill=cutValue))
  plt$geom=geom_col(data=listByParty$中國國民黨, width=0.9)
  plt$scale = scale_fill_manual(
    name="國民黨",
    limits = levels(listByParty$中國國民黨$cutValue),
    values = palettes$國民黨[-1]
  )
  
  plt$geom2 = list(
    ggnewscale::new_scale_fill(),
    geom_col(
      data=listByParty$民主進步黨,
      aes(fill=cutValue), width=0.9),
    scale_fill_manual(
      name="民進黨",
      limits = listByParty$民主進步黨$cutValue,
      values = palettes$民進黨[-1]
    ))
  partyColors = getTaiwanPartyColors()
  plt$geom3 = list(
    ggnewscale::new_scale_fill(),
    geom_col(
      data=listByParty$台灣民眾黨,
      aes(fill=cutValue), color=partyColors$台灣民眾黨, linetype="solid", width=0.9),
    scale_fill_manual(
      name="民眾黨",
      limits = listByParty$台灣民眾黨$cutValue,
      values = palettes$民眾黨[-1]
    )
  )
  plt$geom4 = list(
    ggnewscale::new_scale_fill(),
    geom_col(
      data=listByParty$無黨籍及未經政黨推薦,
      aes(fill=cutValue), width=0.9),
    scale_fill_manual(
      name="無黨籍",
      limits = listByParty$無黨籍及未經政黨推薦$cutValue,
      values = palettes$無[-1]
    )
  )
  plt$others = list(
    scale_x_discrete(
      limits=cityMajorWinnersArranged$行政區別
    ),
    coord_flip()
  )
  martinStyle2::Style2() -> sty
  
  plt$others2 = list(
    sty$scale_y_continuous(
      sec.axis=dup_axis()
    ), sty$theme(),
    theme(
      panel.grid.major.x = element_line(color="grey"),
      panel.grid.major.y = element_blank()
    )
  )
  plt
}
plotBarChart = function(cityMajorWinnersArranged){
  cityMajorWinnersArranged |>
    ggplot()+geom_col(aes(y=value, x=行政區別, fill=cutValue))+
    scale_x_discrete(
      limits=cityMajorWinnersArranged$行政區別
    )+
    coord_flip()
}
arrangeDataAndCreateCutValue = function(cityMajorWinners){
  cityMajorWinners |>
    dplyr::arrange(value) -> cityMajorWinners
  
  cityMajorWinners |>
    dplyr::pull(行政區別) -> xlevels
  cityMajorWinners |>
    dplyr::pull(value) -> yValues
  
  yValues
  # 35-65, 65+
  cut(yValues, c(35,
                 # 40
                 48, 
                 # 50
                 52, 
                 # 60
                 62, 
                 # 64
                 65,
                 # 95
                 95)) -> cityMajorWinners$cutValue
  
  cityMajorWinners
}

createCityMajorData <- function(majorCityMayor, minorCityMayor) {
  majorCityMayor |> renameFeatures() -> majorCityMayor
  majorCityMayor |> correctClass() -> majorCityMayor2
  
  minorCityMayor |> renameFeatures() -> minorCityMayor
  minorCityMayor |> correctClass() -> minorCityMayor2
  
  majorCityMayor2 |> convertToLongData() -> majorCityMajorLong
  minorCityMayor2 |> convertToLongData() -> minorCityMayorLong
  
  dplyr::bind_rows(
    majorCityMajorLong,
    minorCityMayorLong
  ) -> cityMayor
  
  cityMayor |> 
    dplyr::mutate(
      party=stringr::str_extract(type, "^[^\\-]+"),
      valueType = stringr::str_extract(type, "[^\\-]+$")
    ) |> 
    dplyr::filter(
      valueType=="得票率"
    ) 
}
renameFeatures <- function(majorCityMayor) {
  names(majorCityMayor)[seq(3, length(majorCityMayor), by=2)] |>
    rep(each=2) -> parties
  
  majorCityMayor[1,] |> as.character() -> types
  types[-c(1,2)] -> types
  names(majorCityMayor)[c(1,2)] -> prime2
  
  newNames <- c(prime2, paste0(parties, "-", types))
  names(majorCityMayor) <- newNames
  return(majorCityMayor)
}
getTaiwanPartyColors = function(){
  list(
    國民黨="#01108e",
    民進黨="#009300",
    台灣民眾黨="#24bfbf"
  )
}

