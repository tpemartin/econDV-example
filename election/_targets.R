library(targets)
library(econDV2)
source("R/electionSupport.R")
tar_option_set(packages = c("readr", "dplyr", "ggplot2", "econDV2", "magrittr"))
list(
  urlMajorCityMayor %t=% "https://github.com/tpemartin/111-1-econDV/blob/main/data/%E4%B8%AD%E8%A1%A82-1-00(%E4%B8%AD%E5%A4%AE)-111%E5%B9%B4%E7%9B%B4%E8%BD%84%E5%B8%82%E9%95%B7%E9%81%B8%E8%88%89%E5%90%84%E6%94%BF%E9%BB%A8%E3%80%81%E7%84%A1%E9%BB%A8%E7%B1%8D%E5%8F%8A%E6%9C%AA%E7%B6%93%E6%94%BF%E9%BB%A8%E6%8E%A8%E8%96%A6%E4%B9%8B%E5%80%99%E9%81%B8%E4%BA%BA%E5%BE%97%E7%A5%A8%E6%95%B8(%E7%8E%87)%E8%A1%A8.xls?raw=true",
  urlMinorCityMajor %t=% "https://github.com/tpemartin/111-1-econDV/blob/main/data/%E4%B8%AD%E8%A1%A82-2-00(%E4%B8%AD%E5%A4%AE)-111%E5%B9%B4%E7%B8%A3(%E5%B8%82)%E9%95%B7%E9%81%B8%E8%88%89%E5%90%84%E6%94%BF%E9%BB%A8%E3%80%81%E7%84%A1%E9%BB%A8%E7%B1%8D%E5%8F%8A%E6%9C%AA%E7%B6%93%E6%94%BF%E9%BB%A8%E6%8E%A8%E8%96%A6%E4%B9%8B%E5%80%99%E9%81%B8%E4%BA%BA%E5%BE%97%E7%A5%A8%E6%95%B8(%E7%8E%87)%E8%A1%A8.xls?raw=true",
  majorCityMayor %t=% importMajorCityMayor(urlMajorCityMayor),
  minorCityMayor %t=% importMinorCityMayor(urlMinorCityMajor),
  cityMajor %t=% createCityMajorData(majorCityMayor, minorCityMayor),
  cityMajorWinners %t=% keepOnlyCityMajorWinners(cityMajor),
  cityMajorWinnersArranged %t=% arrangeDataAndCreateCutValue(cityMajorWinners),
  listByParty %t=% splitCityMajorWinners(cityMajorWinnersArranged),
  palettes %t=% getPartyPalettes(),
  partyColors %t=% getTaiwanPartyColors(),
  mp %t=% econDV2::Map(),
  sf_taiwan %t=% obtainTaiwanSf(mp),
  sf_taiwanChoropleth %t=% mergeWithElectionData(sf_taiwan, cityMajorWinnersArranged),
   
  # plots ----
  pltBar %t=% plotBarChart(cityMajorWinnersArranged),
  pltBarWithPartyColors %t=% plotBarChartWithPartyColors(listByParty, palettes, cityMajorWinnersArranged, partyColors),
  pltBarFinal %t=% plotMakeup(pltBarWithPartyColors),
  plt %t=% plotChoroplethElection(sf_taiwanChoropleth, palettes, partyColors)
)
