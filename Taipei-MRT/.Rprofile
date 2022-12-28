library(ggplot2)
library(showtext)
library(econDV2)
# add google font: Noto Sans TC for chinese characters
sysfonts::font_add_google('Noto Sans TC')
# turn on showing chinese text in graph device
showtext_auto()
# set our graph theme to use the google font and a classic white back ground
theme_set(
  theme(
    text=element_text(family = "Noto Sans TC")
  )+
    theme_classic()
)
colorspace::diverging_hcl(n = 7, h = c(260, 0), c = 100, l = c(50, 90), power = 1, rev = TRUE, register = "cspaceDiverging")
ggmap::register_google(key=Sys.getenv("googleMapApiKey"))

Sys.setenv(CLIPR_ALLOW=TRUE)
