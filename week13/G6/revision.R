googlesheets4::gs4_deauth()
googlesheets4::read_sheet(ss="https://docs.google.com/spreadsheets/d/1hXtBuoCTp87VUHKhUVz3wDY3lxXhsddNgrk8mlMmWcA/edit#gid=0",
                          sheet = "sheet1")->Accident


tidyr::pivot_longer(data=Accident,cols=2:11,
             names_to="year",
             values_to="NumberOfAccident")->Accident
Accident$city<-factor(Accident$city,levels = c("臺北市","新北市","桃園市","臺中市","臺南市","高雄市"))

# Original design -------
library(ggplot2)
plt = econDV2::Plot()
plt$ggplot = ggplot(data=Accident,
       mapping = aes(x=year,y=NumberOfAccident,
                     colour=city,group=city))
plt$geom = list(
  geom_point(shape=15,size=3),
  geom_line(size=2,alpha=0.9))

plt$others = list(
  labs(title = "直轄市之中，誰的每日交通事故發生件數最少?",
       subtitle = "以2012-2021資料為例，縱軸代表平均每日交通事故發生件數(件/日)",
       caption="資料來源:行政院主計總處https://winsta.dgbas.gov.tw/DgbasWeb/ZWeb/StateFile_ZWeb.aspx"),
  theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          legend.key.size = unit(30,"pt"))
)
plt$scale = scale_colour_manual(values = c("#FF9900","#e9e9e9",
                                           "#CCCCCC","#999999","#666666","#333333"))

plt$make()


# Revision with color twist -------
plt2 = plt
plt2$scale = 
  scale_colour_manual(values = c("#FF9900","#557153",
                                 "#7D8F69","#CEAB93","#A9AF7E","#AD8B73")) 
plt2$make()


# Makeup -----
sty = martinStyle2::Style2()

plt2$others2 = list(
  sty$theme(),
  ylab(NULL), xlab(NULL))

plt2$make()


# ggdash()
