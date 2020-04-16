## after http://egallic.fr/en/european-map-using-r/

library(ggplot2)
library(grid)
library(rworldmap)

worldMap <- getMap()
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")
europeanUnionRussia <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom","Russia")

indEU <- which(worldMap$NAME%in%europeanUnion)

# want to add Russia to the European countries
# need the number for Russia
worldMap$NAME
indEURus<-c(indEU,135)

# this is beyond me...but
# Extract lat/long border coordinates for EU states and Russia
europe_Rus_Coords <- lapply(indEURus, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europe_Rus_Coords <- do.call("rbind", europe_Rus_Coords)

# assign a value to each country for eventual polygon fill
value <- sample(x = seq(0,3,by = 0.1), size = length(europeanUnionRussia),
                replace = TRUE)

europeanUnionRussiaTable <- data.frame(country = europeanUnionRussia, value = value)

# think this adds the values in the previous data.frame table to the Coords data as a new $value column
europe_Rus_Coords$value <- europeanUnionRussiaTable$value[match(europe_Rus_Coords$region,europeanUnionRussiaTable$country)]

P <- ggplot() + geom_polygon(data = europe_Rus_Coords, aes(x = long, y = lat, group = region, fill = value),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 85),  ylim = c(32, 71))

Pscale <- P + scale_fill_gradient(name = "", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50")


# another style
# Ptheme <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  #axis.text.x = element_blank(),
  #axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  #axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  #plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
# Ptheme

# project the geographic location of 9 ecotypes from the Arabidopsis 1001 genomes project
# import datasets
# use 2 datasets with presence absence of ecotype label in 'name'column so that map labels do not overwrite in call to ggplot
nine_ecos_Col<-read.csv('nine_ecotypes_Col.csv', header=TRUE)
nine_ecos<-read.csv('nine_ecotypes.csv', header=TRUE)

Ppoints<- Pscale + geom_point(data=nine_ecos,aes(x=long,y=lat),size=3,alpha=0.5)+labs(x='Longitude', y='Latitude') +
  geom_text(data=nine_ecos, aes(long, lat, label=name),nudge_y=1.75, nudge_x=-1, size=5) +
  geom_text(data=nine_ecos_Col, aes(long, lat, label=name),nudge_y=-1.8, nudge_x=-5, size=5) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14))

# optional add of Figure number label 
# Ppoints2<- Ppoints + annotate("text", x = -3, y = 69, label = "Fig.3", colour="black", size=7, fontface = 'bold')

# Ppoints2

ggsave(Ppoints,filename="nine_ecos_Col.png",width=9, height=5, units="in",dpi=300)
