install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
install.packages("gridExtra")
install.packages("ggpubr")
install.packages("ggrepel")# Install ggrepel package

library("ggrepel")     
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
# library(plyr)
library(dplyr)
# library(googlesheets)
library(readxl)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

# can read from google sheets?

# gs_auth()
# listing<-gs_ls()
# listing$sheet_title

# open google spreadsheet
CM3_meta_data <- read_excel("CM3_Collection_Map.xlsx")

# Get world map from map_data package
world <- map_data("world")

americas <- subset(world, region %in% c("USA","Brazil","Mexico", "Colombia", "Argentina", "Canada",
                                        "Peru","Venezuela","Chile","Guatemala","Ecuador", "Bolivia", "Cuba",
                                        "Honduras", "Paraguay", "Nicaragua","El Salvador", "Costa Rica", "Panama",
                                        "Uruguay",  "Jamaica",  "Trinidad and Tobago", "Guyana", "Suriname", "Belize",
                                        "Barbados", "Saint Lucia", "Grenada", "Saint Vincent and the Grenadines", 
                                        "Antigua and Barbuda", "Saint Kitts and Nevis"))

# this defines limits of the world map in long/lat that will be plotted
xlim = c(-180,180)
ylim = c(-55,75)


# Create a caegorical color scale for hosts
# see: http://www.datavis.ca/sasmac/brewerpal.html
# myColors <- brewer.pal(8,"Dark2")
myColors3 <- c("#666666", "#666666", "#666666", "#666666", "#666666", "#666666", "#D55E00", "#666666", "#666666",
               "#CC66FF", "#990000", "#666666", "#336699")

#666666 = Grey
#D55E00 = Orange (New Orleans)
#CC66FF = Pink (Tapachula)
#990000 = Red (Poza Rica)
#336699 = Blue (Vergel)


myColors <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(12,"Set3")

# names(myColors) <- levels(bunya_meta_data$Host_category)
fillScale <- scale_fill_manual(values = myColors, na.value="black")
fillScale <- scale_fill_manual(values = myColors2, na.value="black")
fillScale <- scale_fill_manual(values = myColors3, na.value="black")
fillScale <- scale_fill_grey()


# unique(bunya_meta_data$Host_category)

# geom_polygon -> makes base of map 
# geom_point -> create points colored by host
# Add geom_points (latitude and longitude) 
# do 2 geom_point calls: 1 for colored circles and one for black outlines
# coord_map clips the map 
world_map <- ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey90", color = "grey70", size=0.2) + theme_bw() + 
  geom_polygon(fill = "white", color = "black") +
  coord_fixed(ratio=1.1, xlim = c(-180, -35))  + 
  theme_void() +
  geom_point(data = CM3_meta_data, aes(x = Long, y = Lat, fill=State), color="black", shape=21, size=1.5, alpha=0.90, stroke=0.2) + 
  coord_map(xlim=xlim, ylim=ylim) + fillScale + theme(axis.text.x=element_blank(),
                                                      axis.text.y=element_blank(),
                                                      legend.text=element_text(family="Helvetica", size=10),
                                                      axis.ticks=element_blank(),
                                                      axis.title.y=element_blank(),
                                                      axis.title.x=element_blank())




world_map

ggsave("CM3_Map.pdf", width=7, height=7, units="in")


#Zoomed in map
theme_set(theme_void())

world_map <- map_data("world")

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

# Some EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy",
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic")

# Retrievethe map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(some.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")

countries_to_include <- c("USA","Brazil","Mexico", "Colombia", "Argentina", "Canada",
                          "Peru","Venezuela","Chile","Guatemala","Ecuador", "Bolivia", "Cuba",
                          "Honduras", "Paraguay", "Nicaragua","El Salvador", "Costa Rica", "Panama",
                          "Uruguay",  "Jamaica",  "Trinidad and Tobago", "Guyana", "Suriname", "Belize",
                          "Barbados", "Saint Lucia", "Grenada", "Saint Vincent and the Grenadines",
                          "Antigua and Barbuda", "Saint Kitts and Nevis")

us_and_central_america <- c("USA","Mexico", "Belize", "Guatemala", "Honduras", "Nicaragua", "El Salvador", "Costa Rica", "Panama")

us_and_mexico_map <- map_data("world", region = us_and_central_america)
?map_data

# us_and_mexico_map <- filter(us_and_mexico_map, subregion !="Alaska" & subregion != "Hawaii")

region.lab.data <- us_and_mexico_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

# this defines limits of the world map in long/lat that will be plotted
xlim = c(-125,-65)
ylim = c(0,55)

ggplot(us_and_mexico_map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  scale_fill_manual(values = c("blue")) +
  # geom_text(aes(label = region), data = region.lab.data  size = 3, hjust = 0.5)+
  coord_map(xlim=xlim, ylim=ylim)+
  theme_void()+ theme(legend.position = "none") +
  geom_point(data = CM3_meta_data, aes(x = Long, y = Lat, fill=State, colour = State), color="black", shape=21, size=5, alpha=0.90, stroke=0.2) + 
  geom_text_repel(data = CM3_meta_data, size = 2, aes(label = State, x = Long, y = Lat)) +
  coord_map(xlim=xlim, ylim=ylim) + fillScale + ggtitle("Map of USA & Mexico", subtitle = "Poza Rica, Tapachula, Vergel, & New Orleans Collection Sites") + theme(axis.text.x=element_blank(),
                                                      axis.text.y=element_blank(),
                                                      legend.text=element_text(family ="Helvetica", size=10),
                                                      axis.ticks=element_blank(),
                                                      axis.title.y=element_blank(),
                                                      axis.title.x=element_blank()) 
