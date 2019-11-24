library(tidyverse)
library(osmdata)
library(sf)
library(grid)
library(gridExtra)
library(extrafont)

#this gets us the long and lat min and max suitable for Be'er-Sheva
b7_coordinates <- getbb("Beer sheva Israel")
b7_coordinates

#to create the box outline we had to take some extra 'long' and 'lat' so that it doesn't fall
#right on the map itself
left <- 34.74647
right <- 34.82900
bottom <- 31.19685
top <- 31.28791

#this will help adding the text easier:
mid_point_x <- (left + right)/2
mid_point_y <- (top +bottom)/2

#We'll start by getting the lines and point for the main streets
streets <- getbb("Beer sheva Israel") %>% 
  opq() %>% 
  add_osm_feature(key = "highway", value = c("motorway", "primary",
                                             "secondary", "tertiary")) %>% 
  osmdata_sf()

#The same for the small streets:
small_streets <- getbb("Beer Sheva Israel") %>% 
  opq() %>% 
  add_osm_feature(key = "highway", value = c("residential", "living_street",
                                             "unclassified","service", "footway")) %>% 
  osmdata_sf()

#Let's load our lon and lat points for the light posts, courtesy of Be'er Sheva (Israel) municupality
sl <- read_csv("streetlight.csv")

#Turn to plot the sf object in ggplot
plot_light <- ggplot()+
  #This will add the light posts lon and lat:
  geom_point(sl, mapping = aes(x = lon, y = lat), size = 0.01, color = "yellow", alpha =0.2)+
  #Laying out the main streets
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .3,
          alpha = .8)+
  #Laying out the smaller streets
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .1,
          alpha = .1) +
  #Note that these parameters are a little more than the map itself so that'll it create
  # a sense of 'zoom out'
  coord_sf(xlim = c(34.74647,34.82900),
           ylim = c(31.19685,31.28791),
           expand = FALSE)+
  #theme_void to clear all axis easily
  theme_void()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 1, color = "white"),
    plot.caption = element_text(size = 8, face = "italic", color = "white"),
    plot.background = element_rect(fill = "#282828"),
    panel.background = element_rect(fill = "#282828"),
    #I also added some margin padding to make the frame a little neater
    plot.margin=unit(c(0.3,0.3,0.3,0.3),"cm"),
  )

#add the panel
map_light <- plot_light +
  #creating the frame
  geom_rect(aes(xmax = right, xmin = left, ymin = bottom, ymax = top),
            alpha = 0, color = "white", size = 2)+
  #adding the city's name, used the mid_point for x & y
  geom_text(aes(x= mid_point_x, y = mid_point_y-0.038, label = "Be'er-Sheva, IL"),
            color = "#b2d9ff", family = "Microsoft Tai Le", fontface = "bold", size = 10)+
  #lastly, adding a caption
  geom_text(aes(x = right-.008, y = bottom+.0016, label = "@Amit_Levinson"),
            color = "white", family = "Miriam", size = 3, fontface = "italic")

#Let's see how it came out
map_light
#and saving it, having the height a little more than the width
ggsave("street_light_of_b7.png", width = 5, height = 7)

#Thanks to Chris' blog post on introducing us the plot:
#https://ggplot2tutor.com/streetmaps/streetmaps/
#and to Connor Rothschild's blog post on adding a frame and text:
#https://connorrothschild.github.io/r/map-springfield/
#Thanks to Be'er Sheva municupality for the free data (in Hebrew) at:
#https://www.beer-sheva.muni.il/OpenData/Pages/Data.aspx