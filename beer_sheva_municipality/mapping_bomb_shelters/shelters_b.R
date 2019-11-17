library(tidyverse)
library(ggmap)
library(geojsonio)
library(sp)
library(extrafont)

#The file was better read through geojson
my_geojson <- "shelters.geojson"
#turning it to a geojson readable file
data_json <- geojson_read(my_geojson, what = "sp")

#downloading the map:
b7_map <- get_googlemap(c(34.791462 , 31.252973), 
                                zoom = 16, scale = 2, maptype = "roadmap")
# Turning the object to a df for plotting
shelters <- as.data.frame(data_json)
#changing names of columns to long and lat
names(shelters)[6:7] <- c("long", "lat")
view(shelters)
#plot the map image along with the df
ggmap(b7_map)+
  geom_point(shelters, mapping = aes(long,lat), color = "red", size = 4, shape = 15)+
  labs (title = "Neighborhood B, Beer-Sheva, Israel, bomb shelters", x = NULL, y = NULL,
        caption = "data: www.beer-sheva.muni.il | @Amit_Levinson")+
  theme_minimal()+
  theme(text = element_text(family = "Microsoft Tai Le"), 
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text = element_blank(),
    plot.caption = element_text(size = 9, face = "italic", hjust = 0),
    panel.border = element_rect(color = "black", size=2, fill = NA)
  )

ggsave("shelters_b_eng.png", width = 8, height = 8)
write_csv(shelters, "clean_beer_sheva_shelters.csv")
