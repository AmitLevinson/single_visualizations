library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggspatial)
library(ggtext)
library(extrafont)

# Read map
isr_map_sf <- st_read("~/amitlevinson.com/static/data/maps/israel/israel_borders.shp")

# Sample 10 random points and 1 point 10 times:
points_df <- data.frame(locations = st_sample(isr_map_sf, 10),
                        us = rep(st_sample(isr_map_sf,1), 10))

# Convert to 4326 for Geodetic coordinates
map_points <- points_df %>% 
  map_dfc(st_transform, crs = 4326)

# Make the label and segments:
segment_info <- map_points[c("geometry", "geometry.1")] %>% 
  # Convert to coordinates (long-lat)
  map_dfc(~ st_coordinates (.x)) %>% 
  map_dfc(as.data.frame) %>% 
  # Change names for easier reading
  set_names(c("loc.x","loc.y", "us.x" ,"us.y")) %>% 
  # Get the distance from the one location to the other 10, convert to Km and round it
  mutate(dist = round(map2_dbl(points_df$geometry, points_df$geometry.1, st_distance) / 1000, 0),
         # Add conditional to color the segment
         closest = ifelse(dist == min(dist), "yes", "no"),
         # Add 'km' label to the highlighted value
         dist = ifelse(closest == "yes", paste0(dist, "km"), dist))

# Revert back to crs 4326 (orginaly it was UTM)
isr_map_4326 <- st_transform(isr_map_sf, crs =4326)

# Plot!

ggplot(isr_map_4326)+
  geom_sf()+
  # Plotting the 10 locations
  geom_sf(data = map_points, aes(geometry = geometry), size = 1.5, color = "gray45")+
  # Add line segments
  geom_spatial_segment(data = segment_info, mapping = aes(x = us.x, xend = loc.x, y = us.y, yend = loc.y,  color = closest))+
  # Add text
  geom_spatial_text_repel(data = segment_info, aes(x =loc.x, y = loc.y, label = dist, color = closest), size = 5)+
  # Add our location (have it on top of the segments)
  geom_sf(data = map_points, aes(geometry = geometry.1), color = "red", size = 2)+
  # Edit color of text and segment line
  scale_color_manual(values = c("yes" = "red", "no" = "gray45"))+
  # Remove legends
  guides(color = "none")+
  # Add title & subtitle
  labs(title = "Calculating Distance to Various Points on a Map",
       subtitle = "Using the sf_distance function from the {sf} R package we can calculate<br>distances from one set of points to another (many to many, one to many, etc).<br>Once we have all distances we can find the <span style='color:red'>nearest point.</span>")+
  # Minor aesthetics
  theme_void()+
  theme(
    text = element_text(family = "IBM Plex Sans"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_markdown(size = 11))

ggsave("point-distance/distances.png", width = 8, height = 8)
