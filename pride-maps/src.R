# Pride colors from https://joelleforestier.com/#pridepalettes
# devtools::install_github("joelleforestier/PridePalettes")
library(PridePalettes)
library(ggplot2)
library(ggfx)
library(sf)


tel_aviv_file_name <- list.files(path = "~/isr_maps", pattern =".Aviv.+\\.shp$", recursive = TRUE)
# I use the file for different projects and reference it several times from a specific folder
tel_aviv_file <- read_sf(paste0("C:/Users/amitl/R_code/isr_maps/", tel_aviv_file_name))

# Create function to color the map ----------------------------------------

pride_my_map <- function(x) {

# Break the x (or y, but then change it) axis into 6 parts
break_intervals <- (st_bbox(x)["xmax"] - st_bbox(x)["xmin"]) / 6

# Create tiles we'll use as background
pride_tiles <- data.frame(
  # Vectorize over the break intervals starting at 0 (at xmin)
  xmin = st_bbox(x)["xmin"] + c(0:5) * break_intervals,
  # Similar but for the upper bound of the rect
  xmax = st_bbox(x)["xmin"] + c(1:6) * break_intervals,
  ymin = rep(st_bbox(x)["ymin"], 6),
  ymax = rep(st_bbox(x)["ymax"], 6),
  fill_color = pride_palette("pride")
)


ggplot(x)+
  as_reference(
    geom_sf(),
    id = "map")+
  with_blend(
    geom_rect(data = pride_tiles, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_color)),
    bg_layer = "map",
    blend_type = "in",
    id = "blended")+
  # Add some shadow to the map
  with_shadow("blended", sigma = 5) +
  scale_fill_identity()+
  theme_void()
}

# Run it on Tel-Aviv
ta_pride <- pride_my_map(tel_aviv_file)
ggsave("ta.png", width =3 ,height = 4)


# Make one for Israel too -------------------------------------------------

isr_map <- list.files(path = "~/isr_maps", pattern ="israel_borders\\.shp$", recursive = TRUE)
# I use the file for different projects and reference it several times from a specific folder
isr_map <- read_sf(paste0("C:/Users/amitl/R_code/isr_maps/", isr_map))

isr_pride <- pride_my_map(isr_map)
ggsave("isr.png", width =3 ,height = 4)

