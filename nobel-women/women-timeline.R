library(rvest)
library(dplyr)
library(purrr)
library(magick)
library(ggplot2)
library(ggimage)
library(extrafont)
library(ggtext)

# Initial data prep -------------------------------------------------------

# Read html page
wk <- read_html("https://en.wikipedia.org/wiki/List_of_female_Nobel_laureates")

# Extract relevant table from wiki page
women_table_raw <- html_table(wk, fill = TRUE)[[2]] %>% 
  # Get names of women without 'shared with'
  mutate(w_name = gsub("\\(.+", "", Laureate),
         # remove spaces for saving images
         img_name = gsub("\\s", "-", w_name))


# Get images and convert them to circles ----------------------------------

# Run this only if circular images don't exist
if (!dir.exists("img")){
  
  # Create directory for images
  dir.create("img")
  
  img_links <- wk %>% 
    # Extract only image nodes
    html_nodes("img") %>% 
    # identify and extract image link, here as 'src'
    html_attr("src") %>% 
    # Keep only images that start with upload
    .[grepl("^//upload.+", .)] %>% 
    # Marie Curie (first women) starts in position 4
    .[4:length(.)] %>% 
    # Add 'https:' before current image link
    paste0("https:", .) 
  
  # For some reason Andrea Mia's page doesn't appear
  img_links <- c(img_links[1:54], "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5f/UCLA_astrophysicist_Andrea_Mia_Ghez_%28cropped%29.jpg/220px-UCLA_astrophysicist_Andrea_Mia_Ghez_%28cropped%29.jpg", img_links[55:57])
  
  # Circular mask
  mask <- image_read("mask.png") %>% 
    image_scale("100") 
  
  
  walk2(img_links, women_table_raw$img_name, ~ {
    # Read the image
    image_read(.x) %>%
      # Rescale it
      image_scale("100") %>%
      # combine it with circular image
      image_composite(mask, ., "plus") %>% 
      # Make background white
      image_transparent("white") %>% 
      # Write image using the name we created
      image_write(path = paste0("img/", .y, ".png"))
  })
}



# More Data processing --------------------------------------------------------


# Read images into column
women_table <- women_table_raw %>% 
  # Read in processed images from above
  mutate(img_file = walk(paste0("img/", women_table_raw$img_name, ".png"), image_read),
         # Make sure year is recognized as numeric
         Year = as.numeric(Year))

# Extract distinct group id for each year
women_table$pos <- women_table %>% 
  group_by(Year) %>% 
  group_indices(Year)

# add spaces where more than 1 winner for a given year
multiples <- seq(6, 30, by = 6)


women_table <- women_table %>%  
  # Have segment start at 0
  mutate(ymin = 0) %>% 
  # Count how many years for each year
  add_count(pos) %>% 
  group_by(pos) %>% 
  # if more than one winner in a year, give them one of the values
  # from the 'multiples' vector
  mutate(ymax = multiples[1:n]) %>% 
  ungroup() %>% 
  mutate(
    # Create spaces where winners appear next to one another in plot
    ymax = case_when(
      Year - lag(Year, 2) == 2 ~ ymax - 3,
      Year - lag(Year,2 ) ==2 & Year - lead(Year,2) == 2 ~ ymax + 3,
      TRUE ~ ymax),
    # have winner be either below line or above line
    ymax = ifelse(pos %% 2 == 1, ymax, ymax *-1),
    # Manually change 2020 which overlaps with 2018
    ymax = ifelse(Year == 2020 & ymax < -3, ymax +3, ymax))



# Plot Prep ---------------------------------------------------------------


year_labels <- data.frame(x = seq(1900,2020,20),
                          y = -0.7,
                          text = c("1900", paste0(seq(20,80,20), "'"), "2000", "20'"))


# Create a customized title to insert in the plot itself
title_df <- data.frame(x = 1900, y = 20, label = "<span style='font-size:16mm; font-family: \"Bodoni MT\"'> Female Nobel Laureates</span><br><span style='font-size:6mm; color:gray40'>Women who won the Nobel Prize from 1903-2020.<br> A total of 57 distinct women</span>")


ggplot(women_table)+
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = ifelse(ymax > 0, ymax -0.75, ymax + 0.75)), color = "gray65", linetype = "dashed", size = 0.3)+
  geom_image(aes(x = Year, y = ymax, image = img_file), size = 0.025)+
  geom_segment(aes(x = 1900, xend = 2020, y = 0, yend = 0), color = "gray25", size = 0.5)+
  geom_segment(data = year_labels, aes(x = x, xend = x, y = 0, yend = -0.25), color = "gray35", size = 0.5)+
  xlim(c(1900,2020))+
  geom_text(year_labels, mapping = aes(x =x, y= y, label = text), family = "Open Sans", color = "gray35", size = 3.5)+
  #geom_text(aes(x = 1900, y = -24, label = "Data: Wikipedia\nViz: @Amit_Levinson"), color = "gray65", size = 3, hjust = 0,family = "Open Sans")+
  geom_richtext(data = title_df, aes(x = x, y =y, label = label), size = 6, hjust = 0, label.size = NA, fill = NA)+
  labs(caption = "Data: Wikipedia\nViz: @Amit_Levinson")+
  coord_fixed(ratio = 2.35/1 ,clip = "off")+
  theme_void()+
  theme(
    plot.caption = element_text(hjust = 0, size = 10, color = "gray45",  family = "Open Sans"),
    plot.margin = margin(4,3,4,3, "mm"),
    plot.background = element_rect(fill = "gray95", color = NA),
    panel.background = element_rect(fill = "gray95", color = NA)
  )

ggsave("nobel-women.png",width = 12, height = 12)
