# Inspiration from Cedric Scherer previous TidyTuesday submission of Canada's Trubines:
# https://github.com/Z3tt/TidyTuesday/blob/master/R/2020_44_CanadianWindTurbines.Rmd

library(sf)
library(dplyr)
library(raster)
library(ggplot2)
library(ggtext)
library(glue)
library(extrafont)
library(readr)

sf_israel <- readRDS("israel-vaccinated/00_Israel_0_sf.rds") %>% 
  st_transform("+proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs ") 

# File name for today
file_name <- paste0("israel-vaccinated/isr_",Sys.Date(), ".Rdata")

# Read the file, if id doesn't exist download the relevant information and save it
if (file.exists(file_name)){
  isr_data <- readRDS(file = file_name)
} else {
  owi_json <- jsonlite::fromJSON("https://covid.ourworldindata.org/data/owid-covid-data.json")
  owi_json %>% 
    .$ISR %>% 
    .$data %>% 
    as.data.frame() %>% 
    dplyr::select(second = people_fully_vaccinated,
                  first = people_vaccinated,
                  date) %>% 
    # Create relevant columns
    mutate(date = as.Date(date, "%Y-%m-%d"),
           population = owi_json %>% .$ISR %>% .$population,
           percent_first = first/population,
           percent_second = second/population,
           nonvac = population - first) %>%
    # Get last updated day from OurWorldinData
    filter(!is.na(first) & !is.na(second) & !is.na(date)) %>% 
    # Not sure why it doesn't work in the row above but:
    filter(date == max(date)) %>% 
    # Save for if I return to it later in the day
    saveRDS(file = file_name)
    # Load the data
    isr_data <- readRDS(file = file_name)
}

# Convert to raster
r <- raster(sf_israel,res = 500)
ras_israel <- rasterize(sf_israel, r, field = 1)
  
# Turn raster object to data.frame
israel_tiles <- ras_israel %>% 
  as.data.frame(xy = TRUE) %>%
  filter(!is.na(layer)) %>% 
  arrange(y) %>% 
  # Here we split the x-y coordinates according to % of each (non/)vaccinated group
  mutate(id = 1:nrow(.),
         group_name = as.factor(case_when(
            id <= nrow(.) * isr_data$percent_second ~ "second",
            id >= nrow(.) * isr_data$percent_second & id <= nrow(.) * isr_data$percent_first ~ "first",
           TRUE ~ "nonvac"))) %>% 
  dplyr::select(-layer)

# Identify percent labels as y-axis
id_to_filter  <-  data.frame(id = 1:nrow(israel_tiles), 
                             point_to_label = as.character(cut(israel_tiles$id, breaks = quantile(israel_tiles$id,probs = 0:5/5),labels = paste0(seq(0, 80, 20), "%"), include.lowest = TRUE)) %>% 
                    ifelse(duplicated(.), NA, .)) 


pct_labels <- inner_join(israel_tiles, id_to_filter) %>% 
  filter(!is.na(point_to_label)) %>% 
  mutate(start_line = min(israel_tiles$x) - 7000,
         # We want the line to end a little before the shape of the map, which means where X is at minimum.
         # The 0 is a place holder for the first value
         end_line = with(israel_tiles[israel_tiles$y %in% .$y,], tapply(x,y,  min)) - 1500) %>% 
  filter(id != 1) 

# Vertical bars for annotation
ver_bars <- data.frame(
  # Point where the line ends (maximum y value for each group)
  yend = unname(sort(with(israel_tiles, tapply(y, group_name, max)), decreasing = TRUE)),
  # Identify min value for the non-vaccinated and 2 spots at min(y) for the vaccinated groups.
  y = c(min(israel_tiles[israel_tiles$group_name == "nonvac","y"]), rep(min(israel_tiles$y), 2)),
  # Add some white space on the side of the map
  x = max(israel_tiles$x) + 3000,
  xend = max(israel_tiles$x) + c(6e4,6e4,-1e4),
  # This is mostly for some conventions on which value represent which group/row in the df
  group_name = c("nonvac","first", "second"))

# Same idea but for easier reading in the plot itself
hor_bars <- ver_bars

# Colors info
aes_details <- data.frame(
  group_name = c("nonvac", "first", "second"),
  group_color = c("gray70", "#85CC6F", "#337B24")
)

# Position of annotations
text_pos <- data.frame(
  # Find the middle location between where the segments starts and ends
  transmute(ver_bars, y = (yend- y)/2 + y),
  group_name = ver_bars$group_name,
  x = ver_bars$xend + 4e3,
  group_color = aes_details$group_color,
  # Create label using {glue} and the downloaded df so it's easy to update when necessary
  label = c(glue("{round(isr_data$nonvac/1e6, 1)}M individuals<br><span style='color:gray50'><b>did not yet receive<br>vaccination ({round({1 - isr_data$percent_first}*100, 1)}%)</b></span>"),
            glue("{round(isr_data$first/1e6, 1)}M individuals<br>received <span style='color:{aes_details$group_color[2]}'><b>first<br>vaccination ({round({isr_data$percent_first}*100, 1)}%)</b></span>"),
            glue("{round(isr_data$second/1e6, 1)}M<br>individuals<br>were<br><span style='color:{aes_details$group_color[3]}'><b>vaccinated<br>twice<br>({round({isr_data$percent_second}*100, 1)}%)</b></span>")
  )) 


p <- ggplot(israel_tiles)+
  # The tiles that fill the map
  geom_tile(aes(x = x,y = y,fill = group_name), size =.3, show.legend = FALSE)+
  # Segments representing the y axis percentages
  geom_segment(data = pct_labels , aes(x = start_line, xend = end_line, y = y, yend = y), color = "gray80", linetype = "dashed")+
  # Percent labels
  geom_text(data = pct_labels,aes(x = start_line - 8.5e3, y = y, label = point_to_label), size = 4.5, color = "gray70", family = "Open Sans")+
  # vertical bars on the side for each group
  geom_segment(data = ver_bars, aes(x = xend, xend = xend, y = y- 2000, yend = yend), color = "gray70")+
  # horizontal bars with the *minimum* y value (y at minimum for each group)
  geom_segment(data = hor_bars[2:3,], aes(x = xend - 2e4, xend = xend - 75, y = y - 1.75e3, yend = y-1.75e3), color = "gray70")+
  # horizontal bars with the *maximum* y values (y at max for each group)
  geom_segment(data = hor_bars, aes(x = xend - 2e4, xend = xend - 50, y = yend, yend = yend), color = "gray70")+
  # Annotation text
  geom_richtext(data = text_pos,aes(x = x, y =y, label = label),  fill = NA, label.color = NA, hjust = 0, size = 6, color = "gray55", family = "Open Sans")+
  coord_equal(clip = "off")+
  # Add some padding for the percent labels
  scale_x_continuous(limits = c(min(pct_labels$start_line) - 1e4,max(text_pos$x) + 8e4))+
  scale_fill_manual(values = c( "nonvac" = "gray65" ,"first" = "#85CC6F", "second" = "#337B24"))+
  labs(title = "Israel's Vaccine Admission",
       subtitle = glue("With a population of {round(isr_data$population/1e6, 1)}M, <b><span style='color:{aes_details$group_color[3]}'>{round(isr_data$second/1e6, 1)}M Israelis ({round(isr_data$percent_second*100, 1)}%) were already<br>vaccinated twice</span></b>. Color represents portion of group out of the<br>whole shape, and not of where vaccinated individuals reside."),
       # Take the most up to date from the isr_data df we created above
       caption = glue("Data: OurWorldInData | {format(isr_data$date, '%B %d, %Y')} | Viz: @Amit_Levinson"))+
  theme_void()+
  theme(
    text = element_text(family = "Open Sans"),
    plot.title = element_text(size = 32, face = "bold", family = "Noto Serif", hjust = 0),
    plot.subtitle = element_markdown(size =18, color = "gray25"),
    plot.caption = element_text(color = "gray60", hjust = 0.5, size = 11),
    plot.margin = margin(8,6,6,8,"mm"),
    # For some odd reason, not speciyging this creates a transparent bg!
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("israel-vaccinated/israel-vaccinated.png", p, height = 13, width = 10)
