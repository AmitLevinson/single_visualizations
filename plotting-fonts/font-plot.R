library(extrafont)
library(tidyverse)

# convert string so they won't be read as factor
options(stringsAsFactors = FALSE)

# Load font names
f_family <- fonts()
# Create x axis (using each) and filter to match the number fonts
x_axis <- rep(1:15, each = 15)[1:length(f_family)]
# Create y axis (using times) and filter to match the number of fonts
y_axis <- rep(1:15, times = 15)[1:length(f_family)]

# Combine to dataframe
df <- as.data.frame(cbind(f_family, x_axis, y_axis))

# Convert long font names to two rows
df <- df %>% 
  mutate(f_family = ifelse(str_count(f_family) >= 16, str_replace(f_family, "\\s", "\n"), f_family))

# Plot
ggplot(df, aes(x = x_axis, y = y_axis))+
  geom_text(aes(label = f_family), family = f_family, size = 4)+
  theme_void()+
  labs(title = "Visualizing system and {extrafont} fonts")+
  theme(plot.title = element_text(family = "Roboto Condensed", color = "dodgerblue", face = "bold", hjust = 0.5, size = 20))


ggsave("text_plot.png", width = 21, height = 12, dpi = 720)

# Idea generated from Hadley Whickham 'ggplt2: Elegant Graphics for Data Analysis' 
# https://ggplot2-book.org/annotations.html
