library(tidyverse)
library(janitor)
library(extrafont)
library(png)
library(grid)
library(ggimage)
library(gridGraphics)

#After copying the info from wikipedia to a csv.
df <- read_csv("Runners_records.csv")
year.sub
#Since i wanted 10 year intervals, i created a vector to subset by
year.sub <- vector ("double", 11)
year.sub[1] <- 1921
year.sub[2:10] <- seq(1930,2010,10)
year.sub[11] <- 2019

#cleaning data
clean <- df %>% 
  clean_names() %>% 
  select(-date, -place) %>% 
  mutate(subset_condition = match(year, year.sub))



#adding our latest point
clean[nrow(clean) + 1,] <- list(2019,"1:59:40", "Eliud Kipchoge") #adding the latest record
#changing it into year
clean$year <- as.numeric(clean$year) 
#using the time as an integer for plotting
clean$time.int <-  as.POSIXct(clean$time, format = "%Hh %Mm %Ss")
#loading the image to plot by as a column name
clean$run <- "run.png"


g <- ggplot(data = subset(clean, !is.na(subset_condition)), mapping = aes(x=year, y = time),colour = "white", size = 1.5)+
  #i initially used points, but once the image was added this becomes redundant
  #geom_point()+
  #geom_point(below_2, mapping = aes(x = year, y= time), colour = "white", size = 3.25)+
  scale_x_continuous(limits = c(1920,2020), breaks = seq(1920,2020,10), 
                     labels = c("1920", paste0("'", seq(30,90,10)), "2000", "'10", "'20"), name = "Year")+
scale_y_continuous(limits = c(6600,10200), breaks = seq(6600,10200,600), 
                   labels = c("1:50", "2:00", "2:10", "2:20", "2:30", "2:40", "2:50"), name = "Time")+
  labs(title = "How does Eliud Kipchoge marathon score compare to previous yearly records?",
       subtitle = "Points are based on best preformances of marathon runs throughout each year. \nEliud Kipchoge is the first to break the two-hour barrier (unofficially), Great job!",
       caption = "Data from: Wikipedia | @Amit_Levinson")+
  #Adding text for today's score
  annotate("text", x=2001, y= 6700, label = "Breaking the two-hour barrier:\n12.10.2019\n1:59:40", color = "black", family = "Verdana", size = 3, hjust = 0) +
  geom_curve(aes(x = 2018, y = 7200, xend = 2012, yend = 7050),
             colour = "black", size = 0.9,
             arrow = arrow(length = unit(2,"mm"))) +
  #Plots the image instead of the points
  geom_image(aes(image = run), size = 0.07)
 

g + theme(
  panel.grid.major = element_line(colour = "gray75", size = 0.1),
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 16, family = "Miriam", colour = "black"),
  plot.subtitle = element_text(size = 12, family = "Miriam", colour = "black"),
  plot.caption = element_text(size = 7, family = "Verdana", colour = "black", face = "italic"),
  axis.title = element_text(size = 14, family = "Verdana"),
  axis.text = element_text(size = 12)
)

ggsave("marathon_runs.png", width =10, height = 6)
