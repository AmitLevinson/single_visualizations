library(ggplot2)

# Front squares
squares <-data.frame(x1=c(10,30,50,70), x2=c(20,40,60,80), y1=10, y2= 15)

# Background colors
background_squares <- data.frame(x1 = seq(0,89,1), x2 = seq(1,90,1), y1 = 5, y2 = 20, col_fill = paste0("gray", seq(89,0,-1)))

# Plot
ggplot() + 
  # Background first
  geom_rect(data=background_squares, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=col_fill), color = NA) +
  # Rectangles after
  geom_rect(data=squares, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="gray50", color = "gray50") +
  # Create equal boundaries on each side
  ylim(5, 25)+
  xlim(0, 90)+
  # Use coloumn values as fill values :)
  scale_fill_identity()+
  theme_void()
  # If you want to add a label then:
  # annotate("text", x = 89.9, y = 5.35, label = "@Amit_Levinson", hjust = 1, size = 3, color = "gray20")

ggsave("random-geoms/same-color.png", width = 8, height = 5, bg = "transparent")

