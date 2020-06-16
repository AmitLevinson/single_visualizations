library(patchwork) # For combining plots
library(ggplot2)
library(extrafont)

theme_set(
  theme_minimal()+
  theme(text = element_text("Roboto Condensed"),
        panel.grid = element_blank()))
# Save plot 1
p1 <- ggplot(data = mtcars,aes(x = wt, y= mpg, color = as.factor(cyl)))+
  geom_point(show.legend = FALSE)+
  labs(title = "P1")

# Save plot 2
p2 <- ggplot(data = mtcars, aes(x = as.factor(cyl), y = mpg))+
  geom_boxplot(aes(fill = as.factor(cyl)), show.legend = FALSE)+
  labs(x = "Cyl", y = "mpg", title = "P2")

# Save plot 3
p3 <- ggplot(data = mtcars,aes(x = as.factor(vs), fill = as.factor(cyl)))+
  geom_bar()+
  facet_wrap(~ cyl)+
  scale_fill_discrete(name = "Cyl")+
  labs(x = "VS", y = "Count", title = "P3")+
  theme(strip.text = element_blank())

# Combine them together!
p <- p2 + p1 / p3

# Add some annotations and collect the legend!
p <- p + 
  plot_annotation(
  title = "Patchwork - Combine separate ggplots",
  subtitle = "Patchwork is an R package that makes combining separate ggplots into one single graphic easy.\nIt's done with a very intuitive syntax such as p2 + p1 / p3 for the graphic below.")+
  guide_area() +
  plot_layout(guides = 'collect')

ggsave("p.png", p, width = 10, height = 6)
