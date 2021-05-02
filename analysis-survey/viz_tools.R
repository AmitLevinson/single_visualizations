library(ggraph)
library(tidyverse)
library(igraph)
library(extrafont)
library(ggtext)
library(readxl)
library(patchwork)  

# read data
da_survey <- read_xlsx("analysis-survey/da_survey_2021.xlsx")
#Sys.setlocale("LC_ALL", "Hebrew")


# Colelct data, available from the 'Data Analytics Facebook group"
tools_used <- da_survey %>% 
  # Select relevant column
  select(id, tools = 19) %>% 
  filter(!is.na(tools)) %>% 
  # Filter responses of tools used
  separate_rows(tools, sep = ",") %>% 
  # Clean some responses
  mutate(tools = gsub("(\"|\\s)", "", tools),
         # clean specific cases of r packages and python
         tools = as.factor(case_when(
           grepl("ויזא", tools) ~ "I don't do\ndata visualization",
           grepl("Jupyter|Python", tools) ~ "Python",
           grepl("(ggplot|(^[R]$))", tools) ~ "R",
           TRUE ~ tools
         )))  %>% 
  # Lump some groups together
  mutate(tools = fct_lump_min(tools, 2),
         # revert to character
         tools = as.character(tools))

# We'll work to create a network graph from one tool to another
t_count <- tools_used %>% 
  group_by(id) %>% 
  # Create combinations
  expand(from = tools,to = tools) %>% 
  # First filter of dupliace values
  filter(from != to) %>% 
  # Using new columns as a way to reorder the values by a.b.c... and then filtering
  # duplicates using distinct. This gets us te relevant combination of tools :)
  transmute(nodea = ifelse(from < to, from, to),
            nodeb = ifelse(from > to, from , to)) %>% 
  distinct(id, nodea,nodeb) %>% 
  ungroup() 


# count the edges
edges_df <- select(t_count, -id) %>% 
  count(nodea, nodeb)
edges_df %>% 
  mutate(n/n_distinct(tools_used$id)) %>% 
  View()

# Create vertices
# New data frame including id in one column and names in another
vertices_df <- data.frame(
  id = rep(t_count$id,2),
  t_name = c(t_count$nodea, t_count$nodeb)) %>% 
  distinct(id, t_name) %>% 
  count(t_name) %>% 
  # Count how many geoms across all scripts that contain geoms
  mutate(
      vjust = ifelse(str_detect(t_name, "data visualization"), -0.5, -0.8),
      pct = round(n/n_distinct(tools_used$id)*100, 1))


# Create the ggraph
t_ggraph <- graph_from_data_frame(edges_df,
                                  directed = F,
                                  vertices = vertices_df)

# Plot
p1 <- ggraph(t_ggraph, layout="stress")+
  geom_edge_link0(aes(edge_width = n), edge_colour = "gray65", alpha = 0.7)+
  geom_node_point(aes(size = pct), shape=19, color = "gray55")+
  geom_node_text(aes(label=name, vjust = vjust), lineheight = 0.9,  repel=F, size = 5, family = "Open Sans")+
  labs(title = paste("\nשילובי כלים"),
       subtitle = paste0("\u202B",
                         "גרף רשתי של כלי וויזואליזציה שאנליסטים ציינו כי הם משתמשים בהם במסגרת עבודתם. ",
                         "עובי הקווים מייצג את שכיחות המופעים של הכלים", 
                         "\n",
                         "\u202B",
                         "יחד מתוך כלל תשובות המשיבים (ניתן היה לבחור יותר מכלי אחד). ",
                         "\u202B",
                         "גודל הנקודות מייצג את שכיחות הכלי בקרב כלל המשיבים."))+
  guides(size = "none",
         edge_width = guide_legend("שכיחות מופעים יחד", title.position = "right"))+
  theme(
    text = element_text(family = "Open Sans Hebrew"),
    plot.title = element_text(hjust = 1, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 1, size = 15, color = "gray20", lineheight = 1.1),
    legend.position= "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 9, color = "gray30"),
    legend.title = element_text(size = 12, color = "gray30"),
    legend.key=element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(size = 10, color = "gray35", hjust = 1)
  )+
  coord_cartesian(clip = "off")

# Second plot - Frequency of tool -----------------------------------------

  tools_separate <- tools_used %>% 
  filter(!is.na(tools), !grepl("I don't", tools))
  
  p2 <- tools_separate %>% 
  count(tools) %>% 
  mutate(n = n/n_distinct(tools_separate$id),
         tools = fct_reorder(tools, -n),
         label_pos = ifelse(n > 0.05, "above", "below")) %>% 
  ggplot(aes (x = tools, y= n))+
  geom_col(fill = "gray55")+
  scale_y_continuous(name = NULL, labels = scales::percent, limits = c(0,0.57))+
  # add text labels
  geom_text(aes(label = scales::percent(n, accuracy = 0.1), y = n), vjust = -0.5, color ="gray25", family = "Open Sans Hebrew",  size = 4, show.legend = FALSE)+
  
  coord_cartesian(clip = "off")+
  labs(title = paste("\u202B", "שכיחות הכלי בקרב כלל המשיבים"),
       x = NULL)+
  theme_minimal()+
  theme(
    text = element_text(family = "Open Sans Hebrew"),
    plot.title = element_text(hjust = 1, face = "bold", size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10, color = "gray25"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )



# Combine plots -----------------------------------------------------------


p1/p2+
  plot_layout(height = c(2,1))+
  # plot_layout(guides = 'collect')+
  plot_annotation(title = paste0("\u202B","\"איזה כלי ויזואלזציה את משתמשת לרוב?\""), 
                  subtitle = paste0("\u202B", "ניתוח תשובותיהם של 318 משיבים לשאלה על שימוש בכלי וויזואליזציה מתוך הסקר 'מקצוע דאטה אנליסט'."),
                  caption = paste0("\u202B", "נתונים: רביע, סיוון ויובל במסגרת קבוצת Data Analytics Israel | וויזואליזציה: עמית לוינסון"),
                  theme = theme(plot.title = element_text(size = 20, face = "bold"),
                                plot.subtitle = element_text(size = 15))) &
  theme(text = element_text('Open Sans Hebrew'),
        plot.title = element_text(face = "bold", hjust = 1),
        plot.subtitle = element_text(hjust = 1),
        plot.margin = unit(c(4,4,4,4), "mm"),
        plot.caption = element_text(color = "gray35", size = 12))


ggsave("analysis-survey/dasurvey.png", width = 15, height = 11)

