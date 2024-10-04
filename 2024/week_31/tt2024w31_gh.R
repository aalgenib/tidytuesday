library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)

# set working directory
work_dir <- "~/R/tidytuesdays/2024/2024w31/"


# load original data
tuesdata <- tidytuesdayR::tt_load('2024-07-30')
summer_movies <- tuesdata$summer_movies


# set fonts
font_add_google("Roboto", "roboto")
ft1 <- "roboto"
showtext_auto()
font_col <- "grey5"


# wrangle data
summer_movies_rat <- summer_movies |> 
  separate_longer_delim(genres, delim = ",") |> 
  select(year, genres, average_rating) |> 
  filter(!genres %in% c("Talk-Show", "Reality-TV", "Film-Noir")) |> 
  na.omit()

total_average <- median(summer_movies_rat$average_rating)


# set labeling
txt_title <- "Documentaries' distinction"
txt_subtitle <- "Each dot represents the genre assigned to a summer movie released between 1950 and 2023.<br>The dot is positioned according to the movie's **average rating** by IMDB voters. Although drama,<br>romance and comedy are by far the most frequent genres, it's the documentary genre whose<br>summer movies are rated highest on <span style = 'color:#d62811'>**total average**</span>."
txt_caption <- "**Data**: IMDB \u2022 **Graphic**: Amman Algenib"


# create plot
ggplot(summer_movies_rat) +
  #geom_jitter(aes(x = genres, y = average_rating, color = genres)) +
  #geom_hline(yintercept = total_average) +
  geom_jitter(aes(x = reorder(genres, average_rating, na.rm = TRUE), y = average_rating), size = .8, height = 1, width = 0.25) +
  stat_summary(
    aes(x = reorder(genres, average_rating, na.rm = TRUE), y = average_rating),
    fun = mean,  # Function to compute median
    geom = "crossbar",  # Use crossbar to draw the median line
    width = 0.5,  # Width of the median line
    color = "#d62811"  # Color of the median line
  ) +
  coord_flip() +
  
  labs(
    title = txt_title,
    subtitle = txt_subtitle,
    caption = txt_caption
  ) +
  
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    text = element_text(family = ft1, colour = font_col),
    axis.text = element_text(colour = font_col, size = 30),
    plot.title = element_markdown(size = 70, face = "bold"),
    plot.subtitle = element_markdown(size = 45, lineheight = 0.4, margin = margin(t = 5, b = 10)),
    plot.caption = element_markdown(size = 30, margin = margin(t = 20)),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    panel.grid.major.y = element_line(linewidth = 7.5, colour = "grey95"),
    
    legend.position = "none",
    
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(hjust = 1)
  )


# save image
ggsave("tt2024w31.png", path = work_dir, height = 12, width = 10)
