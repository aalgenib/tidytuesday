library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)

# working dir
work_dir <- "~/R/tidytuesdays/2024/2024w29"

# load data
tuesdata <- tidytuesdayR::tt_load('2024-07-16')
ewf_appearances <- tuesdata$ewf_appearances

# wrangle data
popularity <- ewf_appearances %>% 
  filter(
    home_team == 1,
    !is.na(attendance)) %>% 
  select(season, attendance) %>% 
  mutate(
    season_mod = str_extract(season, "\\d{2}(?=-)") %>% 
      paste0("/", str_extract(season, "(?<=-\\d{2})\\d{2}"))
  ) %>% 
  group_by(season_mod) %>% 
  summarise(average_attendance = round(mean(attendance), 0)) %>% 
  ungroup() %>% 
  mutate(season_mod = str_replace(season_mod, "23/24", "23/24*"))

# set labels
txt_title <- "English Womens's Football League Attendance"
txt_subtitle <- "Average attendance per game (seasons 11/11 to 23/24)"
txt_caption <- "*until May 18, 2024<br>**Data**: The English Women's Football (EWF) Database \u2022 **Chart**: Amman Algenib"

#set colours
col_column <- "#411C9F"
col_bg <- "grey95"

#set fonts
font_add_google("Asap", "asap")
ft1 <- "asap"
showtext_opts(dpi = 300)

#plot data
ggplot(
  popularity,
  aes(season_mod, average_attendance),
  ) +
  geom_col(fill = col_column) +
  geom_richtext(
    data = tibble(y_text = seq(1000, 6000, by = 1000)), 
    aes(x = 0.7, y = y_text, label = paste0(y_text/1000, "K")),
    vjust = 0, label.color = NA, fill = NA, family = ft1, color = "grey"
    ) +
  geom_richtext(
    data = popularity,
    aes(x = season_mod, y = 200, label = average_attendance),
    fill = NA, label.colour = NA, colour = "white", size = 5
  ) +
  
  scale_y_continuous(
    limits = c(0, 6500), 
    breaks = seq(0, 6500, by = 1000),
    expand = c(0, 0)
  ) +
  
  labs(
    title = txt_title,
    subtitle = txt_subtitle,
    caption = txt_caption
  ) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = col_bg),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.caption = element_markdown(
      margin = margin(t = 0.6, r = 0, b = 0, l = 0, "cm"),
      family = ft1, size = 12, lineheight = 2),
    plot.title = element_markdown(
      margin = margin(t = 0, r = 0, b = 0.2, l = 0, unit = "cm"), 
      family = ft1, face = "bold", size =24),
    plot.subtitle = element_markdown(
      margin = margin(t = 0, r = 0, b = 0.8, l = 0, unit = "cm"),
      family = ft1, size = 20),
    
    panel.grid.major.y = element_line(colour = "grey80"),
    
    axis.line.x = element_line(color = "grey50"),
    axis.text.x = element_markdown(
      margin = margin(t = 7, b = 0, r = 0, l = 0),
      size = 15),
    axis.text.y = element_blank()
  )

ggsave("tt2024w29.png", path = work_dir, height = 10, width = 10)
