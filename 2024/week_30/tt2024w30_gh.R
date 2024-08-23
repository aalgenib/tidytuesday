library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)


# get data
tuesdata <- tidytuesdayR::tt_load('2024-07-23')
songs <- tuesdata$songs


# set working directory
work_dir <- "~/R/tidytuesdays/2024/2024w30"


# set fonts
font_add_google("Archivo Black", "archivo_black")
font_add_google("Archivo", "archivo")
ft1 <- "archivo_black"
ft2 <- "archivo"
showtext_auto()


# set colours
col_family <- "#E8C547"
col_bg <- "#30323D"


# set labels
txt_subtitle = "How many *American Idol* contestants chose a song by Stevie Wonder in seasons 1 to 12?"
txt_caption = "**Data**: Wikipedia \u2022 **Graphic**: Amman Algenib"


# wrangle data
famous_artists <- songs |>   
  group_by(season) |> 
  count(artist) |> 
  mutate(
    season = as.numeric(str_remove_all(season, "Season_0|Season_")),
    n = as.numeric(n)) |>
  filter(season < 13, artist == "Stevie Wonder")
    
sw <- "STEVIEWONDER"
sw_vec <- str_split(sw, "")[[1]]
       
famous_artists$sw <- sw_vec
famous_artists$x <- c(1:6, 1:6)
famous_artists$y <- c(rep(2, 6), rep(1, 6))


# plot data
ggplot(famous_artists) +
  geom_text(aes(x = x, y = y, label = sw), size = famous_artists$n*4, family = ft1, fontface = "bold", colour = col_family) +
  geom_text(aes(x, y, label = paste0("Season ", season)), vjust = famous_artists$n+4, size = 2, family = ft2, colour = col_family) +
  geom_text(aes(x, y, label = if_else(n > 1, paste0(n, " contestants"), paste0(n, " contestant"))), vjust = famous_artists$n+6, size = 2, family = ft2, colour = col_family) +
  coord_fixed() +
  
  annotate("richtext", x = 3.5, y = 0.32, label = txt_subtitle, family = ft2, colour = col_family, size = 4, fill = NA, label.color = NA) +
  annotate("richtext", x = 3.5, y = 0.2, label = txt_caption, family = ft2, colour = col_family, size = 2, fill = NA, label.color = NA) +
  
  scale_y_continuous(limits = c(0.2, 2.5)) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "none"
  )

ggsave("tt2024w30.png", path = work_dir, height = 4.5, width = 10)

