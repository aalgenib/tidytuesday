# load packages ----------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(ggrepel)
library(ggbeeswarm)
library(showtext)
library(ggtext)
library(camcorder)
library(ggforce)
library(patchwork)


# define fonts -----------------------------------------------------------------

font_add_google("Roboto", "roboto")
ft1 <- "roboto"

showtext_auto()
showtext_opts(dpi = 300)


# define colors ----------------------------------------------------------------

txt <- "#143642"
bg <- "#dad2d8"
male <- "#0F8B8D"
female <- "#EC9A29"


# define text ------------------------------------------------------------------

title_text <- "**The Age Of Success**"
subtitle_text <- "Average age of a single artist at which <span style = 'color:#EC9A29'>**her**</span> or <span style = 'color:#0F8B8D'>**his**</span> album was listed<br>in the *Rolling Stone's* '500 Greatest Albums of All Time', including<br>lists from 2003, 2012 and 2020."
caption_txt <- "**Data:** Rolling Stone 500 **\u00b7** **Graphic:** Amman Algenib"


# set file paths  --------------------------------------------------------------

my_dir <- "~/R/tidytuesdays/2024w19"


# load data --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, week = 19)
rs <- tuesdata$rolling_stone


# wrangle data -----------------------------------------------------------------

rs_mod <- rs |> 
  rename(
    "2003" = rank_2003,
    "2012" = rank_2012,
    "2020" = rank_2020,
    artist_name = clean_name
  ) |> 
  filter(artist_member_count == 1) |>
  pivot_longer(
    c("2003", "2012", "2020"),
    names_to = "rank_year",
    values_to = "rank"
      ) |> 
  mutate(
    genre = str_replace(genre, "Blues/Blues ROck", "Blues/Blues Rock"),
    artist_name = str_replace(artist_name, "Billie EIlish", "Billie Eilish")
    ) |> 
  select(
    artist_name, album, release_year, genre, rank_year, rank, weeks_on_billboard, peak_billboard_position, artist_gender, ave_age_at_top_500
  ) |> 
    group_by(artist_name, artist_gender) |> 
  summarise(ave_age = round(mean(ave_age_at_top_500)))

# record plot building ---------------------------------------------------------

gg_record(
  dir = file.path(my_dir, "img"),
  device = "png",
  width = 15,
  height = 20,
  units = "cm",
  dpi = 300
)


# build plot -------------------------------------------------------------------

gp <- ggplot(rs_mod) +
  geom_beeswarm(
    aes(x = artist_gender, y = ave_age, color = artist_gender),
    method = "compactswarm",
    cex = 3,
    side = 0,
    shape= 19,
    size = 3.7
  )

gp +
  annotate("text", x = 2.05, y = 74, hjust = 0, label = "John Lee Hooker", colour = male) +
  annotate("text", x = 2.05, y = 68, hjust = 0, label = "Hank Williams", colour = male) +
  annotate("text", x = 2.05, y = 19, hjust = 0, label = "LL Cool J", colour = male) +
  annotate("text", x = 1.05, y = 68, hjust = 0, label = "Patsy Cline", colour = female) +
  annotate("text", x = 1.05, y = 54, hjust = 0, label = "Loretta Lynn", colour = female) +
  annotate("text", x = 1.1, y = 18.5, hjust = 0, label = "Billie Eilish", colour = female) +
  annotate("text", x = 1.05, y = 16, hjust = 0, label = "Aaliyah", colour = female) +
  annotate("text", x = 0.45, y = 61.5, hjust = 0, label = "aged 60", colour = txt, size = 3, fontface = "italic") +
  annotate("text", x = 0.45, y = 41.5, hjust = 0, label = "aged 40", colour = txt, size = 3, fontface = "italic") +
  annotate("text", x = 0.45, y = 21.5, hjust = 0, label = "aged 20", colour = txt, size = 3, fontface = "italic") +
  
  scale_color_manual(values = c("Male" = male, "Female" = female)) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = bg, colour = NA),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major.y = element_line(colour = txt, size = 0.08)

  ) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_txt,
    
    theme = theme(
      
      plot.title = element_markdown(
        margin = margin(t = 1, b = 0.6, unit = "cm"),
        size = 25,
        hjust = 0,
        family = ft1,
        colour = txt
      ),
      
      plot.subtitle = element_markdown(
        margin = margin(b = 0.2, unit = "cm"),
        size = 12,
        hjust = 0,
        family = ft1,
        lineheight = 1.25,
        colour = txt
      ),
      
      plot.caption.position = "plot",
      plot.caption = element_markdown(
        margin = margin(t = 0.7, b = 0.4, unit = "cm"),
        hjust = 1,
        size = 7,
        colour = txt,
        family = ft1
      ),
      
      plot.background = element_rect(fill = bg)
    )
  )
           
