library(tidytuesdayR)
library(tidyverse)
library(waffle)
library(showtext)
library(ggtext)

# read data --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2024-06-11')

pride_index <- tuesdata$pride_index
pride_index_tags <- tuesdata$pride_index_tags


# set working directory --------------------------------------------------------

my_dir <- "~/R/tidytuesdays/2024/2024w24"


# setting fonts ----------------------------------------------------------------

font_add_google("Lobster", "lobster")
ft1 <- "lobster"

font_add_google("Roboto", "roboto")
ft2 <- "roboto"

showtext_auto()
showtext_opts(dpi = 300)


# setting text -----------------------------------------------------------------

txt_title <- "lgbtq-friendly communities"
txt_subtitle <- "number of US campuses by lgbtq index rating<br>(5 is highest) and community type"
txt_caption <- "**data:** campus pride index | **graphic:** amman algenib"
txt_fill <- "community type\nof the campus"

# setting colours --------------------------------------------------------------

col_community <- c("#E40303", "#FF8C00", "#FFED00", "#008026", "#24408E", "#732982")
col_bg <- "#f6aad0"
col_txt <- "white"


# setting levels ---------------------------------------------------------------

rating_levels <- c("5", "4.5", "4", "3.5", "3", "2.5", "2", "1.5", "1")
community_type_levels <- c("large urban city", "medium city", "small city", "small town", "very small town", "rural community")


# data wrangling ---------------------------------------------------------------

pride_index_mod <- pride_index |> 
  mutate(
    rating_f = factor(rating, levels = rating_levels),
    community_type_f = factor(community_type, levels = community_type_levels)) |>
  group_by(community_type_f) |> 
  count(rating) |> 
  arrange(desc(community_type_f))
  
pride_index_mod  


# plotting ---------------------------------------------------------------------

ggplot(pride_index_mod, aes(fill = community_type_f, values = n)) +
  geom_waffle(color = col_bg, size = 1.5, n_rows = 2, flip = TRUE, reverse = TRUE) +
  facet_wrap(~rating, nrow = 1, strip.position = "bottom") +
  theme_void() +

  scale_fill_manual(values = col_community) +
  scale_y_continuous(limits = c(0, 29), expand = c(0,0)) +
  
  labs(
    title = txt_title,
    subtitle = txt_subtitle,
    caption = txt_caption,
    fill = txt_fill
  ) +
  
  theme(
    plot.background = element_rect(fill = col_bg),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_markdown(hjust = 0.5, family = ft1, size = 50, colour = col_txt, margin = margin(b = 1, t = 0.7, unit = "cm")),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft2, size = 25, colour = col_txt, margin = margin(b = 1.5, unit = "cm"), lineheight = 1.3),
    plot.caption = element_markdown(hjust = 1, vjust = -1, family = ft2, size = 12, colour = col_txt, margin = margin(t = 1, unit = "cm")),
    
    strip.text.x = element_text(hjust = 0.5, vjust = 1, family = ft2, size = 20, colour = col_txt, face = "bold"),
    
    legend.position = "inside",
    legend.position.inside = c(0.18, 0.84),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0, family = ft2, size = 20, colour = col_txt, face = "bold", margin = margin(b = 0.5, unit = "cm")),
    legend.text = element_text(hjust = 0, family = ft2, size = 17, colour = col_txt),
    legend.key.spacing.y = unit(0.3, "cm"),
    legend.background = element_rect(colour = "white", fill = NA, linewidth = 1),
    legend.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")
  )


# saving plot ------------------------------------------------------------------

ggsave("tt2024w24.png", path = my_dir, height = 15, width = 8)

