library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)
library(ggforce)

# set working directory
work_dir <- "~/R/tidytuesdays/2024/2024w32/"


# load orignial data
tuesdata <- tidytuesdayR::tt_load('2024-08-06')
olympics <- tuesdata$olympics


# set fonts
font_add_google("Archivo Black", "arch_black")
font_add_google("Archivo", "arch")
ft1 <- "arch_black" 
ft2 <- "arch"
showtext_auto()


# set colours
col_bg <- "grey90"


# set labeling
txt_title <- "Guess who dominates olympic basketball?"
txt_subtitle <- "Gold medal winners in the men's olympic basketball tournaments between 1936 and 2020"  
txt_caption <- "**Data**: Kaggle \u2022 **Graphic**: Amman Algenib"


# wrangle data
bball <- olympics |> 
  select(sex, noc, sport, year, medal) |> 
  filter(
    sport == "Basketball",
    sex == "M",
    medal == "Gold") |> 
  na.omit(medal) |> 
  group_by(year, noc) |> 
  reframe() |> 
  add_row(year = 2020, noc = "USA") |> 
  mutate(
    x = rep(1:5, times = 4),
    y = rep(4:1, each = 5),
    fill_circ1 = case_when(
      noc == "USA" ~ "#0a3161",
      noc == "URS" ~ "#cc0000",
      noc == "YUG" ~ "#003893",
      noc == "ARG" ~ "#74acdf"),
    fill_circ2 = case_when(
      noc == "USA" ~ "#b31942",
      noc == "URS" ~ "#cc0000",
      noc == "YUG" ~ "#ffffff",
      noc == "ARG" ~ "#ffffff"),
    fill_circ3 = case_when(
      noc == "USA" ~ "#ffffff",
      noc == "URS" ~ "#cc0000",
      noc == "YUG" ~ "#dd0000",
      noc == "ARG" ~ "#74acdf")
  )


# create plot
ggplot(bball, aes(x = x, y = y)) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.4, fill = I(fill_circ1)), colour = NA) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.35, fill = I(fill_circ2)), colour = NA) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.3, fill = I(fill_circ3)), colour = NA) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.25), fill = "gold", colour = NA) +
  
  geom_richtext(aes(label = noc), vjust = 0.5, family = ft1, size = 23, fill = NA, label.colour = NA) +
  geom_richtext(aes(label = year), vjust = 1.5, family = ft2, size = 10, fill = NA, label.colour = NA) +
  
  coord_fixed() +
  
  scale_y_continuous(limits = c(0.6, 4.4)) +
  scale_x_continuous(limits = c(0.6, 5.4)) +
  
  labs(
    title = txt_title,
    subtitle = txt_subtitle,
    caption = txt_caption
  ) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = col_bg, colour = NA),
    plot.margin = unit(rep(0.5, times = 4), "cm"),
    plot.title = element_markdown(family = ft1, size = 60, hjust = 0.5),
    plot.subtitle = element_markdown(family = ft2, size = 40, hjust = 0.5),
    plot.caption = element_markdown(family = ft2, size = 25, hjust = 0.5)
  )


# save image
ggsave("tt2024w32.png", path = work_dir, height = 8.65, width = 10)

