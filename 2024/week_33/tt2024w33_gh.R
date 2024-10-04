library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)
library(ggrepel)
library(ggpattern)

# set working directory
wk_dir <- "~/R/tidytuesdays/2024/2024w33/"


# set fonts
font_add_google("Asap", "asap")
ft1 <- "asap"
showtext_auto()


# set colours
col_bg <- "white"
col_text <- "grey20"
col_blue1 <- "#A9D6E5"
col_blue2 <- "#468FAF"
col_blue3 <- "#014F86"


# set labeling
lab_title <- "**Shang*high* in world fair attraction**"
lab_subtitle <- "Total visitors (in millions) of world fairs in the <span style='color:#014F86'>**19<sup>th</sup>**</span>, <span style='color:#468FAF'>**20<sup>th</sup>**</span> and <span style='color:#A9D6E5'>**21<sup>st</sup>**</span> century."
lab_caption <- "**Data**: Wikipedia \u2022 **Graphic**: Amman Algenib"
lab_annotate <- "**Shanghai** ranks at the top<br>with **over 73 million** visitors."


# load original data
tuesdata <- tidytuesdayR::tt_load('2024-08-13')
worlds_fairs <- tuesdata$worlds_fairs


# wrangle data
wf_visitors <- worlds_fairs |> 
  filter(category == "World Expo") |> 
  select(city, country, start_year, end_year, visitors, attending_countries) |> 
  na.omit(visitors|attending_countries)


# plot data
city_year <- paste0(wf_visitors$city, " (", wf_visitors$start_year, ")")

ggplot(wf_visitors) +
  geom_vline(xintercept = c(20, 40, 60), linewidth = 0.1, color = "grey50") +
  geom_col(
    aes(visitors, as_factor(start_year)), 
    fill = case_when(
      wf_visitors$start_year >= 2000 ~ col_blue1,
      wf_visitors$start_year < 2000 & wf_visitors$start_year >= 1900 ~ col_blue2,
      wf_visitors$start_year < 1900 ~ col_blue3
    ),
    colour = NA) +
  geom_text(
    aes(
      if_else(
        str_length(city_year) <= 20 & visitors >= 12, 0.1, visitors
      ),
      as_factor(start_year),
      label = paste0(city, " (", start_year, ")")
      ),
    hjust = -0.05, family = ft1, size = 12,
    color = if_else(wf_visitors$visitors <= 12 | wf_visitors$start_year >= 2000, col_text, "white")
    ) +
  
  annotate("richtext", x = 52, y = 33.5, label = lab_annotate, family = ft1, size = 12, hjust = 0, lineheight = 0.4, fill = NA, label.color = NA) +
  
  labs(
    title = lab_title,
    subtitle = lab_subtitle,
    caption = lab_caption
  ) +
  
  geom_curve(aes(x = 52, y = 33.5, xend = 48, yend = 32), linewidth = 0.2) +
  
  scale_x_continuous(expand = c(0,0)) +

  theme_void() +
  theme(
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    plot.background = element_rect(fill = "white"),
    text = element_text(family = ft1, colour = col_text),
    plot.title = element_markdown(size = 70, margin = margin(b = 10)),
    plot.subtitle = element_markdown(size = 45, margin = margin(b = 35), lineheight = 0.4),
    plot.caption = element_markdown(size = 30, margin = margin(t = 20)),
    axis.line.y = element_line(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.text.x = element_text(size = 30, margin = margin(t = 0, b = 15), color = col_text)
  )


#save image
ggsave("tt2024w33v2.png", path = wk_dir, height = 11, width = 8)

