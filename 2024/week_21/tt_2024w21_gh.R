# loading packages -------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)
library(camcorder)


# loading data -----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2024-05-21')
emissions <- tuesdata$emissions


# setting fonts ----------------------------------------------------------------

font_add_google("News Cycle", "news_cycle")
ft1 <- "news_cycle"

showtext_auto()
showtext_opts(dpi = 300)


# setting text  ----------------------------------------------------------------

title_text <- "When Will The Sky\nClear Up Again?"
subtitle_text <- "According to the Paris Climate Agreement\nworld economies aim to achieve\nnet zero greenhouse gas emissions by 2050.\nShown here are annual total carbon dioxide\nemissions by 122 of the world’s largest\noil, gas, coal, and cement producers\nbetween 1900 an 2022 compared\nto the 20th-century average."
caption_text <- "Data: Carbon Majors | Graphic: Amman Algenib"
legend_title <- "Carbon Dioxide Equivalents (Megatons)"
label_text <- "2015 (Paris Climate Agreement)"


# setting colors  --------------------------------------------------------------

color_label <- "#ffb703"


# setting file paths  ----------------------------------------------------------

my_dir <- "~/R/tidytuesdays/2024w21"


# data wrangling ---------------------------------------------------------------

## calculating the average total annual emissions for the 20th century

average_emission <- emissions |> 
  filter(year %in% c(1900:2000)) |>
  group_by(year) |> 
  summarise(sum_emis = sum(total_emissions_MtCO2e)) |> 
  ungroup() |> 
  summarise(ave_emis = mean(sum_emis))


## calculating the difference to the mean

emissions_diff <- emissions |> 
  select(year, total_emissions_MtCO2e) |> 
  filter(year %in% c(1900:2022)) |> 
  group_by(year) |> 
  summarise(sum_tot_emis = sum(total_emissions_MtCO2e)) |> 
  mutate(
    emission_difference = sum_tot_emis - average_emission[[1]],
    x = 50)


# recording --------------------------------------------------------------------

gg_record(
  dir = file.path(my_dir, "img"),
  device = "png",
  width = 10,
  height = 15,
  units = "in",
  dpi = 300)


# creating the plot ------------------------------------------------------------

ggplot(emissions_diff) +
  geom_rect(aes(xmin = 0, xmax = x, ymin = year, ymax = year + 1, fill = emission_difference), show.legend = TRUE) +
  geom_hline(yintercept = 2015, colour = color_label, linewidth = 0.2) +
  
  scale_fill_gradient2(
    midpoint = 0, 
    low = color_label,
    high = "black",
    name = legend_title,
    breaks = c(30000, 20000, 10000, 0, -5000),
    labels = c("+30,000", "+20,000", "+10,000", "\u00B10", "-5,000"),
    guide = guide_legend(theme = theme(
      legend.direction = "horizontal",
      legend.title = element_text(hjust = 0, family = ft1, size = 18, margin = margin(b = 0.5, unit = "cm")),
      legend.title.position = "top",
      legend.text.position = "right",
      legend.text = element_text(hjust = 0, vjust = 0.5, angle = 0, family = ft1, size = 18),
      legend.key = element_rect(color = "black", fill = NA)))) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(1920, 2020, 20)) +
  scale_x_continuous(expand = c(0, 0)) +
  
  annotate("text", y = 1955, x = 8, label = title_text, hjust = 0, vjust = 1, family = ft1, size = 13, fontface = "bold") +
  annotate("text", y = 1941, x = 8, label = subtitle_text, hjust = 0, vjust = 1, family = ft1, size = 18/.pt) +
  annotate("text", y = 1903, x = 48, label = caption_text, hjust = 1, vjust = 0, family = ft1, size = 4) +
  annotate("text", y = 2012, x = 1, label = label_text, hjust = 0, vjust = 0.5, family = ft1, size = 18/.pt, colour = color_label) +
  annotate("text", y = 2000, x = 1, label = "2000", hjust = 0, vjust = 0.5, family = ft1, size = 18/.pt) +
  annotate("text", y = 1902, x = 1, label = "1900", hjust = 0, vjust = 0.5, family = ft1, size = 18/.pt) +
  
  theme_void() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    plot.background = element_rect(colour = NA, fill = "white"),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.ticks.y.left = element_line(colour = "black"),
    axis.minor.ticks.y.left = element_line(colour = "black"),
    axis.ticks.length.y = unit(-0.250,"cm"),
    legend.position = c(0.437, 0.1)
  )
