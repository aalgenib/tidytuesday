library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(showtext)


# read raw data

tuesdata <- tidytuesdayR::tt_load(2024, week = 22)

spending_2020 <- tuesdata$spending_2020
planting_2020 <- tuesdata$planting_2020
harvest_2020 <- tuesdata$harvest_2020


# wrangle data

spending_2020_mod <- spending_2020 |> 
  select(vegetable, price_with_tax) |>
  rename(price_wt = price_with_tax) |> 
  group_by(vegetable) |> 
  summarise(total_price_wt = sum(price_wt)) |> 
  filter(!vegetable %in% c("raised garden blend", "enriched topsoil", "dirt", "hot peppers", "potatoes"))

planting_2020_mod <- planting_2020 |> 
  select(vegetable, number_seeds_planted) |> 
  group_by(vegetable) |> 
  summarise(total_number_seeds_planted = sum(number_seeds_planted)) |> 
  filter(!is.na(total_number_seeds_planted))

harvesting_2020_mod <- harvest_2020 |> 
  select(vegetable, weight) |> 
  group_by(vegetable) |> 
  summarise(total_weight_harvest = sum(weight))

tbl_garden_2020 <- reduce(
  list(
    spending_2020_mod, 
    planting_2020_mod, 
    harvesting_2020_mod),
  left_join, by = "vegetable") |> 
  drop_na() |> 
  mutate(total_weight_harvest = total_weight_harvest/1000)


# define text

title_text <- "Which is Lisa's superfood?"
subtitle_text <- "In terms of yield per cost the zucchini ranks among the top vegetables<br>in Lisa's jungle garden, especially given the low amount of seeds planted.<br>Note the log10 scaling of the axes. Excluded are vegetables like hot peppers<br>and potatoes, which were kindly gifted from a friend or were otherwise<br>not payed for."
caption_text <- "Data by Lisa Lendway | Graphic by Amman Algenib"
guide_text <- "Total number\nof seeds\nplanted"


# set font(s)

font_add_google("Indie Flower", "indie")
ft1 <- "indie"

showtext_auto()
showtext_opts(dpi = 300)


# colors

col_text <- "#264027"
col_plot_fill <- "#6f732f"
col_panel_fill <- "#b38a58"
col_border <- "#72360b"

col_veg <- c(
  "basil" = "#1d8427",
  "beans" = "#681d1d",
  "beets" = "#601344",
  "broccoli" = "#139664",
  "carrots" = "#e06923",
  "cilantro" = "#1d8427",
  "corn" = "#e0c023",
  "cucumbers" = "#1d8427",
  "edamame" = "#1d8427",
  "jalapeño" = "#1d8427",
  "kale" = "#139664",
  "kohlrabi" = "#83d197",
  "lettuce" = "#1d8427",
  "onions" = "#d1b185",
  "peas" = "#1d8427",
  "peppers" = "#e82719",
  "pumpkins" = "#d6610e",
  "radish" = "#c11d53",
  "spinach" = "#1d8427",
  "squash" = "#f7ee71",
  "tomatoes" = "#e82719",
  "zucchini" = "#1d8427"
)


# set directory

my_dir <- "~/R/tidytuesdays/2024w22/plot_image"


# create plot

ggplot(data = tbl_garden_2020,
       aes(
         x = total_weight_harvest,
         y = total_price_wt)) +
  
  geom_point(aes(
    size = total_number_seeds_planted,
    colour = vegetable), show.legend = c(size = TRUE, colour = FALSE)) +
  
  geom_text_repel(
        aes(label = vegetable),
        box.padding = 0.47,
        segment.color = NA,
        colour = col_text,
        family = ft1) +
  
  # annotations
  annotate("text", x = 4, y = 19, label = "Lisa's Jungle Garden", hjust = 0.5, family = ft1, size = 11, colour = col_border) +
  
  # scales
  scale_x_continuous(name = "total yield (kilograms)", trans = "log10") +
  scale_y_continuous(name = "total cost (dollars)", trans = "log10") +
  scale_colour_manual(values = col_veg) +
  
  
  theme(
    
    #plot settings
    plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"),
    plot.background = element_rect(fill = col_plot_fill, color = NA, linewidth = 4),
    plot.title = element_markdown(
      margin = margin(t = 1, b = 0.6, unit = "cm"),
      size = 25,
      hjust = 0,
      family = ft1,
      face = "bold",
      colour = col_text
    ),
    plot.subtitle = element_markdown(
      margin = margin(b = 0.8, unit = "cm"),
      size = 15,
      hjust = 0,
      family = ft1,
      lineheight = 1.25,
      colour = col_text
    ),
    plot.caption = element_markdown(
      margin = margin(t = 0.2, b = 0.4, unit = "cm"),
      hjust = 1,
      size = 10,
      colour = col_text,
      family = ft1
    ),
    plot.caption.position = "plot",
    
    # panel settings
    panel.background = element_rect(fill = col_panel_fill, color = col_border, linewidth = 7),
    panel.grid = element_blank(),
    
    # axes settings
    axis.ticks = element_blank(),
    axis.title.x = element_text(colour = col_text, size = 14, margin = margin(t = 0.3, b = 0.3, unit = "cm"), hjust = 0.5,  family = ft1),
    axis.title.y = element_text(colour = col_text, size = 14, margin = margin(l = 0.1, r = 0.3, unit = "cm"), vjust = 0.5, angle = 90, family = ft1),
    axis.text = element_text(colour = col_text, size = 10, family = ft1),
  
    # legend settings
    legend.position.inside = c(0.12, 0.825),
    legend.background = element_rect(colour = col_border, fill = col_panel_fill),
    legend.margin = margin(t = 0.2, b = 0.2, l = 0.2, r = 0.2, unit = "cm"),
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = col_text, family = ft1),
    legend.title = element_text(colour = col_text, family = ft1)
    ) +
  
  # guides settings
  guides(
    size = guide_legend(
      override.aes = list(color = col_text),
      position = "inside"
      )
    ) +
  
  # labels
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    size = guide_text,
  )


# save plot
ggsave(path = my_dir, filename = "tt2024w22.png", width = 20, height = 25, units = "cm", dpi = 300)
 
