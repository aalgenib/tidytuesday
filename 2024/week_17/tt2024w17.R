# loading packages -------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(ggstream)
library(ggimage)
library(patchwork)
library(glue)
library(png)
library(ggforce)
library(showtext)
library(ggtext)
library(camcorder)


# loading data -----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2024-04-23')


# setting fonts ----------------------------------------------------------------

font_add_google("Titan One", "titan")
font_add_google("Roboto", "roboto")

ft1 <- "titan"
ft2 <- "roboto"
showtext_auto()
showtext_opts(dpi = 300)


# setting colors ---------------------------------------------------------------

txt <- "#ECEFF6"
bg <- "#28468B"
pal <- c('#F9DBBD', '#FFA5AB', '#DA627D', '#A53860', "#450920")


# setting text  ----------------------------------------------------------------

my_hdl1 <- "SPACE"
my_hdl2 <- "RACE"
my_label1 <- "**How many objects were launched into Earth orbit or beyond since the beginning of space missions in the late 1950s? And who are the biggest players in these endeavours until today?**"
my_label2 <- "The objects include crewed spacecrafts, space station flights, landers, satellites and probes. The data is based on national registers of launches submitted to the UN by participating nations."
my_cap <- "Data: Our World In Data | Image: modified from pngmart.com | Graphic: Amman Algenib"


# setting file paths  ----------------------------------------------------------

my_dir <- "2024w17"


# data wrangling ---------------------------------------------------------------

space_obj <- tuesdata$outer_space_objects |>
  rename(country = Entity, year = Year) |>
  mutate(region = case_when(
    country %in% c("EUMETSAT", "European Space Agency", "European Union", "Eutelsat", "Inmarsat", "Intelsat", "Starsem", "Portugal", "Spain", "Italy", "France", "Switzerland", "Austria", "Netherlands", "Belgium", "Luxembourg", "United Kingdom", "Ireland", "Germany", "Denmark", "Norway", "Sweden", "Finland", "Poland", "Czechia", "Slovakia", "Hungary", "Slovenia", "Greece", "Bulgaria", "Romania", "Ukraine", "Lithuania", "Latvia", "Estonia") ~ "Europe",
    country %in% c("Russia", "Intersputnik") ~ "Russia",
    country == "United States" ~ "USA",
    country == "China" ~ "China"
  )) |>
  filter(!country %in% c("World", "NATO")) |>
  mutate(region = ifelse(is.na(region), "Rest of the World", region)) |> 
  group_by(region, year) |> 
  summarize(objects = sum(num_objects))

region_order <- c("USA", "Russia", "Europe", "China", "Rest of the World")
space_obj$region <- factor(space_obj$region, levels = region_order)

rocket <- readPNG(file.path(my_dir, "rocket_mod.png"), native = TRUE)


# recording --------------------------------------------------------------------

gg_record(
  dir = file.path(my_dir, "img"),
  device = "png",
  width = 5,
  height = 7,
  units = "in",
  dpi = 300
)


# plotting ---------------------------------------------------------------------

plt <- space_obj |> 
  ggplot() +
  
  # base plot
  geom_stream(aes(x = year, y = objects, fill = region), type = "ridge"
  ) +
  
  # axes scaling
  scale_x_continuous(limits = c(1957, 2023), expand = c(0, 0), position = "bottom", breaks = seq(1960, 2020, 20)) +
  scale_y_continuous(limits = c(0, 2500), expand = c(0,0), position = "right", breaks = seq(500, 2000, 500)
  ) +
  
  # labels
  annotate("text", x = 1961, y = 2490, label = my_hdl1, family = ft1, size = 15, colour = txt, hjust = 0, vjust = 1) +
  annotate("text", x = 1961, y = 2250, label = my_hdl2, family = ft1, size = 18.7, colour = txt, hjust = 0, vjust = 1) +
  annotate("text_box", x = 1960, y = 1900, label = my_label1, family = ft2, size = 3.5, colour = txt, hjust = 0, vjust = 1, width = unit(7.5, "cm"), fill = NA, box.colour = NA) +
  annotate("text_box", x = 1960, y = 1500, label = my_label2, family = ft2, size = 3.5, colour = txt, hjust = 0, vjust = 1, width = unit(7.5, "cm"), fill = NA, box.colour = NA) +
  labs(
    caption = my_cap,
    y = "total number of objects per year"
    ) +
  
  # theme
  theme_void() +
  theme(
    # plot
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    # axes
    axis.title.y = element_text(size = 9, colour = txt, vjust = -5, hjust = 0.5, angle = 90),
    axis.text.x = element_text(size = 9, colour = txt, margin = margin(t = 0, b = 0, r = 0, l = 0), vjust = -2),
    axis.text.y = element_text(size = 9, colour = txt, margin = margin(t = 0, b = 0, r = 0, l = 10), hjust = 0),
    axis.line = element_blank(),
    #legend
    legend.position = c(0.135, 0.2), 
    legend.justification = c(0.2, 0.2),
    legend.text = element_text(size = 9, colour = txt, family = ft2),
    legend.title = element_text(size = 9, colour = txt, family = ft2, face = "bold"),
    #plot
    plot.caption = element_text(size = 6, colour = txt, family = ft2, hjust = 1.2, vjust = -12),
    plot.background = element_rect(fill = bg)
    ) +
  
  # other
  guides(fill = guide_legend(
    title = "Country or Region")
  ) +
  
  scale_fill_manual(values = pal)


# adding image  ----------------------------------------------------------------

plt_combined <- plt +
  inset_element(p = rocket,
                left = 0.95,
                bottom = 0.92,
                right = 1.05,
                top = 1.0) +
  theme(
    rect = element_rect(fill = "transparent"),
    plot.background = element_rect(color = "transparent")
    )

plt_combined



