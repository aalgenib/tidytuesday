library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)
library(maps)
library(patchwork)


# set working directory --------------------------------------------------------

my_dir <- "/home/bastian/R/tidytuesdays/2024/2024w23"


# getting data -----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2024-06-04')
tbl_cheese <- tuesdata$cheeses


# data wrangling ---------------------------------------------------------------

tbl_cheese_map <- tbl_cheese |>
  select(country, region, cheese) |>
  separate_longer_delim(country, delim = ", ") |> 
  filter(country == "United States") |> 
  mutate(region = case_when(
    str_detect(region, "NY") ~ "New York",
    str_detect(region, "Peekskill") ~ "New York",
    str_detect(region, "VT") ~ "Vermont",
    str_detect(region, "Shelburne Farms") ~ "New Hampshire",
    str_detect(region, "NJ") ~ "New Jersey",
    str_detect(region, "MN") ~ "Minnesota",
    str_detect(region, "California") ~ "California",
    str_detect(region, "Nicasio") ~ "California",
    str_detect(region, "Wisconsin") ~ "Wisconsin",
    str_detect(region, "Oregon") ~ "Oregon",
    str_detect(region, "Fairview") ~ "Oregon",
    str_detect(region, "Bloomdale") ~ "Ohio",
    str_detect(region, "CT") ~ "Connecticut",
    str_detect(region, "Kimball") ~ "Michigan",
    str_detect(region, "MI") ~ "Michigan",
    str_detect(region, "Port Townsend") ~ "Washington",
    str_detect(region, ", ") ~ str_extract(region, "\\b\\w+$"),
    .default = region)) |> 
  filter(!region %in% c("Apulia", "Northwest")) |> 
  filter(!is.na(region)) |> 
  mutate(region = tolower(region)) |>
  select(region, cheese) |> 
  group_by(region) |> 
  summarise(n_cheese = n())


## get all US state names
us_states_codes <- read_csv("/home/bastian/R/tidytuesdays/2024/2024w23/states.csv") |> 
  rename(region = State, code = Abbreviation)

tbl_cheese_map2 <- full_join(
  tbl_cheese_map,
  states_map,
  by = "region") |> 
  mutate(n_cheese = if_else(is.na(n_cheese), 0, n_cheese)) |> 
  mutate(n_cheese_cat = case_when(
    n_cheese > 30 ~ ">30",
    n_cheese <= 30 & n_cheese > 20 ~ "21-30",
    n_cheese <= 20 & n_cheese > 10 ~ "11-20",
    n_cheese <= 10 & n_cheese > 0 ~ "1-10",
    n_cheese == 0 ~ "0"))

levels_cheese_cat <- c(">30", "21-30", "11-20", "1-10", "0")

tbl_cheese_map2$n_cheese_cat <- factor(tbl_cheese_map2$n_cheese_cat, levels = levels_cheese_cat)


# setting fonts ----------------------------------------------------------------

font_add_google("Concert One", "concert")
ft1 <- "concert"

font_add_google("Ubuntu", "ubuntu")
ft2 <- "ubuntu"

showtext_auto()
showtext_opts(dpi = 300)


# setting colors  --------------------------------------------------------------

col_bg <- "#EBF2FF"
col_ft1 <- "#FFC300"
col_ft2 <- "#001029"
col_pal <- c("#FFC300", "#FFD95C", "#FFE799", "#FFF5D6", "white")


# tibbles for labels  ----------------------------------------------------------

## Wisconsin
x_wisc_pt <- -91
x_wisc_txt <- -105
y_wisc <- 40.5
max_wisc <- tbl_cheese_map2 |> 
  filter(region == "wisconsin") |>
  slice(1) |> 
  pull(n_cheese)

point_wisc <- tibble(
  x = x_wisc_pt,
  y = y_wisc
)

text_anchor_wisc <- tibble(
  x = x_wisc_txt,
  y = y_wisc
)

## california
x_calif_pt <- -114.2
x_calif_txt <- -105
y_calif <- 34.8
max_calif <- tbl_cheese_map2 |> 
  filter(region == "california") |> 
  slice(1) |> 
  pull(n_cheese)

point_calif <- tibble(
  x = x_calif_pt,
  y = y_calif
)

text_anchor_calif <- tibble(
  x = x_calif_txt,
  y = y_calif
)

## lines for both states
lines <- tibble(
  x = c(x_wisc_pt, x_wisc_txt, x_calif_pt, x_calif_txt),
  y = c(y_wisc, y_wisc, y_calif, y_calif),
  group = c("wisc", "wisc", "calif", "calif")
)


# create plot ------------------------------------------------------------------

## overall layout

layout <- c(
  area(t = 1, l = 1, b = 3, r = 3),
  area(t = 1, l = 1, b = 3, r = 3)
)


## main plot

plt1 <- ggplot() +
  geom_polygon(data = tbl_cheese_map2, aes(x = long, y = lat, fill = n_cheese_cat, group = group), colour = col_bg, linewidth = 1.5) +
  coord_map("polyconic") +

  scale_fill_manual(values = col_pal, name = "") +
  
  labs(
    title = "Cheese States of America",
    subtitle = "number of unique cheese varieties by state of origin",
    caption = "**Data**: cheese.com<br>**Graphic**: Amman Algenib"
  ) +
  
  theme_void() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.title = element_text(hjust = 0.5, family = ft1, size = 63, color = col_ft1, margin = margin(c(1, 0, 0, 0), unit = "cm")),
    plot.subtitle = element_text(hjust = 0.5, family = ft2, size = 30, color = col_ft2, margin = margin(c(0.5, 0, 0, 0), unit = "cm")),
    plot.caption = element_markdown(family = ft2, size = 15, color = col_ft2, hjust = 1, lineheight = 1.25),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 17, family = ft2),
    legend.margin = margin(c(0, 0, 1, 0), unit = "cm")
  )


## labels plot

plt2 <- ggplot() +
  geom_point(data = point_wisc, aes(x, y), size = 5) +
  geom_point(data = point_calif, aes(x, y), size = 5) +
  geom_line(data = lines, aes(x, y, group = group), linewidth = 1) +
  geom_text(data = text_anchor_wisc, aes(x, y), label = "Wisconsin", hjust = 0, vjust = -0.5, size = 6, family = ft2, color = col_ft2) +
  geom_text(data = text_anchor_wisc, aes(x, y), label = max_wisc, hjust = 0, vjust = 1.25, size = 18, family = ft2, color = col_ft2) +
  geom_text(data = text_anchor_calif, aes(x, y), label = "California", hjust = 1, vjust = -0.5, size = 6, family = ft2, color = col_ft2) +
  geom_text(data = text_anchor_calif, aes(x, y), label = max_calif, hjust = 1, vjust = 1.25, size = 15,family = ft2, color = col_ft2) +
  theme_void() +
  xlim(-124.6813, -67.00742) +
  ylim(25.12993, 49.38323)


## joined plots

plt1 + plt2 + plot_layout(design = layout)


# save plot --------------------------------------------------------------------

ggsave("tt2024w23.png", path = my_dir, height = 15, width = 15)

