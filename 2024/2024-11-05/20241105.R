
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(patchwork)
library(camcorder)
library(glue)


# set path ----------------------------------------------------------------

my_path <- c("~", "R", "tidytuesdays")


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-11-05")
tbl <- tuesdata$democracy_data

# Load fonts --------------------------------------------------------------

font_add_google("Asap", "asap")
ft1 <- "asap"
showtext_auto()


# Define colours ----------------------------------------------------------

col_bg <- "white"
col_text <- "grey20"
col_dem <- "#006ba2"
col_female <- "#db444b"


# Data wrangling ----------------------------------------------------------

toi <- c("is_democracy", "is_female_president") # toi = topics of interest

tbl_toi <- tbl |> 
  select(country_name, year, is_democracy, is_presidential, is_female_president) |> 
  filter(is_democracy == TRUE & is_presidential == TRUE) |> 
  group_by(year) |> 
  summarise(across(toi, ~ sum(., na.rm = TRUE))) |> 
  rename(
    female_presidents = is_female_president,
    democracy = is_democracy
  )

tbl_toi <- tbl_toi |> 
  pivot_longer(
    cols = c("female_presidents", "democracy"),
    names_to = "toi",
    values_to = "n"
  )

max_dem <- tbl_toi |> 
  filter(toi == "democracy") |> 
  filter(n == max(n)) |> 
  slice(1)

max_female_pres <- tbl_toi |> 
  filter(toi == "female_presidents") |> 
  filter(n == max(n)) |> 
  slice(1)

max_val <- rbind(max_dem, max_female_pres)

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path(paste(my_path, collapse = "/"), "2024", "2024-11-05", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

txt_title <- "**Shouldn't the world give more women a chance?**"
txt_subtitle <- "Global number of countries with a <span style='color:#006ba2'>**presidential democracy**</span> in relation to the global number of countries<br>with a <span style='color:#db444b'>**presidential democracy lead by a female president**</span> (1950--2020). Highest values are indicated<br>by a point, respectively."
txt_caption <- "**Data**: Bjørnskov, C. and Rode, M. *Rev Int Organ* 2020;15:531–551 \u2022 **Graphic**: Amman Algenib"


# Plot --------------------------------------------------------------------

ggplot() +
  geom_line(data = tbl_toi, aes(year, n, colour = toi), linewidth = 0.8) +
  geom_point(data = max_val, aes(year, n, colour = toi), size = 2) +
  
  annotate(geom = "text", x = max_val$year[[1]], y = max_val$n[[1]], label = "65 countries\n in 2014", hjust = 0.5, vjust = 2.2, size = 25/.pt, lineheight = 0.3, family = ft1) +
  annotate(geom = "text", x = max_val$year[[2]], y = max_val$n[[2]], label = "8 countries\nin 2011", hjust = 0.5, vjust = -0.5, size = 25/.pt, lineheight = 0.3, family = ft1) +
  
  labs(
    title = txt_title,
    subtitle = txt_subtitle,
    caption = txt_caption
  ) +
  
  scale_x_continuous(breaks = seq(1950, 2030, by = 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 20), expand = c(0.01, 0.01)) +
  scale_color_manual(values = c(col_dem, col_female)) +
  
  theme_void() +
  theme(
    
    text = element_text(family = ft1, colour = col_text),
    
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    plot.background = element_rect(fill = col_bg),
    plot.title = element_markdown(size = 55, margin = margin(b = 5)),
    plot.subtitle = element_markdown(size = 30, margin = margin(b = 15), lineheight = 0.4),
    plot.caption = element_markdown(size = 25),
    
    panel.grid.major.y = element_line(linewidth = 0.1, colour = "grey60"), 
    
    axis.line = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.text.x = element_text(size = 25, margin = margin(t = 3, b = 15), color = col_text),
    axis.text.y = element_text(size = 25, margin = margin(r = 7, l = 0), color = col_text, hjust = 1),
    
    legend.position = "none"
  )

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path(paste(my_path, collapse = "/"), "2024", "2024-11-05", paste0("20241105", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = col_bg
)
