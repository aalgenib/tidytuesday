
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(camcorder)
library(patchwork)
library(glue)
library(ggforce)


# set path ----------------------------------------------------------------

my_path <- c("~", "R", "tidytuesdays")


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-11-19")
tbl <- tuesdata$episode_metrics

# Load fonts --------------------------------------------------------------

font_add_google("Asap", "asap")
ft1 <- "asap"
showtext_auto()


# Define colours ----------------------------------------------------------

col_bg <- "white"
col_text <- "grey20"
col_quest <- "#006ba2"
col_exclam <- "#db444b"
# col_c <- "#3ebcd2"
# col_d <- "#379a8b"
# col_e <- "#ebb434"
# col_f <- "#b4ba39"
# col_g <- "#9a607f"
# col_h <- "#d1b07c"
col_grey <- "#758d99"



# Data wrangling ----------------------------------------------------------

glimpse(tbl)

tbl_mod <- tbl |> 
  select(season, episode, question_ratio, exclamation_ratio) |> 
  group_by(season) |> 
  summarise(
    mean_quest_ratio = mean(question_ratio),
    mean_exclam_ratio = mean(exclamation_ratio))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path(paste(my_path, collapse = "/"), "2024", "2024-11-19", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

txt_title <- "**Bob: Declining!!!<br>Linda: Consistant???**"
txt_subtitle <- "Average proportion of lines of dialogue per episode and season that contain at least one<br><span style='color:#db444b'>**exclamation point**</span> or one <span style='color:#006ba2'>**question mark**</span> in the animated sitcom 'Bob's Burgers'."
txt_cap <- "**Data**: bobsburgersR \u2022 **Graphic**: Amman Algenib"


# Plot --------------------------------------------------------------------

ggplot(tbl_mod) +
  
  geom_richtext(
    data = tibble(y_text = seq(0.05, 0.2, by = 0.05)),
    aes(x = 1.3, y = y_text, label = paste0(y_text*100, "%")),
    vjust = 0.15, size = 25/.pt, fill = NA, label.colour = NA, colour = col_grey
  ) +
  
  geom_line(aes(season, mean_exclam_ratio), colour = col_exclam) +
  geom_line(aes(season, mean_quest_ratio), colour = col_quest) +
  

  labs(
    title = txt_title,
    subtitle = txt_subtitle,
    caption = txt_cap
  ) +

  scale_x_continuous(breaks = seq(1, 14, by = 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.21), expand = c(0, 0)) +

  theme_void() +
  theme(
      text = element_text(family = ft1, colour = col_text),

      plot.margin = margin(1, 1, 1, 1, unit = "cm"),
      plot.background = element_rect(fill = col_bg),
      plot.title = element_markdown(size = 55, margin = margin(b = 7), lineheight = 0.4),
      plot.subtitle = element_markdown(size = 30, margin = margin(b = 15), lineheight = 0.4),
      plot.caption = element_markdown(size = 25),
      
      axis.line = element_line(),
      axis.text.x = element_text(size = 25, margin = margin(t = 3, b = 4)),
      
      panel.grid.major.y = element_line(colour = col_grey, linewidth = 0.1)
  )
  
  
# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path(paste(my_path, collapse = "/"), "2024", "2024-11-19", paste0("20241119", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = col_bg
)
