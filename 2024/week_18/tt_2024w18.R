# loading packages -------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)
library(camcorder)
library(ggforce)
library(patchwork)


# loading data -----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2024-04-30')

wwbi_data <- tuesdata$wwbi_data
wwbi_series <- tuesdata$wwbi_series
wwbi_country <- tuesdata$wwbi_country


# setting fonts ----------------------------------------------------------------

font_add_google("Roboto", "roboto")

ft1 <- "roboto"

showtext_auto()
showtext_opts(dpi = 300)


# setting colors ---------------------------------------------------------------

txt <- "grey10"
bg <- "grey95"
public <- "#2d45c5"
private <- "#cb3d7d"
#pal <- c('#F9DBBD', '#FFA5AB', '#DA627D', '#A53860', "#450920")


# setting text  ----------------------------------------------------------------

title_text <- "**Mind The Wage Gap!**"
subtitle_text <- "The **mean female-to-male wage ratio** in the <span style = 'color:#2d45c5'>**public**</span> versus <span style = 'color:#cb3d7d'>**private**</span> sector."
caption_txt <- "Data: World Bank | Graphic: Amman Algenib"
x_label <- "female/male wage ratio"

# setting file paths  ----------------------------------------------------------

my_dir <- "~/R/tidytuesdays/2024w18"


# factors ----------------------------------------------------------------------

region_levels <- c("Europe & Central Asia", "South Asia", "East Asia & Pacific", "Middle East & North Africa", "Sub-Saharan Africa", "Latin America & Caribbean")


# data wrangling ---------------------------------------------------------------

country_names <- wwbi_country |> 
  select(country_code, short_name, region) |> 
  rename(country_name = short_name)

country_names$region <- factor(country_names$region, levels = region_levels)

wwbi_data_mod <- full_join(
  wwbi_data,
  wwbi_series,
  by = "indicator_code")

wwbi_data_mod2 <- wwbi_data_mod |> 
  relocate(indicator_code, .after = indicator_name) |> 
  filter(str_detect(indicator_name, "wage ratio")) |> 
  filter(!str_detect(indicator_name, "median")) |> 
  rename(
    mean_FMWR = value, # median female-to-male wage ratio 
    sector = indicator_name
    ) |> 
  mutate(sector = 
    ifelse(
      str_detect(sector, "private"),
      "private",
      "public"
      )
    ) |> 
  select(!indicator_code)
  
wwbi_data_mod3 <- wwbi_data_mod2 |> 
  left_join(
    x = wwbi_data_mod2,
    y = country_names,
    by = "country_code"
  ) |> 
  relocate(c(country_name, region), .after = country_code)

wwbi_data_mod4 <- wwbi_data_mod3 |> 
  group_by(country_name, region, sector) |> 
  filter(
    year == max(year),
    !country_name == "Papua New Guinea",
    )

mean_FMWR_sector <- wwbi_data_mod4 |> 
  group_by(sector) |> 
  summarise(mean_FMWR_sec = mean(mean_FMWR))


# recording --------------------------------------------------------------------

gg_record(
  dir = file.path(my_dir, "img"),
  device = "png",
  width = 10,
  height = 20,
  units = "in",
  dpi = 300
)


# primary plot --------------------------------------------------------------------

p_primary <- ggplot(
  data = wwbi_data_mod4,
  aes(
    x = mean_FMWR,
    y = fct_reorder(country_name, mean_FMWR),
    )
  ) +
  
  # vertical line at equal wages
  geom_vline(
    xintercept = 1.0, linetype="solid", linewidth = 0.3, colour = "grey60"
    ) +
  
  # geom_point connector
  geom_line(
    aes(
      group = country_name),
    colour = c("grey50"), #"yellow", "blue", "orange", "purple", "grey"),
    linewidth = 0.5
    ) +
  
  # points for private or public mean FMWRs
  geom_point(
    aes(
      colour = sector,
      group = country_name),
    size = 2
    ) +
  
  # facetting
  facet_col(
    ~region, scales = "free_y", strip.position = "top", space = "free"
  ) +
  
  # scaling
  scale_x_continuous(limits = c(0,2), breaks = seq(0, 2, 0.2), expand = c(0, 0)) +
  scale_color_manual(values = c("public" = public, "private" = private)) +
  
  theme_minimal() +
  #scale_y_discrete(labels = y_axis_label)
  theme(
    plot.margin = unit(c(0, 2, 0, 2), "cm"),
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text = element_text(angle = 0, hjust = 1, vjust = 0, size = 10, face = "bold"),
    axis.text = element_text(family = ft1, size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.caption = element_text(family = ft1, size = 10),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.2, colour = "grey90")
    )

# secondary plot ---------------------------------------------------------------

explanatory_text <- tibble(
  x = c(0.1, 0.9), 
  y = 0, 
  label = c("**A wage ratio smaller than 1**<span style = 'color:grey40'><br>indicates that<br></span>**men earn more than women**.",
            "**A wage ratio larger than 1**<span style = 'color:grey40'><br>indicates that<br></span>**women earn more than men**.")
)

p_secondary <- explanatory_text %>% 
  ggplot() +
  geom_richtext(
    aes(x, y, label = label),
    size = 3,
    hjust = 0.5,
    vjust = 0,
    label.colour = txt
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 2), clip = 'off') +
  theme_void()

p_secondary


# final plot composition -------------------------------------------------------

layout <- c(
  area(t = 1, l = 1, b = 4, r = 4),
  area(t = 4, l = 1, b = 4, r = 4)
)

p_primary +
  p_secondary +
  plot_layout(design = layout) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_txt,
    
    theme = theme(
      
      plot.title = element_markdown(
        margin = margin(t = 1, b = 0.6, unit = "cm"),
        size = 25,
        hjust = 0.67,
        family = ft1
      ),
      
      plot.subtitle = element_markdown(
        margin = margin(b = 0.8, unit = "cm"),
        size = 15,
        hjust = 0.9,
        family = ft1,
        lineheight = 1.25
      ),
      
      plot.caption.position = "plot",
      plot.caption = element_markdown(
        margin = margin(t = 0.7, b = 0.4, unit = "cm"),
        hjust = 1,
        size = 7,
        colour = txt,
        family = ft1
      )
    )
  )

