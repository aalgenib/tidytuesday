library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)
library(grid)


# load data --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, week = 26)
lgbtq_movies <- tuesdata$lgbtq_movies


# set working directory --------------------------------------------------------

my_dir <- "~/R/tidytuesdays/2024/2024w26"


# data wrangling ---------------------------------------------------------------

movie_votes <- lgbtq_movies |> 
  select(title, original_language, release_date, vote_average, vote_count) |> 
  mutate(release_date = as.Date(release_date)) |> 
  filter(
    vote_count >= 1000,
    original_language != "it") |> 
  drop_na() |> 
  mutate(original_language = case_when(
    original_language == "en" ~ "English",
    original_language == "fr" ~ "French",
    original_language == "es" ~ "Spanish",
    original_language == "ko" ~ "Korean",
    original_language == "th" ~ "Thai",
    )
  ) |> 
  mutate(original_language = factor(original_language, levels = c("English", "French", "Spanish", "Korean", "Thai")))


# set text content -------------------------------------------------------------

txt_title <- "Among the vast amount of english movies relevant to<br>the LGBTQ+ community a Korean film stands out"
txt_subtitle <- "Average user score of movies in *The Movie Database* (TMDB) with assigned LGBTQ+ relevant<br>keywords<sup>#</sup>, from 1975 to 2021. Notice that this chart only includes movies which have been<br>rated by a minimum of 1,000 user votes in order to have a solid sample size for scoring."
txt_caption <- "*<sup>#</sup> relevancy defined as having assigned at least one of the following keywords: lgbt, gay, lesbian, transgender, bisexual, intersex, queer, genderqueer,<br>non-binary, gender, asexual; the movie 'Salò, or the 120 Days of Sodom' was not included in the analysis, as none of the TMDB keywords matched the<br>above keywords*
\n
**Source**: LGBTQ movies database, TMDB  \u2022  **Chart**: Amman Algenib"


# set font ---------------------------------------------------------------------

font_add_google("Asap", "asap")
ft1 <- "asap"

showtext_auto()
showtext_opts(dpi = 300)


# set colours ------------------------------------------------------------------

col_bg <- "white"
col_axis <- "grey30"
col_grid <- "grey80"
col_text_y_axis <- "grey60"
col_text_x_axis <- "grey30"
col_languages <- c("#073B4C", "#06D6A0", "#FFD166", "#118AB2", "#EF476F")


# define annotations ------------------------------------------------------------

grob_korean <- grobTree(
  textGrob("The Handmaiden", x = 0.75, y = 0.95, hjust = 1, vjust = 1, gp = gpar(family = ft1, fontface = "bold", fontsize = 13, col = "#118AB2")),
  textGrob("avg. score: 8.3\nvotes: 2,682", x = 0.75, y = 0.90, hjust = 1, vjust = 1, gp = gpar(family = ft1, fontface = "plain", fontsize = 13, col = "#118AB2"))
)

grob_thai <- grobTree(
  textGrob("Ong Bak:\nMuay Thai Warrior", x = 0.55, y = 0.35, hjust = 1, vjust = 1, gp = gpar(family = ft1, fontface = "bold", fontsize = 13, col = "#EF476F")),
  textGrob("avg. score: 7,1\nvotes: 1,345", x = 0.55, y = 0.25, hjust = 1, vjust = 1, gp = gpar(family = ft1, fontface = "plain", fontsize = 13, col = "#EF476F"))
)


# plot chart -------------------------------------------------------------------

ggplot(movie_votes, aes(release_date, vote_average, colour = original_language, size = vote_count)) +
  geom_point() +
  
  scale_x_date(date_labels = "%Y", limits = as.Date(c('1975-01-01','2022-01-01')), breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), by = "5 years")) +
  scale_y_continuous(limits = c(5.5, 8.5), breaks = seq(5.5, 8.5, 0.5), expand = c(0, 0)) +
  scale_size_continuous(range = c(2, 10), breaks = c(1000, 5000, 10000),
                        labels = c("1k", "5k", "10k")) +
  scale_colour_manual(values = col_languages) +
  
  labs(
    title = txt_title,
    subtitle = txt_subtitle,
    caption = txt_caption,
    colour = "The movie's original language",
    size = "Number of votes"
  ) +
  
  theme_minimal() +
  theme(
    
    plot.background = element_rect(colour = col_bg),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),

    plot.title = element_markdown(size = 26, family = ft1, face = "bold", margin = margin(b = 0.5, unit = "cm"), lineheight = 1.2),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 15, family = ft1, margin = margin(b = 0.4, unit = "cm"), lineheight = 1.3),
    plot.caption = element_markdown(hjust = 0, size = 10, family = ft1, margin = margin(t = 0.8, unit = "cm"), colour = col_text_x_axis, lineheight = 1),
    plot.caption.position = "plot",

    
    axis.line.x = element_line(colour = col_axis, linewidth = 0.5),
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.title = element_blank(),
    axis.text.x = element_text(family = ft1, size = 13, colour = col_text_x_axis, margin = margin(t = 10, b = 0, r = 0, l = 0)),
    axis.text.y = element_text(family = ft1, size = 13, colour = col_text_y_axis),
    
    panel.grid.major.y = element_line(colour = col_grid, linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.location = "plot",
    legend.position = "top",
    legend.justification.top = "left",
    legend.margin = margin(l = 0, t = 0.2, b = 0.5, r = 1, unit = "cm"),
    legend.direction = "horizontal",
    
    legend.title.position = "top",
    legend.title = element_text(family = ft1, size = 14),
    legend.text = element_text(family = ft1, size = 14),
    legend.key.size = unit(0.5, "cm"),
    legend.key.height = unit(1, "cm")
  ) +
  
  guides(
    colour = guide_legend(order = 1, nrow = 1, override.aes = list(size = 5)),
    size = guide_legend(order = 2, override.aes = list(colour = "grey70"))
    ) +
  
  # korean film
  annotate(geom = "segment", x = as.Date("2012-04-01"), y = 8.3, xend = as.Date("2015-06-01"), yend = 8.3, colour = "#118AB2") +
  annotation_custom(grob_korean) +

  
  # thai film
  annotate(geom = "segment", x = as.Date("2002-10-01"), y = 7.03, xend = as.Date("2001-01-01"), yend = 6.6, colour = "#EF476F") +
  annotation_custom(grob_thai)


# save chart -------------------------------------------------------------------

ggsave("tt2024w26.png", path = my_dir, height = 10, width = 10)
  
