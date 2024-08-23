library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)

# working directory
my_dir <- "~/R/tidytuesdays/2024/2024w27"

# fonts
font_add_google("Roboto", "roboto")
ft1 <- "roboto"
showtext_opts(dpi = 300)

# raw data
tuesdata <- tidytuesdayR::tt_load('2024-07-02')
tt_urls <- tuesdata$tt_urls

# data wrangling
total_sources_per_year <- tt_urls %>% 
  filter(type == "source") %>% 
  count(year) %>% 
  rename(total = n)

top_domains <- tt_urls %>%
  filter(type == "source") %>% 
  count(domain, sort = TRUE)

rest_domains <- top_domains %>% 
  filter(n < 12) %>% 
  pull(domain)

domains_per_year <- tt_urls %>% 
  filter(type == "source") %>% 
  select(year, domain) %>% 
  mutate(
    domain_mod = ifelse(domain %in% rest_domains, "rest", domain),
    domain_mod = factor(domain_mod, levels = c("github", "kaggle", "data", "wikipedia", "ourworldindata", "rest"))) %>% 
  group_by(year) %>% 
  count(domain_mod, .drop = FALSE)

top_domains_yearly_fraction <- left_join(
  domains_per_year,
  total_sources_per_year,
  by = "year",
) %>% 
  mutate(percentage = round(n / total * 100, 2))

# colors
color_levels_b <- c("#23362b", "#1bb28c", "#e86a58", "#fed45b", "#9bc7c5", "#efeeea")

# plot
ggplot(
  top_domains_yearly_fraction,
  aes(x = year, y = percentage, fill = domain_mod)) +
    geom_area() +
  
  scale_fill_manual(values = color_levels_b, labels = c("GitHub", "Kaggle", "data.world", "Wikipedia", "Our World In Data", "all other sources")) +
  
  guides(fill = guide_legend(nrow = 1)) +
  
  annotate("richtext", x = 2021, y = 20, label = "Yearly proportions of the five most frequently used<br>sources for tidy tuesday datasets compared<br>to all other sources (2018--2024)", hjust = 0.5, family = ft1, size = 8, fill = NA, label.color = NA) +
  
  annotate("richtext", x = 2021, y = 10, label = "**Data**: ttmeta package | **Graphic**: Amman Algenib", hjust = 0.5, family = ft1, size = 3, fill = NA, label.color = NA) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.08),
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA, color = "grey", linewidth = 0.2),
    legend.key.size = unit(0.5, 'cm'),
    legend.text = element_text(family = ft1, size = 14),
  )

# image
ggsave("tt2024w27.png", path = my_dir, height = 10, width = 10)
