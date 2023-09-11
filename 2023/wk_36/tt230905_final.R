library(tidyverse)

# load data
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

# Fetch information about US regions
df_regions <- tibble(state.abb, state.region)

# Modify data
states_mod <- states %>%
  group_by(state_abbreviation) %>%
  filter(year %in% c(1983, 2022) & sector == "Total") %>% 
  summarize(memb_diff = members[year == 2022] - members[year == 1983]) %>% 
  merge(df_regions, by.x = "state_abbreviation", by.y = "state.abb", all = TRUE) %>% 
  rename(state_abbr = state_abbreviation,
         state_region = state.region)

# "DC" is not defined in state.region, hence NA needs to be changed to "Northeast"
states_mod$state_region[states_mod$state_abbr == "DC"] <- "Northeast"

# Plot
ggplot(states_mod, aes(
  x = fct_reorder(state_abbr, -memb_diff),
  y = memb_diff,
  fill = state_region
)) +
  geom_hline(
    yintercept = c(500000, 250000, -250000, -500000),
    color = "#eeeeee"
  ) +
  geom_col() +
  geom_hline(
    yintercept = 0,
    color = "darkgrey"
  ) +
  labs(
    title = "How did the number of employed workers who are union members\nchange between 1983 and 2022 in the four US regions?",
    subtitle = "",
    x = "",
    y = "",
    caption = "Source: Union Membership and Coverage Database | Chart: Algenib",
    fill = "US region"
    ) +
  theme(
    # General settings
    plot.title = element_text(hjust = 0, vjust = 0, size = 16, face = "bold"),
    plot.margin = margin(20, 20, 20, 20),
    # Legend settings
    legend.position = c(.80, .80),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.direction = "vertical",
    # Panel settings
    panel.grid = element_blank(),
    panel.background = element_blank(),
    # Axes settings
    axis.line.y = element_line(colour = "darkgrey"),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 9),
    ) +
  scale_fill_manual(values = c("#d58850", "#91aed7", "#d0ae57", "#719b85")) +
  # annotate California
  annotate(
    geom = "curve",
    x = 7,
    y = 400000,
    xend = 1,
    yend = 350000,
    curvature = .1,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "text",
    x = 7.6,
    y = 400000,
    label = "California",
    size = 5,
    hjust = 0) +
  # annotate New York
  annotate(
    geom = "curve",
    x = 44,
    y = -350000,
    xend = 50,
    yend = -300000,
    curvature = .1,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "text",
    x = 43,
    y = -350000,
    label = "New York",
    size = 5,
    hjust = 1) +
  # annotate Pennsylvania
  annotate(
    geom = "curve",
    x = 44,
    y = -450000,
    xend = 51,
    yend = -400000,
    curvature = .1,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "text",
    x = 43,
    y = -450000,
    label = "Pennsylvania",
    size = 5,
    hjust = 1) 


  