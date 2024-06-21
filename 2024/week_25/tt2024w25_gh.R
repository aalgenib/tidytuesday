library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)

# set working directory --------------------------------------------------------

my_dir <- "~/R/tidytuesdays/2024/2024w25"


# setting fonts ----------------------------------------------------------------

font_add_google("Roboto", "roboto")
ft1 <- "roboto"

showtext_auto()
showtext_opts(dpi = 300)


# setting text -----------------------------------------------------------------

txt_title <- "**Filling Holiday Gaps**"
txt_subtitle <- "An overview of <span style = 'color:#679436'>**existing**</span> and <span style = 'color:#EB6424'>**proposed**</span> federal holidays in the USA.<br>Some of the proposed federal holidays would fill the large gap<br>between President's Day and Memorial Day. The range of<br>non-fixed holidays is indicated in lighter colors."
txt_caption <- "**Data:** Wikipedia | **Visualization:** Amman Algenib"


# setting colours --------------------------------------------------------------

col_months <- rep(c("grey70", "grey90"), times = 6)
col_bg <- "white"
col_txt <- "#0a1128"
col_existing <- "#679436"
col_proposed <- "#EB6424"


# create year circle -----------------------------------------------------------

## set number of points to days in a year
n <- 365

## set theta for the points
theta <- seq(0, 2 * pi, length.out = n + 1)[-1]

## set circle radius
r <- 4

## set x and y coordinates
x <- r * sin(theta)
y <- r * cos(theta)

## create tibble with circle coordinates
circle_data <- tibble(x = x, y = y) 



# construct tibble with month and day data -------------------------------------

## add month and day information
month_levels <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

month_names <- c(rep("January", times = 31), rep("February", times = 28), rep("March", times = 31), rep("April", times = 30), rep("May", times = 31), rep("June", times = 30), rep("July", times = 31), rep("August", times = 31), rep("September", times = 30), rep("October", times = 31), rep("November", times = 30), rep("December", times = 31))

day_numbers <- c(c(1:31), c(1:28), c(1:31), c(1:30), c(1:31), c(1:30), c(1:31), c(1:31), c(1:30), c(1:31), c(1:30), c(1:31))

circle_data_md <- circle_data |> 
  mutate(
    month = factor(month_names, levels = month_levels),
    day = day_numbers
    )


# construct federal holidays tibble --------------------------------------------

## get data
tuesdata <- tidytuesdayR::tt_load(2024, week = 25)

federal_holidays <- tuesdata$federal_holidays
proposed_federal_holidays <- tuesdata$proposed_federal_holidays

## data wrangling
federal_holidays_mod <- federal_holidays |> 
  select(date, official_name) |>
  mutate(category = "existing") |> 
  separate_wider_delim(
    date,
    delim = " ",
    names = c("month", "day")
  ) |> 
  separate_wider_delim(
    day,
    delim = "\U2013",
    names = c("day", "day_max"),
    too_few = "align_start"
  )

proposed_federal_holidays_mod <- proposed_federal_holidays |> 
  select(date, official_name) |>
  mutate(category = "proposed") |> 
  separate_wider_delim(
    date,
    delim = " ",
    names = c("month", "day")
  ) |> 
  separate_wider_delim(
    day,
    delim = "\U2013",
    names = c("day", "day_max"),
    too_few = "align_start"
  )


tib_fed_hol <- bind_rows(federal_holidays_mod, proposed_federal_holidays_mod) |> 
  mutate(definition = "fixed") |> 
  mutate(day = as.numeric(day))

## adding the range for the non-fixed holidays
add_range <- function (df, month, day, day_max, official_name, category) {
    range_rows <- tibble(
        month = month, 
        day = day:day_max, 
        day_max = NA, 
        official_name = official_name, 
        category = category, 
        definition = "range"
        )
    df <- bind_rows(df, range_rows)
    return(df)
    }

tib_fed_hol <- tib_fed_hol |> 
  add_range("January", 16, 21, "Birthday of Martin Luther King, Jr.", "existing") |> 
  add_range("February", 16, 21, "Washington's Birthday", "existing") |> 
  add_range("May", 26, 31, "Memorial Day", "existing") |>
  add_range("September", 2, 7, "Labor Day", "existing") |>
  add_range("October", 9, 14, "Columbus Day", "existing") |>
  add_range("November", 23, 28, "Thanksgiving Day", "existing") |> 
  add_range("February", 16, 21, "Susan B. Anthony Day", "proposed") |> 
  add_range("March", 26, 31, "Cesar Chavez Day", "proposed") |> 
  add_range("May", 16, 21, "Malcolm X Day", "proposed") |> 
  add_range("September", 16, 21, "Native Americans' Day", "proposed") |> 
  add_range("November", 2, 8, "Election Day / Democracy Day", "proposed")


## create four separate tibbles for the plot -----------------------------------

fed_hol_exist_fixed <- tib_fed_hol |> 
  filter(
    category == "existing",
    definition == "fixed"
  ) |> 
  select(!day_max)

fed_hol_exist_fixed <- left_join(
  circle_data_md,
  fed_hol_exist_fixed,
  by = c("month", "day")
) |> 
  drop_na()
  

fed_hol_exist_range <- tib_fed_hol |> 
  filter(
    category == "existing",
    definition == "range"
  ) |> 
  select(!day_max)

fed_hol_exist_range <- left_join(
  circle_data_md,
  fed_hol_exist_range,
  by = c("month", "day")
) |> 
  drop_na()


fed_hol_prop_fixed <- tib_fed_hol |> 
  filter(
    category == "proposed",
    definition == "fixed"
  ) |> 
  select(!day_max)

fed_hol_prop_fixed <- left_join(
  circle_data_md,
  fed_hol_prop_fixed,
  by = c("month", "day")
) |> 
  drop_na()



fed_hol_prop_range <- tib_fed_hol |> 
  filter(
    category == "proposed",
    definition == "range"
  ) |> 
  select(!day_max)

fed_hol_prop_range <- left_join(
  circle_data_md,
  fed_hol_prop_range,
  by = c("month", "day")
) |> 
  drop_na()


# create the plot --------------------------------------------------------------

## set parameters for text annotations
e <- 1.2
f <- 4.2
g <- 3.2
h <- 3.0
i <- 15
j <- 42
k <- 72


## plotting
ggplot() +
  geom_point(data = circle_data_md, aes(x, y, colour = month), show.legend = FALSE) +
  geom_point(data = fed_hol_prop_range, aes(x, y), color = col_proposed, alpha = 0.4) +
  geom_point(data = fed_hol_exist_range, aes(x, y), color = col_existing, alpha = 0.4) +
  geom_point(data = fed_hol_prop_fixed, aes(x, y), color = col_proposed, size = 4) +
  geom_point(data = fed_hol_exist_fixed, aes(x, y), color = col_existing, size = 4) +

  # existing federal holidays
  annotate("text", x = 0, y = 0, label = "New Year's Day", hjust = -1.85, angle = 89, family = ft1, size = 3.5, colour = col_existing) +
  annotate("text", x = 0, y = 0, label = "Birthday of Martin Luther King, Jr.", hjust = -.32, angle = 75, family = ft1, size = 3.5, colour = col_existing) +
  annotate("text", x = 0, y = 0, label = "Presidents Day", hjust = -1.9, angle = 46, family = ft1, size = 3.5, colour = col_existing) +
  annotate("text", x = 0, y = 0, label = "Memorial Day", hjust = -2.2, angle = -53, family = ft1, size = 3.5, colour = col_existing) +
  annotate("text", x = 0, y = 0, label = "Juneteenth National Independence Day", hjust = -0.13, angle = -78, family = ft1, size = 3.5, colour = col_existing) +
  annotate("text", x = 0, y = 0, label = "Independence Day", hjust = 2.4, angle = 87, family = ft1, size = 3.5, colour = col_existing) +
  annotate("text", x = 0, y = 0, label = "Labor Day", hjust = 4.4, angle = 29, family = ft1, size = 3.5, colour = col_existing) +
  annotate("text", x = 0, y = 0, label = "Columbus Day", hjust = 3.05, angle = -7, family = ft1, size = 3.5, colour = col_existing) +
  annotate("text", x = 0, y = 0, label = "Veterans Day", hjust = 3.35, angle = -41, family = ft1, size = 3.45, colour = col_existing) +
  annotate("text", x = 0, y = 0, label = "Thanksgiving Day", hjust = 2.5, angle = -52, family = ft1, size = 3.5, colour = col_existing) +
  annotate("text", x = 0, y = 0, label = "Christmas Day", hjust = 3.05, angle = -84, family = ft1, size = 3.5, colour = col_existing) +
  
  # proposed federal holidays
  annotate("text", x = 0, y = 0, label = "Susan B. Anthony Day", hjust = -1, angle = 42, family = ft1, size = 3.5, colour = col_proposed) +
  annotate("text", x = 0, y = 0, label = "Harriet Tubman Day", hjust = -1.2, angle = 22, family = ft1, size = 3.5, colour = col_proposed) +
  annotate("text", x = 0, y = 0, label = "Cesar Chavez Day", hjust = -1.45, angle = 7, family = ft1, size = 3.5, colour = col_proposed) +
  annotate("text", x = 0, y = 0, label = "Malcolm X Day", hjust = -1.95, angle = -43, family = ft1, size = 3.5, colour = col_proposed) +
  annotate("text", x = 0, y = 0, label = "Flag Day", hjust = -4.2, angle = -73, family = ft1, size = 3.5, colour = col_proposed) +
  annotate("text", x = 0, y = 0, label = "September 11 Day of Remembrance", hjust = 1.22, angle = 20, family = ft1, size = 3.5, colour = col_proposed) +
  annotate("text", x = 0, y = 0, label = "Native Americans' Day", hjust = 1.95, angle = 15, family = ft1, size = 3.5, colour = col_proposed) +
  annotate("text", x = 0, y = 0, label = "Election Day / Democracy Day", hjust = 1.48, angle = -32, family = ft1, size = 3.5, colour = col_proposed) +
  annotate("text", x = 0, y = 0, label = "Rosa Parks Day", hjust = 2.85, angle = -61, family = ft1, size = 3.5, colour = col_proposed) +
  
  # months
  annotate("text", x = e, y = f, label = "Jan", hjust = 0.5, angle = -i, family = ft1, size = 3.5, colour = col_txt) +
  annotate("text", x = g, y = h, label = "Feb", hjust = 0.5, angle = -j, family = ft1, size = 3.5, colour = col_txt) +
  annotate("text", x = f, y = e, label = "Mar", hjust = 0.5, angle = -k, family = ft1, size = 3.5, colour = col_txt) +
  
  annotate("text", x = f, y = -e, label = "Apr", hjust = 0.5, angle = k, family = ft1, size = 3.5, colour = col_txt) +
  annotate("text", x = g, y = -h, label = "May", hjust = 0.5, angle = j, family = ft1, size = 3.5, colour = col_txt) +
  annotate("text", x = e, y = -f, label = "Jun", hjust = 0.5, angle = i, family = ft1, size = 3.5, colour = col_txt) +
  
  annotate("text", x = -e, y = -f, label = "Jul", hjust = 0.5, angle = -i, family = ft1, size = 3.5, colour = col_txt) +
  annotate("text", x = -g, y = -h, label = "Aug", hjust = 0.5, angle = -j, family = ft1, size = 3.5, colour = col_txt) +
  annotate("text", x = -f, y = -e, label = "Sep", hjust = 0.5, angle = -k, family = ft1, size = 3.5, colour = col_txt) +
  
  annotate("text", x = -f, y = e, label = "Oct", hjust = 0.5, angle = k, family = ft1, size = 3.5, colour = col_txt) +
  annotate("text", x = -g, y = h, label = "Nov", hjust = 0.5, angle = j, family = ft1, size = 3.5, colour = col_txt) +
  annotate("text", x = -e, y = f, label = "Dec", hjust = 0.5, angle = i, family = ft1, size = 3.5, colour = col_txt) +
  
  scale_colour_manual(values = col_months) +
  
  labs(
    title = txt_title,
    subtitle = txt_subtitle,
    caption = txt_caption
  ) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = col_bg),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_markdown(hjust = 0.5, family = ft1, size = 25, colour = col_txt, margin = margin(b = 0.8, t = 0.5, unit = "cm")),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft1, size = 12, colour = col_txt, margin = margin(b = 1.5, unit = "cm"), lineheight = 1.3),
    plot.caption = element_markdown(hjust = 1, vjust = -1, family = ft1, size = 7, colour = col_txt, margin = margin(t = 1, unit = "cm")),
  ) +
  coord_fixed()


# save image -------------------------------------------------------------------

ggsave("tt2024w25.png", path = my_dir, height = 10, width = 8)

