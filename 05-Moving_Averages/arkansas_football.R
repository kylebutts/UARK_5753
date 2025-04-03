# %%
library(tidyverse)
library(here)

football <- tibble(
  date = c(
    "11-11-2023",
    "11-04-2023",
    "09-23-2023",
    "09-02-2023",
    "10-07-2023",
    "09-16-2023",
    "09-09-2023",
    "10-21-2023",
    "11-24-2023",
    "10-14-2023",
    "11-18-2023",
    "09-30-2023"
  ),
  school = rep("Arkansas", 12L),
  opponent = c(
    "Auburn",
    "Florida",
    "(12) LSU",
    "Western Carolina",
    "(16) Ole Miss",
    "BYU",
    "Kent State",
    "Mississippi State",
    "(10) Missouri",
    "(11) Alabama",
    "Florida International",
    "Texas A&M"
  ),
  result = c("L", "W", "L", "W", "L", "L", "W", "L", "L", "L", "W", "L"),
  pts = c(10, 39, 31, 56, 20, 31, 28, 3, 14, 21, 44, 22),
  pts_opponent = c(48, 36, 34, 13, 27, 38, 6, 7, 48, 24, 20, 34)
)
football$date <- football$date |>
  parse_date_time(orders = "%m-%d-%Y") |>
  as_date()


# %%
(p_football <- ggplot() +
  geom_line(
    aes(x = date, y = pts_opponent, color = "Opponent"),
    data = football,
    linewidth = 1.25
  ) +
  geom_point(
    aes(x = date, y = pts_opponent, color = "Opponent"),
    data = football
  ) +
  geom_line(
    aes(x = date, y = pts, color = "Arkansas"),
    data = football,
    linewidth = 1.25
  ) +
  geom_point(
    aes(x = date, y = pts, color = "Arkansas"),
    data = football
  ) +
  scale_x_date() +
  scale_color_manual(
    values = c(
      "Arkansas" = "#9D2235",
      "Opponent" = kfbmisc::tailwind_color("zinc-500")
    )
  ) +
  labs(
    x = NULL,
    y = "Points Scored",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14, legend = "top"))

# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/arkansas_2023_football.pdf"),
  plot = p_football,
  width = 8,
  height = 4.5
)

# %%
