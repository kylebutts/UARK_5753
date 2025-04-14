# %%
library(tidyverse)
library(here)
library(fixest)
library(tsibble)
url <- "https://raw.githubusercontent.com/facebook/prophet/main/examples/example_wp_log_peyton_manning.csv"
df <- url |>
  read_csv(show_col_types = FALSE) |>
  select(date = ds, views = y)

# %%
df$month <- month(df$date, label = TRUE)
est_monthly_pattern <- feols(
  views ~ i(month),
  data = df,
  vcov = "hc1"
)
df$views_hat_monthly <- predict(est_monthly_pattern)

df$yearmonth <- yearmonth(df$date)
est_yearmonth_pattern <- feols(
  views ~ i(yearmonth),
  data = df,
  vcov = "hc1"
)
df$views_hat_yearmonth <- predict(est_yearmonth_pattern)

df$week <- week(df$date)
est_weekly_pattern <- feols(
  views ~ i(week),
  data = df,
  vcov = "hc1"
)
df$views_hat_weekly <- predict(est_weekly_pattern)

# Season starts in September
df$season <- year(df$date %m+% months(-9))
est_monthly_and_season_fe <- feols(
  views ~ i(month) + i(season),
  data = df,
  vcov = "hc1"
)
df$views_hat_monthly_and_season_fe <- predict(est_monthly_and_season_fe)

df$football_gameday <- (month(df$date) %in% c(9, 10, 11, 12, 1)) &
  (wday(df$date) %in% c(1, 2, 5))

est_monthly_and_gameday <- feols(
  views ~ i(month) + football_gameday,
  data = df,
  vcov = "hc1"
)
df$views_hat_monthly_and_gameday <-
  predict(est_monthly_and_gameday)


# %%
(plot_raw <- ggplot() +
  geom_line(
    aes(x = date, y = views),
    data = df,
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  labs(x = NULL, y = "Peyton Manning's Wikipedia Views") +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(plot_monthly <- ggplot() +
  geom_line(
    aes(x = date, y = views, color = "A"),
    data = df,
  ) +
  geom_line(
    aes(x = date, y = views_hat_monthly, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  # geom_line(
  #   aes(x = date, y = views_hat_weekly, color = "C"),
  #   data = df,
  #   linewidth = 1.2
  # ) +
  # geom_line(
  #   aes(x = date, y = views_hat_monthly_and_season_fe, color = "D"),
  #   data = df,
  #   linewidth = 1.2
  # ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-400"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("green"),
      "D" = kfbmisc::kyle_color("yellow")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Monthly Pattern",
      "C" = "Weekly",
      "D" = "Monthly Pattern + Season Shocks"
    ),
  ) +
  labs(
    x = NULL,
    y = "Peyton Manning's Wikipedia Views",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(plot_year_by_month <-
  ggplot() +
  geom_line(
    aes(x = date, y = views, color = "A"),
    data = df,
  ) +
  geom_line(
    aes(x = date, y = views_hat_monthly, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  geom_line(
    aes(x = date, y = views_hat_yearmonth, color = "C"),
    data = df,
    linewidth = 1.2
  ) +
  # geom_line(
  #   aes(x = date, y = views_hat_monthly_and_season_fe, color = "D"),
  #   data = df,
  #   linewidth = 1.2
  # ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-400"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("green"),
      "D" = kfbmisc::kyle_color("yellow")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Monthly Pattern",
      "C" = "Year-month",
      "D" = "Monthly Pattern + Season Shocks"
    ),
  ) +
  labs(
    x = NULL,
    y = "Peyton Manning's Wikipedia Views",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(plot_season_effects <-
  ggplot() +
  geom_line(
    aes(x = date, y = views, color = "A"),
    data = df,
  ) +
  geom_line(
    aes(x = date, y = views_hat_monthly, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  # geom_line(
  #   aes(x = date, y = views_hat_weekly, color = "C"),
  #   data = df,
  #   linewidth = 1.2
  # ) +
  geom_line(
    aes(x = date, y = views_hat_monthly_and_season_fe, color = "D"),
    data = df,
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-400"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("green"),
      "D" = kfbmisc::kyle_color("yellow")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Monthly Pattern",
      "C" = "Year-month",
      "D" = "Monthly Pattern + Season Shocks"
    ),
  ) +
  labs(
    x = NULL,
    y = "Peyton Manning's Wikipedia Views",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

# %%
kfbmisc::tikzsave(
  here("06-Time_Series_Regression/figures/peyton_raw.pdf"),
  plot = plot_raw,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here("06-Time_Series_Regression/figures/peyton_monthly.pdf"),
  plot = plot_monthly,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here("06-Time_Series_Regression/figures/peyton_year_by_month.pdf"),
  plot = plot_year_by_month,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here("06-Time_Series_Regression/figures/peyton_season_effects.pdf"),
  plot = plot_season_effects,
  width = 8,
  height = 4.2
)
