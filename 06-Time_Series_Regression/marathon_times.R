# %%
# https://people.duke.edu/~rnau/411home.htm
library(tidyverse)
library(here)
library(fixest)
library(fpp3)

data(boston_marathon, package = "fpp3")
df <- boston_marathon |>
  janitor::clean_names() |>
  filter(event == "Men's open division") |>
  filter(year >= 1924) |>
  mutate(
    minutes = as.numeric(time) / 60
  )

# %%
est_trend <- feols(
  minutes ~ year,
  data = df,
  vcov = NW() ~ year
)
df$minutes_hat <- predict(est_trend)

df$minutes_minus_linear_trend <-
  df$minutes - df$minutes_hat

df$trend_1 <- df$year
df$trend_2 <- (df$year - 1950) * (df$year > 1950)
df$trend_3 <- (df$year - 1980) * (df$year > 1980)

est_piecewise_trends <- feols(
  minutes ~ trend_1 + trend_2 + trend_3,
  data = df,
  vcov = NW() ~ year
)
df$minutes_hat_piecewise_trends <- predict(est_piecewise_trends)

# %%
df$trend_1_uncentered <- df$year
df$trend_2_uncentered <- df$year * (df$year > 1950)
df$trend_3_uncentered <- df$year * (df$year > 1980)
df$minutes_hat_uncentered <- predict(feols(
  minutes ~ trend_1_uncentered + trend_2_uncentered + trend_3_uncentered,
  data = df,
  vcov = NW() ~ year
))
df$epoch <- case_when(
  df$year > 1980 ~ 3,
  df$year <= 1980 & df$year > 1950 ~ 2,
  TRUE ~ 1
)


# %%
(plot_raw <- ggplot(df) +
  geom_line(
    aes(x = year, y = minutes),
    color = kfbmisc::tailwind_color("zinc-400"),
    linewidth = 1
  ) +
  labs(x = NULL, y = "Boston Marathon Finishing Time (Minutes)") +
  kfbmisc::theme_kyle(base_size = 12))

(plot_time_trend <-
  ggplot(df) +
  geom_line(
    aes(x = year, y = minutes, color = "A"),
    linewidth = 1
  ) +
  geom_line(
    aes(x = year, y = minutes_hat, color = "B"),
    data = df,
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-400"),
      "B" = kfbmisc::kyle_color("blue")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Linear Trend"
    )
  ) +
  labs(
    x = NULL,
    y = "Boston Marathon Finishing Time (Minutes)",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(plot_minutes_minus_linear_trend <-
  ggplot() +
  geom_line(
    aes(x = year, y = minutes_minus_linear_trend),
    data = df,
    color = kfbmisc::tailwind_color("zinc-400"),
    linewidth = 1
  ) +
  scale_y_continuous(limits = c(-20, 20)) +
  labs(
    x = NULL,
    y = "Residuals: $y_t - \\hat{\\alpha} - \\hat{\\lambda} t$",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 12))

# %%
(plot_piecewise_trends <-
  ggplot(df) +
  geom_line(
    aes(x = year, y = minutes, color = "A"),
    linewidth = 1
  ) +
  geom_line(
    aes(x = year, y = minutes_hat, color = "B"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = year, y = minutes_hat_piecewise_trends, color = "C"),
    data = df,
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-400"),
      "B" = kfbmisc::kyle_color("blue"),
      "C" = kfbmisc::kyle_color("rose")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Linear Trend",
      "C" = "Piecewise Linear"
    )
  ) +
  labs(
    x = NULL,
    y = "Boston Marathon Finishing Time (Minutes)",
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
(plot_piecewise_trends_uncentered <-
  ggplot(df) +
  geom_line(
    aes(x = year, y = minutes, color = "A"),
    linewidth = 1
  ) +
  geom_line(
    aes(x = year, y = minutes_hat, color = "B"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = year, y = minutes_hat_piecewise_trends, color = "C"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = year, y = minutes_hat_uncentered, group = epoch, color = "D"),
    data = df,
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-400"),
      "B" = kfbmisc::kyle_color("blue"),
      "C" = kfbmisc::kyle_color("rose"),
      "D" = kfbmisc::kyle_color("green")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Linear Trend",
      "C" = "Piecewise Linear",
      "D" = "Piecewise Linear (without centering)"
    )
  ) +
  labs(
    x = NULL,
    y = "Boston Marathon Finishing Time (Minutes)",
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
  here::here("06-Time_Series_Regression/figures/marathon_raw.pdf"),
  plot_raw,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here::here("06-Time_Series_Regression/figures/marathon_linear_trend.pdf"),
  plot_time_trend,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here::here("06-Time_Series_Regression/figures/marathon_piecewise_linear.pdf"),
  plot_piecewise_trends,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here::here(
    "06-Time_Series_Regression/figures/marathon_piecewise_linear_without_centering.pdf"
  ),
  plot_piecewise_trends_uncentered,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here::here(
    "06-Time_Series_Regression/figures/marathon_minutes_minus_linear_trend.pdf"
  ),
  plot_minutes_minus_linear_trend,
  width = 8,
  height = 4.2
)
