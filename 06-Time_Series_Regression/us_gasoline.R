# %%
library(tidyverse)
library(fpp3)
library(forecast)
library(prophet)
data(us_gasoline, package = "fpp3")
us_gasoline <- us_gasoline |>
  mutate(date = as_date(Week)) |>
  rename(barrels = Barrels)

us_gasoline <- us_gasoline |>
  filter(year(date) >= 2007)

# %%
(plot_raw <- ggplot() +
  geom_line(
    aes(x = date, y = barrels),
    data = us_gasoline,
    color = kfbmisc::tailwind_color("zinc-600"),
    linewidth = 1
  ) +
  labs(x = NULL, y = "U.S. Gasoline Production (Millions of Barrels)") +
  kfbmisc::theme_kyle(base_size = 12))

fourier_series <- function(dates, period, series.order) {
  t <- as.numeric(difftime(
    dates,
    as_date("1970-01-01 00:00:00"),
    units = "days"
  ))
  features <- matrix(0, length(t), 2 * series.order)
  for (i in 1:series.order) {
    x <- as.numeric(2 * i * pi * t / period)
    features[, i * 2 - 1] <- sin(x)
    features[, i * 2] <- cos(x)
  }
  return(features)
}

est_fourier_1 <- feols(
  barrels ~ fourier_series(date, period = 365, series.order = 1),
  us_gasoline
)
us_gasoline$barrels_hat_fourier_1 <- predict(est_fourier_1)

est_fourier_2 <- feols(
  barrels ~ fourier_series(date, period = 365, series.order = 2),
  us_gasoline
)
us_gasoline$barrels_hat_fourier_2 <- predict(est_fourier_2)

est_fourier_7 <- feols(
  barrels ~ fourier_series(date, period = 365, series.order = 7),
  us_gasoline
)
us_gasoline$barrels_hat_fourier_7 <- predict(est_fourier_7)

est_monthly <- feols(
  barrels ~ i(month(date)),
  us_gasoline
)
us_gasoline$barrels_hat_monthly <- predict(est_monthly)

est_weekly <- feols(
  barrels ~ i(week(date)),
  us_gasoline
)
us_gasoline$barrels_hat_weekly <- predict(est_weekly)


# %%
(plot_fourier <- ggplot() +
  geom_line(
    aes(x = date, y = barrels, color = "A"),
    data = us_gasoline,
    linewidth = 1
  ) +
  geom_line(
    aes(x = date, y = barrels_hat_fourier_1, color = "B"),
    data = us_gasoline,
    linewidth = 1
  ) +
  geom_line(
    aes(x = date, y = barrels_hat_fourier_7, color = "C"),
    data = us_gasoline,
    linewidth = 1
  ) +
  # geom_line(
  #   aes(x = date, y = barrels_hat_monthly, color = "D"),
  #   data = us_gasoline,
  #   linewidth = 1
  # ) +
  # geom_line(
  #   aes(x = date, y = barrels_hat_weekly, color = "E"),
  #   data = us_gasoline,
  #   linewidth = 1
  # ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("blue"),
      "D" = kfbmisc::kyle_color("green"),
      "E" = kfbmisc::kyle_color("yellow")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Fourier with $K = 1$",
      "C" = "Fourier with $K = 7$",
      "D" = "Month Effects",
      "E" = "Week Effects"
    )
  ) +
  labs(
    x = NULL,
    y = "U.S. Gasoline Production (Millions of Barrels)",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 12, legend = "top"))


(plot_indicators <- ggplot() +
  geom_line(
    aes(x = date, y = barrels, color = "A"),
    data = us_gasoline,
    linewidth = 1
  ) +
  # geom_line(
  #   aes(x = date, y = barrels_hat_fourier_1, color = "B"),
  #   data = us_gasoline,
  #   linewidth = 1
  # ) +
  # geom_line(
  #   aes(x = date, y = barrels_hat_fourier_7, color = "C"),
  #   data = us_gasoline,
  #   linewidth = 1
  # ) +
  geom_line(
    aes(x = date, y = barrels_hat_weekly, color = "E"),
    data = us_gasoline,
    linewidth = 1
  ) +
  geom_line(
    aes(x = date, y = barrels_hat_monthly, color = "D"),
    data = us_gasoline,
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("blue"),
      "D" = kfbmisc::kyle_color("green"),
      "E" = kfbmisc::kyle_color("yellow")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Fourier with $K = 1$",
      "C" = "Fourier with $K = 7$",
      "D" = "Month Effects",
      "E" = "Week Effects"
    )
  ) +
  labs(
    x = NULL,
    y = "U.S. Gasoline Production (Millions of Barrels)",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 12, legend = "top"))

# %%
kfbmisc::tikzsave(
  here::here("06-Time_Series_Regression/figures/us_gasoline_raw.pdf"),
  plot_raw,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here::here(
    "06-Time_Series_Regression/figures/us_gasoline_fourier.pdf"
  ),
  plot_fourier,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here::here(
    "06-Time_Series_Regression/figures/us_gasoline_indicators.pdf"
  ),
  plot_indicators,
  width = 8,
  height = 4.2
)
