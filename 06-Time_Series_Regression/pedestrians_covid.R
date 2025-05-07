# %%
library(tidyverse)
library(here)
library(fixest)
library(tsibble)

# source: https://facebook.github.io/prophet/docs/handling_shocks.html#case-study---pedestrian-activity

url <- 'https://raw.githubusercontent.com/facebook/prophet/main/examples/example_pedestrians_covid.csv'
df <- url |>
  read_csv(show_col_types = FALSE) |>
  select(date = ds, ppl = y)

# %%
df$month <- month(df$date, label = TRUE)
est_monthly_pattern <- feols(
  ppl ~ i(month),
  data = df,
  vcov = NW() ~ date
)
df$ppl_hat_monthly <- predict(est_monthly_pattern)

df$covid_period <-
  (df$date >= ymd("2020-03-21")) & (df$date < ymd("2020-10-27"))
df$post_covid_period <- (df$date >= ymd("2020-10-27"))

est_monthly_and_covid <- feols(
  ppl ~ i(month) + i(covid_period) + i(post_covid_period),
  data = df,
  vcov = NW() ~ date
)
df$ppl_hat_monthly_and_covid <- predict(est_monthly_and_covid)

# %%
(plot_raw <- ggplot() +
  geom_line(
    aes(x = date, y = ppl),
    data = df,
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  labs(x = NULL, y = "Number of Pedestrians") +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(plot_monthly <- ggplot() +
  geom_line(
    aes(x = date, y = ppl, color = "A"),
    data = df,
  ) +
  geom_line(
    aes(x = date, y = ppl_hat_monthly, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  # geom_line(
  #   aes(x = date, y = ppl_hat_monthly_and_covid, color = "C"),
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
      "C" = "Monthly Pattern + Covid Period Indicator",
      "D" = "?"
    ),
  ) +
  labs(
    x = NULL,
    y = "Number of Pedesterians",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 12) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(plot_monthly_and_covid <- ggplot() +
  geom_line(
    aes(x = date, y = ppl, color = "A"),
    data = df,
  ) +
  geom_line(
    aes(x = date, y = ppl_hat_monthly, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  geom_line(
    aes(x = date, y = ppl_hat_monthly_and_covid, color = "C"),
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
      "C" = "Monthly Pattern + Covid Period Indicator",
      "D" = "?"
    ),
  ) +
  labs(
    x = NULL,
    y = "Number of Pedesterians",
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
  here("06-Time_Series_Regression/figures/melbourne_pedestrians_raw.pdf"),
  plot = plot_raw,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here("06-Time_Series_Regression/figures/melbourne_pedestrians_monthly.pdf"),
  plot = plot_monthly,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here(
    "06-Time_Series_Regression/figures/melbourne_pedestrians_monthly_and_covid.pdf"
  ),
  plot = plot_monthly_and_covid,
  width = 8,
  height = 4.2
)
