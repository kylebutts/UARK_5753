# https://people.duke.edu/~rnau/411home.htm
# %%
library(tidyverse)
library(here)
library(fixest)

# https://people.duke.edu/~rnau/Slides_on_forecasting_with_inflation_seasonal_adjustment_and_Winters_model--Robert%20Nau.pdf
df <- here("05-Moving_Averages/data/FRED/GDP.csv") |>
  read_csv(show_col_types = FALSE) |>
  select(date = DATE, gdp = GDP) |>
  dplyr::filter(year(date) >= 2000)

# %%
(plot_raw <- ggplot(df) +
  geom_line(
    aes(x = date, y = gdp),
    color = kfbmisc::tailwind_color("zinc-600"),
    linewidth = 1
  ) +
  labs(x = NULL, y = "U.S. GDP (\\$ Billions)") +
  kfbmisc::theme_kyle(base_size = 12))

est_linear_trend <- feols(
  gdp ~ date,
  data = df,
  vcov = "hc1"
)
df$gdp_hat <- predict(est_linear_trend)

(plot_linear_trend <-
  plot_raw +
  geom_line(
    aes(x = date, y = gdp_hat),
    data = df,
    color = kfbmisc::kyle_color("blue"),
    linewidth = 1
  ))

# %%
predictions <- predict(est_linear_trend, se.fit = TRUE)
head(predictions)

# The next 7 days from our sample
prediction_df <- data.frame(
  date = ymd("2021-06-30") + 1:5
)
predictions <- predict(est_linear_trend, newdata = prediction_df, se.fit = TRUE)
predictions$ci_lower <- predictions$fit - 1.96 * predictions$se.fit
predictions$ci_upper <- predictions$fit + 1.96 * predictions$se.fit

# %%
library(tsibble)
df$month = yearmonth(df$date)
df$quarter = yearquarter(df$date)
# Does not change the predictions, just the interpretation of coefficients
# This is not exactly true since months have varying number of days
feols(
  gdp ~ month,
  data = df,
  vcov = "hc1"
)
feols(
  gdp ~ quarter,
  data = df,
  vcov = "hc1"
)

# %%
kfbmisc::tikzsave(
  here::here("Slides/06_Time_Series_Regression/figures/gdp_raw.pdf"),
  plot_raw,
  width = 8,
  height = 3.8
)
kfbmisc::tikzsave(
  here::here("Slides/06_Time_Series_Regression/figures/gdp_linear_trend.pdf"),
  plot_linear_trend,
  width = 8,
  height = 3.8
)
