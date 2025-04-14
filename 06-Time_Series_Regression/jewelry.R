# https://people.duke.edu/~rnau/411home.htm
# %%
library(tidyverse)
library(here)
library(fixest)

# https://people.duke.edu/~rnau/Slides_on_forecasting_with_inflation_seasonal_adjustment_and_Winters_model--Robert%20Nau.pdf
df <- here("06-Time_Series_Regression/data/jewelry_retail_sales.csv") |>
  read_csv(show_col_types = FALSE) |>
  select(date = DATE, sales = MRTSMPCSM44831USN) |>
  dplyr::filter(year(date) >= 2012)

# %%
(plot_raw <- ggplot(df) +
  geom_line(
    aes(x = date, y = sales),
    color = kfbmisc::tailwind_color("zinc-600"),
    linewidth = 1
  ) +
  labs(x = NULL, y = "US Jewelry Sales (Millions \\$)") +
  kfbmisc::theme_kyle(base_size = 12))

df$month = month(df$date, label = TRUE)
est_monthly_pattern <- feols(
  sales ~ i(month),
  data = df,
  vcov = "hc1"
)
df$sales_hat <- predict(est_monthly_pattern)

(plot_monthly_pattern <-
  plot_raw +
  geom_line(
    aes(x = date, y = sales_hat),
    data = df,
    color = kfbmisc::kyle_color("blue"),
    linewidth = 1
  ))

# %%
kfbmisc::tikzsave(
  here::here("06-Time_Series_Regression/figures/jewelry_sales_raw.pdf"),
  plot_raw,
  width = 8,
  height = 4.2
)
kfbmisc::tikzsave(
  here::here(
    "06-Time_Series_Regression/figures/jewelry_sales_monthly_pattern.pdf"
  ),
  plot_monthly_pattern,
  width = 8,
  height = 4.2
)
