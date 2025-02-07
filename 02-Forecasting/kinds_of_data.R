library(tidyverse)
library(tinytable)
library(janitor)

## Cross-sectional ----
# NYC schools average SAT scores and demographic information
nyc_sat <- here("02-Forecasting/data/nyc_sat.csv") |>
  read_csv(show_col_types = FALSE) |>
  janitor::clean_names()

tab_nyc_sat <- nyc_sat |>
  select(school_id, avg_sat_math = average_score_sat_math, pct_white = percent_white, pct_black = percent_black) |>
  # select(`School ID`, Borough, `Average Score (SAT Math)`, `Percent White`) |>
  _[3:9, ] |>
  tt() |>
  theme_tt("striped") |>
  format_tt(escape = TRUE) |>
  format_tt(i = 4, fn = \(x) "$\\vdots$") |>
  style_tt(tabularray_inner = "colsep = {0.5em}")

cat("\n\n## Cross-sectional ----\n\n")
print(tab_nyc_sat, "latex")
tab_nyc_sat |>
  save_tt(here("02-Forecasting/tables/data_types_ex_cs.tex"), overwrite = TRUE)

## Time-series ----
# Hourly usage of a bike sharing program in Washington, DC.
ht <- function(data) {
  rbind(head(data, n = 4), tail(data, n = 3))
}

data(Bikeshare, package = "ISLR2")
head(Bikeshare)

Bikeshare <- Bikeshare |>
  mutate(
    date = lubridate::as_date("2018-12-31") + day,
    month = month(date),
    day = day(date),
    hour = hr
  ) |>
  select(month, day, hour, holiday, weathersit, temp, atemp, hum, windspeed, casual, registered, bikers)

tab_bikeshare <- Bikeshare |>
  select(month, day, hour, bikers, temp) |>
  ht() |>
  tt() |>
  theme_tt("striped") |>
  format_tt(i = 4, fn = \(x) "$\\vdots$") |>
  style_tt(tabularray_inner = "colsep = {0.5em}")

cat("\n\n## Time-series ----\n\n")
print(tab_bikeshare, "latex")
tab_bikeshare |>
  save_tt(here("02-Forecasting/tables/data_types_ex_ts.tex"), overwrite = TRUE)


## Panel ----
data(Fund, package = "ISLR2")
Fund <- Fund |>
  as_tibble() |>
  mutate(month = cur_group_rows(), .before = 1) |>
  pivot_longer(
    cols = starts_with("Manager"),
    names_prefix = "Manager",
    names_to = "fund_manager",
    names_transform = as.numeric,
    values_to = "return"
  ) |>
  select(fund_manager, month, return) |>
  arrange(fund_manager, month)

tab_fund <- Fund |>
  ht() |>
  tt() |>
  theme_tt("striped") |>
  format_tt(
    j = "return", fn = scales::label_percent(scale = 1, suffix = "\\%")
  ) |>
  format_tt(i = 4, fn = \(x) "$\\vdots$") |>
  format_tt(i = 0, escape = TRUE) |>
  style_tt(tabularray_inner = "colsep = {0.5em}")

cat("\n\n## Panel ----\n\n")
print(tab_fund, "latex")
tab_fund |>
  save_tt(here("02-Forecasting/tables/data_types_ex_panel.tex"), overwrite = TRUE)
