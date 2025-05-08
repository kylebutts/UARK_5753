# %%
library(tidyverse)
library(fixest)
library(here)
library(fpp3)

college <- here(
  "Projects/Cross_Sectional/data/college_scorecard/college_scorecard.csv"
) |>
  read_csv() |>
  mutate(
    mean_earnings_10yr_after = parse_number(mean_earnings_10yr_after),
    share_low_income = parse_number(share_low_income),
    private_for_profit = ownership == "Private for-profit"
  )

data(boston_marathon, package = "fpp3")
boston <- boston_marathon |>
  janitor::clean_names() |>
  filter(event == "Men's open division") |>
  filter(year >= 1924) |>
  mutate(
    minutes = as.numeric(time) / 60
  )


# air passengers works well with \log
air <- read_csv(
  "https://raw.githubusercontent.com/facebook/prophet/refs/heads/main/examples/example_air_passengers.csv"
)
air$n_passengers <- air$y
air$date <- air$ds
air$date_numeric <- as.numeric(air$date)
air$year <- year(air$date)
air$month <- month(air$date, label = TRUE, abbr = FALSE)
ggplot() +
  geom_line(aes(x = date, y = n_passengers), data = air) +
  kfbmisc::theme_kyle()

# %%
air_est <- feols(
  log(n_passengers) ~ year + i(month),
  data = air,
  vcov = NW() ~ date
)
air_est

# %%
(p_airline <- ggplot() +
  geom_line(
    aes(x = date, y = log(n_passengers)),
    data = air,
    linewidth = 1,
    color = kfbmisc::kyle_color("blue")
  ) +
  scale_x_date(minor_breaks = "1 year") +
  labs(x = NULL, y = NULL) +
  kfbmisc::theme_kyle(grid_minor = "v"))

kfbmisc::tikzsave(
  here("Exams/S2025_Final/figures/log_n_passengers_airline.pdf"),
  p_airline,
  width = 6,
  height = 3
)

# %%
earnings_sat <- feols(
  mean_earnings_10yr_after ~ sat_math_median + sat_math_median^2,
  data = college,
  vcov = "HC1"
)
earnings_sat
fitted <- data.frame(sat_math_median = seq(400, 780, by = 0.5))
fitted$earnings_hat <- predict(earnings_sat, newdata = fitted)

ggplot() +
  geom_point(
    aes(x = sat_math_median, y = mean_earnings_10yr_after),
    data = college
  ) +
  geom_line(
    aes(x = sat_math_median, y = earnings_hat),
    data = fitted,
    color = "blue",
    linewidth = 1.5
  )


# %%
feols(
  mean_earnings_10yr_after ~ share_low_income,
  data = college,
  vcov = "HC1"
)

feols(
  mean_earnings_10yr_after ~ i(hbcu) + share_low_income,
  data = college,
  vcov = "HC1"
)

# %%
boston$trend_1 <- boston$year
boston$trend_2 <- (boston$year - 1950) * (boston$year > 1950)
boston$trend_3 <- (boston$year - 1980) * (boston$year > 1980)

est_piecewise_trends <- feols(
  minutes ~
    year + I((year - 1950) * (year > 1950)) + I((year - 1980) * (year > 1980)),
  data = boston,
  vcov = NW() ~ year
)
est_piecewise_trends

etable()
