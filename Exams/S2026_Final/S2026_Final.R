# %%# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(forecast)
library(fixest)
library(here)
library(fpp3)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
df <- read_csv(
  "~/Library/CloudStorage/Dropbox/Projects/Inference after DNN with Dependent Data/data/macroeconomic_effects_of_government_asset_purchases/base/megap_data_clean.csv",
  show_col_types = FALSE
) |>
  filter(year(date) >= 1966) |>
  mutate(
    month = month(date, label = TRUE, abbr = TRUE),
    ## Billions
    m_originations_billion = m_originations_not_seasonally_adjusted / 1000
  )

df <- df |>
  filter(year(date) >= 1995) |>
  mutate(
    trend_1 = as.numeric(date),
    trend_2 = as.numeric(date - ymd("2000-01-01")) *
      (date >= ymd("2000-01-01")),
    trend_3 = as.numeric(date - ymd("2004-01-01")) *
      (date >= ymd("2004-01-01")),
    trend_4 = as.numeric(date - ymd("2009-01-01")) *
      (date >= ymd("2009-01-01")),
  )


est_ts <- feols(
  m_originations_billion ~
    trend_1 + trend_2 + trend_3 + trend_4 + i(month),
  data = df |>
    filter(year(date) >= 1995)
)

est_ts |>
  etable()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ggplot() +
  geom_line(
    aes(x = date, y = m_holdings_stock_total),
    data = df
  ) +
  kfbmisc::theme_kyle(base_size = 10)

ggplot() +
  geom_line(
    aes(x = date, y = m_fed_funds_rate),
    data = df
  ) +
  kfbmisc::theme_kyle(base_size = 10)

ggplot() +
  geom_line(
    aes(x = date, y = m_originations_billion),
    data = df
  ) +
  labs(x = NULL, y = "Monthly Mortgage Originations (\\$ Billion)") +
  kfbmisc::theme_kyle(base_size = 10)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Raw Data + TS Regression
(p_raw_ts <- ggplot() +
  geom_vline(
    xintercept = c(ymd("2000-01-01"), ymd("2004-01-01"), ymd("2009-01-01")),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_line(
    aes(x = date, y = m_originations_billion, color = "Raw Data"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = date, y = predict(est_ts, newdata = df), color = "TS Regression"),
    data = df
  ) +
  scale_color_manual(
    values = c(
      "Raw Data" = kfbmisc::tailwind_color("zinc-400"),
      "TS Regression" = kfbmisc::kyle_color("green")
    )
  ) +
  labs(
    x = NULL,
    y = "Monthly Mortgage Originations (\\$B)",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 10, legend = "top"))

## Raw Data + SES Smoothing
(p_raw_ses <- ggplot() +
  geom_vline(
    xintercept = c(ymd("2000-01-01"), ymd("2004-01-01"), ymd("2009-01-01")),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_line(
    aes(x = date, y = m_originations_billion, color = "Raw Data"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(
      x = date,
      y = ses(m_originations_billion, alpha = 0.6)$fitted,
      color = "SES $\\alpha_1$"
    ),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(
      x = date,
      y = ses(m_originations_billion, alpha = 0.2)$fitted,
      color = "SES $\\alpha_2$"
    ),
    data = df,
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "Raw Data" = kfbmisc::tailwind_color("zinc-400"),
      "SES $\\alpha_1$" = kfbmisc::kyle_color("magenta"),
      "SES $\\alpha_2$" = kfbmisc::kyle_color("blue")
    )
  ) +
  labs(
    x = NULL,
    y = "Monthly Mortgage Originations (\\$B)",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 10, legend = "top") +
  theme(axis.title.y = element_text(size = rel(1))))

# %%
kfbmisc::tikzsave(
  here("Exams/S2026_Final/figures/raw_data_ts_regression.pdf"),
  p_raw_ts,
  width = 6,
  height = 3.5
)

kfbmisc::tikzsave(
  here("Exams/S2026_Final/figures/raw_data_ses.pdf"),
  p_raw_ses,
  width = 6,
  height = 3.5
)
