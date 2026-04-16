# %%
library(tidyverse)
library(here)
library(fpp3)
library(fixest)
library(forecast)
library(patchwork)

candy <- here("05-Moving_Averages/data/FRED/Candy_Production.csv") |>
  read_csv(show_col_types = FALSE) |>
  setNames(c("date", "production")) |>
  filter(date >= ymd("2000-01-01")) |>
  arrange(date)

# %%
(p_production <- ggplot() +
  geom_line(
    aes(x = date, y = production),
    data = candy,
    linewidth = 1.25,
    color = kfbmisc::tailwind_color("zinc-500")
  ) +
  labs(
    x = NULL,
    y = "U.S. Candy Production"
  ) +
  scale_y_continuous(limits = c(50, 130), expand = expansion(0, 0)) +
  kfbmisc::theme_kyle(base_size = 14))

# %%
candy$production_hat_ses_opt <- ses(candy$production)$fitted
candy$production_hat_holt_opt <- holt(candy$production)$fitted
candy$production_hat_holt_winters_opt <- candy$production |>
  ts(start = c(2000, 1), frequency = 12) |>
  hw() |>
  _$fitted

forecast_ses_opt <- candy$production |>
  ts(start = c(2000, 1), frequency = 12) |>
  ses() |>
  as.data.frame() |>
  rownames_to_column("date") |>
  setNames(c(
    "date",
    "est",
    "ci_80_lower",
    "ci_80_upper",
    "ci_95_lower",
    "ci_95_upper"
  )) |>
  mutate(date = my(date))

forecast_holt_opt <- candy$production |>
  ts(start = c(2000, 1), frequency = 12) |>
  holt() |>
  as.data.frame() |>
  rownames_to_column("date") |>
  setNames(c(
    "date",
    "est",
    "ci_80_lower",
    "ci_80_upper",
    "ci_95_lower",
    "ci_95_upper"
  )) |>
  mutate(date = my(date))

forecast_holt_winters_opt <- candy$production |>
  ts(start = c(2000, 1), frequency = 12) |>
  hw() |>
  as.data.frame() |>
  rownames_to_column("date") |>
  setNames(c(
    "date",
    "est",
    "ci_80_lower",
    "ci_80_upper",
    "ci_95_lower",
    "ci_95_upper"
  )) |>
  mutate(date = my(date))

# %%
# This time without extra legends
(p_forecast_ses <- ggplot() +
  geom_line(
    aes(x = date, y = production, color = "A"),
    data = candy,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = date, y = production_hat_ses_opt, color = "B"),
    data = candy,
    linewidth = 1.25
  ) +
  # geom_line(aes(x = 0, y = 100, color = "C"), linewidth = 1.25) +
  # geom_line(aes(x = 0, y = 100, color = "D"), linewidth = 1.25) +
  geom_line(
    aes(x = date, y = est),
    data = forecast_ses_opt,
    color = kfbmisc::kyle_color("magenta"),
    linewidth = 1.25
  ) +
  geom_ribbon(
    aes(x = date, ymin = ci_80_lower, ymax = ci_80_upper),
    data = forecast_ses_opt,
    fill = kfbmisc::kyle_color("magenta"),
    alpha = 0.1
  ) +
  geom_ribbon(
    aes(x = date, ymin = ci_95_lower, ymax = ci_95_upper),
    data = forecast_ses_opt,
    fill = kfbmisc::kyle_color("magenta"),
    alpha = 0.1
  ) +
  labs(
    x = NULL,
    y = "candy production by date",
    color = NULL,
    fill = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("blue"),
      "D" = kfbmisc::kyle_color("green")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Simple Exponential Smoothing",
      "C" = "Holt Method",
      "D" = "Holt Winters Method"
    )
  ) +
  scale_y_continuous(limits = c(50, 130), expand = expansion(0, 0)) +
  kfbmisc::theme_kyle(base_size = 14, legend = "top"))

(p_forecast_holt <- ggplot() +
  geom_line(
    aes(x = date, y = production, color = "A"),
    data = candy,
    linewidth = 1.25
  ) +
  # geom_line(aes(x = 0, y = 100, color = "B"), linewidth = 1.25) +
  geom_line(
    aes(x = date, y = production_hat_holt_opt, color = "C"),
    data = candy,
    linewidth = 1.25
  ) +
  # geom_line(aes(x = 0, y = 100, color = "D"), linewidth = 1.25) +
  geom_line(
    aes(x = date, y = est),
    data = forecast_holt_opt,
    color = kfbmisc::kyle_color("blue"),
    linewidth = 1.25
  ) +
  geom_ribbon(
    aes(x = date, ymin = ci_80_lower, ymax = ci_80_upper),
    data = forecast_holt_opt,
    fill = kfbmisc::kyle_color("blue"),
    alpha = 0.1
  ) +
  geom_ribbon(
    aes(x = date, ymin = ci_95_lower, ymax = ci_95_upper),
    data = forecast_holt_opt,
    fill = kfbmisc::kyle_color("blue"),
    alpha = 0.1
  ) +
  labs(
    x = NULL,
    y = "candy production by date",
    color = NULL,
    fill = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("blue"),
      "D" = kfbmisc::kyle_color("green")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Simple Exponential Smoothing",
      "C" = "Holt Method",
      "D" = "Holt Winters Method"
    )
  ) +
  scale_y_continuous(limits = c(50, 130), expand = expansion(0, 0)) +
  kfbmisc::theme_kyle(base_size = 14, legend = "top"))

(p_forecast_holt_winters <- ggplot() +
  geom_line(
    aes(x = date, y = production, color = "A"),
    data = candy,
    linewidth = 1.25
  ) +
  # geom_line(aes(x = 0, y = 100, color = "B"), linewidth = 1.25) +
  geom_line(
    aes(x = date, y = production_hat_holt_opt, color = "D"),
    data = candy,
    linewidth = 1.25
  ) +
  # geom_line(aes(x = 0, y = 100, color = "D"), linewidth = 1.25) +
  geom_line(
    aes(x = date, y = est),
    data = forecast_holt_winters_opt,
    color = kfbmisc::kyle_color("green"),
    linewidth = 1.25
  ) +
  geom_ribbon(
    aes(x = date, ymin = ci_80_lower, ymax = ci_80_upper),
    data = forecast_holt_winters_opt,
    fill = kfbmisc::kyle_color("green"),
    alpha = 0.1
  ) +
  geom_ribbon(
    aes(x = date, ymin = ci_95_lower, ymax = ci_95_upper),
    data = forecast_holt_winters_opt,
    fill = kfbmisc::kyle_color("green"),
    alpha = 0.1
  ) +
  labs(
    x = NULL,
    y = "candy production by date",
    color = NULL,
    fill = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("blue"),
      "D" = kfbmisc::kyle_color("green")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Simple Exponential Smoothing",
      "C" = "Holt Method",
      "D" = "Holt Winters Method"
    )
  ) +
  scale_y_continuous(limits = c(50, 130), expand = expansion(0, 0)) +
  kfbmisc::theme_kyle(base_size = 14, legend = "top"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/candy_production.pdf"),
  plot = p_production,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/candy_forecast_ses.pdf"),
  plot = p_forecast_ses,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/candy_forecast_holt.pdf"),
  plot = p_forecast_holt,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/candy_forecast_holt_winters.pdf"),
  plot = p_forecast_holt_winters,
  width = 8,
  height = 4.5
)

# %%
