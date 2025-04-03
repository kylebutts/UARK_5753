# %%
library(tidyverse)
library(here)
library(fpp3)
library(fixest)

data(us_employment, package = "fpp3")
retail_tsibble <- us_employment |>
  janitor::clean_names() |>
  filter(year(month) >= 1990, title == "Retail Trade") |>
  select(-series_id) |>
  # millions
  mutate(employed = employed / 1000)

retail <- retail_tsibble |>
  as_tibble() |>
  mutate(month = as.Date(month))

# %%
(p_retail <- ggplot() +
  geom_line(
    aes(x = month, y = employed),
    data = retail,
    linewidth = 1.25,
    color = kfbmisc::tailwind_color("zinc-700")
  ) +
  labs(
    x = NULL,
    y = "U.S. Retail Employment (millions)"
  ) +
  kfbmisc::theme_kyle(base_size = 14))

# %%
simple_exp_smooth <- function(x, alpha = 0.5) {
  x_hat <- c(x[1])
  for (t in 2:length(x)) {
    x_hat[t] <-
      alpha * x[t - 1] + (1 - alpha) * x_hat[t - 1]
  }
  x_hat[1] <- NA
  return(x_hat)
}

retail$employed_hat_ses_alpha_pt8 <- simple_exp_smooth(
  retail$employed,
  alpha = 0.8
)
retail$employed_hat_ses_alpha_pt5 <- simple_exp_smooth(
  retail$employed,
  alpha = 0.5
)
retail$employed_hat_ses_alpha_pt2 <- simple_exp_smooth(
  retail$employed,
  alpha = 0.2
)


# %%
ma_2_by_12 <- function(x) {
  ma_12 <- slider::slide_dbl(x, mean, .before = 5, .after = 6, .complete = TRUE)
  ma_2_by_12 <- slider::slide_dbl(
    ma_12,
    mean,
    .before = 1,
    .after = 0,
    .complete = TRUE
  )

  return(ma_2_by_12)
}

retail$employed_hat_ma_2_by_12 <- ma_2_by_12(retail$employed)

retail$employed_detrended <- retail$employed - retail$employed_hat_ma_2_by_12

seasonality_model <- feols(
  employed_detrended ~ 0 + i(month(month)),
  data = retail
)
retail$employed_seasonal <- predict(seasonality_model, sample = "original")

retail$employed_resid <- retail$employed -
  retail$employed_hat_ma_2_by_12 -
  retail$employed_seasonal

# %%
(p_retail_ses <- ggplot() +
  geom_line(
    aes(x = month, y = employed, color = "A"),
    data = retail,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = month, y = employed_hat_ses_alpha_pt8, color = "B"),
    data = retail,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = month, y = employed_hat_ses_alpha_pt2, color = "C"),
    data = retail,
    linewidth = 1.25
  ) +
  labs(
    x = NULL,
    y = "Retail Employment (millions)",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      # "B" = "white",
      "C" = kfbmisc::kyle_color("blue")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "SES with $\\alpha = 0.8$",
      "C" = "SES with $\\alpha = 0.2$"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))


# %%
(p_retail_2_by_12 <- ggplot() +
  geom_line(
    aes(x = month, y = employed, color = "A"),
    data = retail,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = month, y = employed_hat_ma_2_by_12, color = "B"),
    data = retail,
    linewidth = 1.25
  ) +
  labs(
    x = NULL,
    y = "Retail Employment (millions)",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "$2 \\times 12$ MA"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(p_retail_detrended <- ggplot() +
  geom_line(
    aes(x = month, y = employed_detrended, color = "A"),
    data = retail,
    linewidth = 1.25
  ) +
  # geom_line(
  #   aes(x = month, y = employed_seasonal, color = "B"),
  #   data = retail,
  #   linewidth = 1.25
  # ) +
  labs(
    x = NULL,
    y = "Detrended Retail Employment (millions)",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta")
    ),
    labels = c(
      "A" = "$y_t - \\hat{T}_t$",
      "B" = "$\\hat{S}_t$"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(p_retail_seasonal <- ggplot() +
  geom_line(
    aes(x = month, y = employed_detrended, color = "A"),
    data = retail,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = month, y = employed_seasonal, color = "B"),
    data = retail,
    linewidth = 1.25
  ) +
  labs(
    x = NULL,
    y = "Detrended Retail Employment (millions)",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta")
    ),
    labels = c(
      "A" = "$y_t - \\hat{T}_t$",
      "B" = "$\\hat{S}_t$"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

# %%

(p_retail_resid <- ggplot() +
  geom_line(
    aes(x = month, y = employed_resid, color = "A"),
    data = retail,
    linewidth = 1.25
  ) +
  # geom_line(
  #   aes(x = month, y = employed_seasonal, color = "B"),
  #   data = retail,
  #   linewidth = 1.25
  # ) +
  labs(
    x = NULL,
    y = "Residual Retail Employment (millions)",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta")
    ),
    labels = c(
      "A" = "$y_t - \\hat{T}_t$ - $\\hat{S}_t$"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/retail.pdf"),
  plot = p_retail,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/retail_ses_alphas.pdf"),
  plot = p_retail_ses,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/retail_ma_2_by_12.pdf"),
  plot = p_retail_2_by_12,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/retail_detrended.pdf"),
  plot = p_retail_detrended,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/retail_seasonal.pdf"),
  plot = p_retail_seasonal,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/retail_resid.pdf"),
  plot = p_retail_resid,
  width = 8,
  height = 4.5
)
