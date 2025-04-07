# %%
library(tidyverse)
library(here)
library(fpp3)
library(fixest)
library(tinytable)
fs::dir_create(here("05-Moving_Averages/tables"))
fs::dir_create(here("05-Moving_Averages/facts"))

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
simple_exp_smooth <- function(y, alpha = 0.8) {
  T <- length(y)
  l <- rep(NA, T)
  l[1] <- y[1] # l_1 = y_1

  # Update l in each period
  for (t in 2:length(y)) {
    l[t] <- alpha * y[t] + (1 - alpha) * l[t - 1]
  }
  return(l)
}

# retail$employed_hat_ses_alpha_0_8 <- simple_exp_smooth(
#   retail$employed,
#   alpha = 0.8
# )
# retail$employed_hat_ses_alpha_0_5 <- simple_exp_smooth(
#   retail$employed,
#   alpha = 0.5
# )
# retail$employed_hat_ses_alpha_0_2 <- simple_exp_smooth(
#   retail$employed,
#   alpha = 0.2
# )
retail$employed_hat_ses_alpha_0_8 <- ses(retail$employed, alpha = 0.8)$fitted
retail$employed_hat_ses_alpha_0_5 <- ses(retail$employed, alpha = 0.5)$fitted
retail$employed_hat_ses_alpha_0_2 <- ses(retail$employed, alpha = 0.2)$fitted
retail$employed_hat_ses_opt <- ses(retail$employed)$fitted

summary(ses(retail$employed))

mspe_table <- tibble(alpha = seq(0.05, 0.95, 0.1))
mspe_table$mspe <- map_dbl(
  mspe_table$alpha,
  \(alpha) {
    sum(ses(algeria_economy$Exports, alpha = alpha)$residuals^2)
  }
)

mspe_table |>
  slice(1:5) |>
  setNames(c("$\\alpha$", "MSPE")) |>
  tt() |>
  save_tt(
    here("05-Moving_Averages/tables/ses_mspe_table_1.tex"),
    overwrite = TRUE
  )
mspe_table |>
  slice(6:n()) |>
  setNames(c("$\\alpha$", "MSPE")) |>
  tt() |>
  save_tt(
    here("05-Moving_Averages/tables/ses_mspe_table_2.tex"),
    overwrite = TRUE
  )

opt_alpha <- ses(algeria_economy$Exports)$model$par["alpha"]
cat(
  round(opt_alpha, 3),
  file = here("05-Moving_Averages/facts/ses_opt_alpha.tex")
)

# %%
holt_smoothing <- function(y, alpha = 0.5, beta = 0.2) {
  T <- length(y)
  l <- rep(NA, T)
  b <- rep(NA, T)
  l[1] <- y[1]
  b[1] <- y[2] - y[1]

  # Update l in each period
  for (t in 2:length(y)) {
    l[t] <- alpha * y[t] + (1 - alpha) * (l[t - 1] + b[t - 1])
    b[t] <- beta * (l[t] - l[t - 1]) + (1 - beta) * b[t - 1]
  }
  return(data.frame(l = l, b = b))
}
retail$employed_hat_holt_alpha_0_5_beta_0_3 <-
  holt(retail$employed, alpha = 0.5, beta = 0.3)$fitted
retail$employed_hat_holt_alpha_0_5_beta_0_1 <-
  holt(retail$employed, alpha = 0.5, beta = 0.1)$fitted
retail$employed_hat_holt_alpha_0_1_beta_0_1 <-
  holt(retail$employed, alpha = 0.1, beta = 0.1)$fitted
retail$employed_hat_holt_opt <-
  holt(retail$employed)$fitted


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
    aes(x = month, y = employed_hat_ses_alpha_0_8, color = "B"),
    data = retail,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = month, y = employed_hat_ses_alpha_0_2, color = "C"),
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
(p_retail_holt <- ggplot() +
  geom_line(
    aes(x = month, y = employed, color = "A"),
    data = retail,
    linewidth = 1.25
  ) +
  geom_line(
    aes(
      x = month,
      y = employed_hat_holt_alpha_0_5_beta_0_3,
      color = "B"
    ),
    data = retail,
    linewidth = 1.25
  ) +
  geom_line(
    aes(
      x = month,
      y = employed_hat_holt_alpha_0_5_beta_0_1,
      color = "C"
    ),
    data = retail,
    linewidth = 1.25
  ) +
  geom_line(
    aes(
      x = month,
      y = employed_hat_holt_alpha_0_1_beta_0_1,
      color = "D"
    ),
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
      "C" = kfbmisc::kyle_color("blue"),
      "D" = kfbmisc::kyle_color("green")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "Holt $\\alpha = 0.5$, $\\beta = 0.3$",
      "C" = "Holt $\\alpha = 0.5$, $\\beta = 0.1$",
      "D" = "Holt $\\alpha = 0.1$, $\\beta = 0.1$"
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
(p_retail_opt <- ggplot() +
  geom_line(
    aes(x = month, y = employed, color = "A"),
    data = retail,
    linewidth = 1.25
  ) +
  geom_line(
    aes(
      x = month,
      y = employed_hat_ses_opt,
      color = "B"
    ),
    data = retail,
    linewidth = 1.25
  ) +
  geom_line(
    aes(
      x = month,
      y = employed_hat_holt_opt,
      color = "C"
    ),
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
      "C" = kfbmisc::kyle_color("blue")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "SES optimal",
      "C" = "Holt optimal"
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
  here("05-Moving_Averages/figures/retail_holt.pdf"),
  plot = p_retail_holt,
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
