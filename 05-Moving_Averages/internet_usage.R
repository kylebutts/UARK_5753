# %%
library(tidyverse)
library(here)
library(fpp3)
library(fixest)
library(forecast)
library(patchwork)

data(WWWusage, package = "datasets")
# WWWusage |> fable::as_tsibble()
internet <- tibble(
  minute = 1:100,
  usage = as.numeric(WWWusage)
)

# %%
(p_usage <- ggplot() +
  geom_line(
    aes(x = minute, y = usage),
    data = internet,
    linewidth = 1.25,
    color = kfbmisc::tailwind_color("zinc-500")
  ) +
  labs(
    x = NULL,
    y = "U.S. usage"
  ) +
  kfbmisc::theme_kyle(base_size = 14))

# %%
internet$usage_hat_ses_alpha_pt8 <- ses(internet$usage, alpha = 0.8)$fitted
internet$usage_hat_ses_alpha_pt5 <- ses(internet$usage, alpha = 0.5)$fitted
internet$usage_hat_ses_alpha_pt2 <- ses(internet$usage, alpha = 0.2)$fitted
internet$usage_hat_ses_opt <- ses(internet$usage)$fitted
internet$usage_hat_holt_opt <- holt(internet$usage)$fitted
internet$usage_hat_holt_damped <- holt(internet$usage, damped = TRUE)$fitted

forecast_ses_opt <- ses(internet$usage) |>
  as.data.frame() |>
  rownames_to_column("minute") |>
  mutate(minute = as.numeric(minute)) |>
  setNames(c(
    "minute",
    "est",
    "ci_80_lower",
    "ci_80_upper",
    "ci_95_lower",
    "ci_95_upper"
  ))


forecast_holt_opt <- holt(internet$usage) |>
  as.data.frame() |>
  rownames_to_column("minute") |>
  mutate(minute = as.numeric(minute)) |>
  setNames(c(
    "minute",
    "est",
    "ci_80_lower",
    "ci_80_upper",
    "ci_95_lower",
    "ci_95_upper"
  ))

forecast_holt_damped <- holt(internet$usage, damped = TRUE) |>
  as.data.frame() |>
  rownames_to_column("minute") |>
  mutate(minute = as.numeric(minute)) |>
  setNames(c(
    "minute",
    "est",
    "ci_80_lower",
    "ci_80_upper",
    "ci_95_lower",
    "ci_95_upper"
  ))

# %%
(p_usage_ses <- ggplot() +
  geom_line(
    aes(x = minute, y = usage, color = "A"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = minute, y = usage_hat_ses_alpha_pt8, color = "B"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = minute, y = usage_hat_ses_alpha_pt2, color = "C"),
    data = internet,
    linewidth = 1.25
  ) +
  labs(
    x = NULL,
    y = "Internet Usage by Minute",
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


(p_usage_ses_and_holt <- ggplot() +
  geom_line(
    aes(x = minute, y = usage, color = "A"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = minute, y = usage_hat_ses_alpha_pt8, color = "B"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = minute, y = usage_hat_ses_alpha_pt2, color = "C"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = minute, y = usage_hat_holt_opt, color = "D"),
    data = internet,
    linewidth = 1.25
  ) +
  labs(
    x = NULL,
    y = "Internet Usage by Minute",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("blue"),
      "D" = kfbmisc::kyle_color("yellow")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "SES with $\\alpha = 0.8$",
      "C" = "SES with $\\alpha = 0.2$",
      "D" = "Holt Method with Optimal $\\hat{\\alpha}$ and $\\hat{\\beta}$"
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
(p_forecast_ses <- ggplot() +
  geom_line(
    aes(x = minute, y = usage, color = "A"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = minute, y = usage_hat_ses_opt, color = "B"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(aes(x = 0, y = 100, color = "C"), linewidth = 1.25) +
  geom_line(aes(x = 0, y = 100, color = "D"), linewidth = 1.25) +
  geom_line(
    aes(x = minute, y = est),
    data = forecast_ses_opt,
    color = kfbmisc::kyle_color("magenta"),
    linewidth = 1.25
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_80_lower, ymax = ci_80_upper),
    data = forecast_ses_opt,
    fill = kfbmisc::kyle_color("magenta"),
    alpha = 0.1
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_95_lower, ymax = ci_95_upper),
    data = forecast_ses_opt,
    fill = kfbmisc::kyle_color("magenta"),
    alpha = 0.1
  ) +
  labs(
    x = NULL,
    y = "Internet Usage by Minute",
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
      "B" = "SES",
      "C" = "Holt Method",
      "D" = "Holt Method w/ Dampening"
    )
  ) +
  scale_y_continuous(limits = c(50, 350), expand = expansion(0, 0)) +
  kfbmisc::theme_kyle(base_size = 14, legend = "top"))

(p_forecast_holt <- ggplot() +
  geom_line(
    aes(x = minute, y = usage, color = "A"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(aes(x = 0, y = 100, color = "B"), linewidth = 1.25) +
  geom_line(
    aes(x = minute, y = usage_hat_holt_opt, color = "C"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(aes(x = 0, y = 100, color = "D"), linewidth = 1.25) +
  geom_line(
    aes(x = minute, y = est),
    data = forecast_holt_opt,
    color = kfbmisc::kyle_color("blue"),
    linewidth = 1.25
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_80_lower, ymax = ci_80_upper),
    data = forecast_holt_opt,
    fill = kfbmisc::kyle_color("blue"),
    alpha = 0.1
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_95_lower, ymax = ci_95_upper),
    data = forecast_holt_opt,
    fill = kfbmisc::kyle_color("blue"),
    alpha = 0.1
  ) +
  labs(
    x = NULL,
    y = "Internet Usage by Minute",
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
      "B" = "SES",
      "C" = "Holt Method",
      "D" = "Holt Method w/ Dampening"
    )
  ) +
  scale_y_continuous(limits = c(50, 350), expand = expansion(0, 0)) +
  kfbmisc::theme_kyle(base_size = 14, legend = "top"))

(p_forecast_holt_damped <- ggplot() +
  geom_line(
    aes(x = minute, y = usage, color = "A"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(aes(x = 0, y = 100, color = "B"), linewidth = 1.25) +
  geom_line(aes(x = 0, y = 100, color = "C"), linewidth = 1.25) +
  geom_line(
    aes(x = minute, y = usage_hat_holt_damped, color = "D"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = minute, y = est),
    data = forecast_holt_damped,
    color = kfbmisc::kyle_color("green"),
    linewidth = 1.25
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_80_lower, ymax = ci_80_upper),
    data = forecast_holt_damped,
    fill = kfbmisc::kyle_color("green"),
    alpha = 0.1
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_95_lower, ymax = ci_95_upper),
    data = forecast_holt_damped,
    fill = kfbmisc::kyle_color("green"),
    alpha = 0.1
  ) +
  labs(
    x = NULL,
    y = "Internet Usage by Minute",
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
      "B" = "SES",
      "C" = "Holt Method",
      "D" = "Holt Method w/ Dampening"
    )
  ) +
  scale_y_continuous(limits = c(50, 350), expand = expansion(0, 0)) +
  kfbmisc::theme_kyle(base_size = 14, legend = "top"))

(p_usage_forecast <-
  (p_forecast_ses + p_forecast_holt + p_forecast_holt_damped) +
  plot_layout(guides = "collect", axes = "collect") &
  labs(y = NULL) &
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.location = "panel",
    legend.justification = c(0.5, 0),
    plot.margin = margin(8, 4, 8, 4, "pt")
  ))


# %%
# This time without extra legends
(p_forecast_ses_single <- ggplot() +
  geom_line(
    aes(x = minute, y = usage, color = "A"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = minute, y = usage_hat_ses_opt, color = "B"),
    data = internet,
    linewidth = 1.25
  ) +
  # geom_line(aes(x = 0, y = 100, color = "C"), linewidth = 1.25) +
  # geom_line(aes(x = 0, y = 100, color = "D"), linewidth = 1.25) +
  geom_line(
    aes(x = minute, y = est),
    data = forecast_ses_opt,
    color = kfbmisc::kyle_color("magenta"),
    linewidth = 1.25
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_80_lower, ymax = ci_80_upper),
    data = forecast_ses_opt,
    fill = kfbmisc::kyle_color("magenta"),
    alpha = 0.1
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_95_lower, ymax = ci_95_upper),
    data = forecast_ses_opt,
    fill = kfbmisc::kyle_color("magenta"),
    alpha = 0.1
  ) +
  labs(
    x = NULL,
    y = "Internet Usage by Minute",
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
      "D" = "Holt Method with Dampening"
    )
  ) +
  scale_y_continuous(limits = c(50, 350), expand = expansion(0, 0)) +
  kfbmisc::theme_kyle(base_size = 14, legend = "top"))

(p_forecast_holt_single <- ggplot() +
  geom_line(
    aes(x = minute, y = usage, color = "A"),
    data = internet,
    linewidth = 1.25
  ) +
  # geom_line(aes(x = 0, y = 100, color = "B"), linewidth = 1.25) +
  geom_line(
    aes(x = minute, y = usage_hat_holt_opt, color = "C"),
    data = internet,
    linewidth = 1.25
  ) +
  # geom_line(aes(x = 0, y = 100, color = "D"), linewidth = 1.25) +
  geom_line(
    aes(x = minute, y = est),
    data = forecast_holt_opt,
    color = kfbmisc::kyle_color("blue"),
    linewidth = 1.25
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_80_lower, ymax = ci_80_upper),
    data = forecast_holt_opt,
    fill = kfbmisc::kyle_color("blue"),
    alpha = 0.1
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_95_lower, ymax = ci_95_upper),
    data = forecast_holt_opt,
    fill = kfbmisc::kyle_color("blue"),
    alpha = 0.1
  ) +
  labs(
    x = NULL,
    y = "Internet Usage by Minute",
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
      "D" = "Holt Method with Dampening"
    )
  ) +
  scale_y_continuous(limits = c(50, 350), expand = expansion(0, 0)) +
  kfbmisc::theme_kyle(base_size = 14, legend = "top"))

(p_forecast_holt_damped_single <- ggplot() +
  geom_line(
    aes(x = minute, y = usage, color = "A"),
    data = internet,
    linewidth = 1.25
  ) +
  # geom_line(aes(x = 0, y = 100, color = "B"), linewidth = 1.25) +
  # geom_line(aes(x = 0, y = 100, color = "C"), linewidth = 1.25) +
  geom_line(
    aes(x = minute, y = usage_hat_holt_damped, color = "D"),
    data = internet,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = minute, y = est),
    data = forecast_holt_damped,
    color = kfbmisc::kyle_color("green"),
    linewidth = 1.25
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_80_lower, ymax = ci_80_upper),
    data = forecast_holt_damped,
    fill = kfbmisc::kyle_color("green"),
    alpha = 0.1
  ) +
  geom_ribbon(
    aes(x = minute, ymin = ci_95_lower, ymax = ci_95_upper),
    data = forecast_holt_damped,
    fill = kfbmisc::kyle_color("green"),
    alpha = 0.1
  ) +
  labs(
    x = NULL,
    y = "Internet Usage by Minute",
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
      "D" = "Holt Method with Dampening"
    )
  ) +
  scale_y_continuous(limits = c(50, 350), expand = expansion(0, 0)) +
  kfbmisc::theme_kyle(base_size = 14, legend = "top"))

# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/usage.pdf"),
  plot = p_usage,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/usage_ses_alphas.pdf"),
  plot = p_usage_ses,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/usage_ses_and_holt.pdf"),
  plot = p_usage_ses_and_holt,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/usage_forecast_ses.pdf"),
  plot = p_forecast_ses_single,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/usage_forecast_holt.pdf"),
  plot = p_forecast_holt_single,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/usage_forecast_holt_damped.pdf"),
  plot = p_forecast_holt_damped_single,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/usage_forecast_all_3.pdf"),
  plot = p_usage_forecast,
  width = 8,
  height = 4.5
)

# %%
