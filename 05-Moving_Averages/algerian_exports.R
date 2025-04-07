# %%
library(tidyverse)
library(here)
library(fpp3)
library(fixest)
library(tinytable)
fs::dir_create(here("05-Moving_Averages/tables"))
fs::dir_create(here("05-Moving_Averages/facts"))

data(global_economy, package = "tsibbledata")
algeria_economy <- global_economy |>
  filter(Country == "Algeria") |>
  janitor::clean_names()

# %%
(p_algerian_exports <- ggplot() +
  geom_line(
    aes(x = year, y = exports),
    data = algeria_economy,
    linewidth = 1.25,
    color = kfbmisc::tailwind_color("zinc-700")
  ) +
  labs(
    x = NULL,
    y = "U.S. Algerian Exports (\\% of GDP)"
  ) +
  kfbmisc::theme_kyle(base_size = 14))

# %%
algeria_economy$exports_hat_ses_alpha_0_8 <- ses(
  algeria_economy$exports,
  alpha = 0.8
)$fitted
algeria_economy$exports_hat_ses_alpha_0_5 <- ses(
  algeria_economy$exports,
  alpha = 0.5
)$fitted
algeria_economy$exports_hat_ses_alpha_0_2 <- ses(
  algeria_economy$exports,
  alpha = 0.2
)$fitted
algeria_economy$exports_hat_ses_opt <- ses(algeria_economy$exports)$fitted

summary(ses(algeria_economy$exports))

mspe_table <- tibble(alpha = seq(0.05, 0.95, 0.1))
mspe_table$mspe <- map_dbl(
  mspe_table$alpha,
  \(alpha) {
    sum(ses(algeria_economy$exports, alpha = alpha)$residuals^2)
  }
)

mspe_table |>
  setNames(c("$\\alpha$", "MSPE")) |>
  tt() |>
  print("markdown")

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

opt_alpha <- ses(algeria_economy$exports)$model$par["alpha"]
cat(
  sprintf("$\\hat{\\alpha} = %0.3f$", opt_alpha),
  file = here("05-Moving_Averages/facts/algeria_exports_ses_opt_alpha.tex")
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
algeria_economy$exports_hat_holt_alpha_0_5_beta_0_3 <-
  forecast::holt(algeria_economy$exports, alpha = 0.5, beta = 0.3)$fitted
algeria_economy$exports_hat_holt_alpha_0_5_beta_0_1 <-
  forecast::holt(algeria_economy$exports, alpha = 0.5, beta = 0.1)$fitted
algeria_economy$exports_hat_holt_alpha_0_1_beta_0_1 <-
  forecast::holt(algeria_economy$exports, alpha = 0.1, beta = 0.1)$fitted
algeria_economy$exports_hat_holt_opt <-
  forecast::holt(algeria_economy$exports)$fitted

holt_opt_params <- forecast::holt(algeria_economy$exports)$model$par

cat(
  sprintf("$\\hat{\\alpha} = %0.3f$", holt_opt_params["alpha"]),
  file = here("05-Moving_Averages/facts/algeria_exports_holt_opt_alpha.tex")
)
cat(
  sprintf("$\\hat{\\beta} = %0.3f$", holt_opt_params["beta"]),
  file = here("05-Moving_Averages/facts/algeria_exports_holt_opt_beta.tex")
)
holt_opt_params[c("alpha", "beta")]

# %%
(p_algerian_exports_ses_alpha_pt8 <- ggplot() +
  geom_line(
    aes(x = year, y = exports, color = "A"),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = year, y = exports_hat_ses_alpha_0_8, color = "B"),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  labs(
    x = NULL,
    y = "Algerian Exports (\\% of GDP)",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "SES with $\\alpha = 0.8$"
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
(p_algerian_exports_ses <- ggplot() +
  geom_line(
    aes(x = year, y = exports, color = "A"),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = year, y = exports_hat_ses_alpha_0_8, color = "B"),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = year, y = exports_hat_ses_alpha_0_2, color = "C"),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  labs(
    x = NULL,
    y = "Algerian Exports (\\% of GDP)",
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

# %%
(p_algerian_exports_holt <- ggplot() +
  geom_line(
    aes(x = year, y = exports, color = "A"),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  geom_line(
    aes(
      x = year,
      y = exports_hat_holt_alpha_0_5_beta_0_3,
      color = "B"
    ),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  geom_line(
    aes(
      x = year,
      y = exports_hat_holt_alpha_0_5_beta_0_1,
      color = "C"
    ),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  geom_line(
    aes(
      x = year,
      y = exports_hat_holt_alpha_0_1_beta_0_1,
      color = "D"
    ),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  labs(
    x = NULL,
    y = "Algerian Exports (\\% of GDP)",
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
(p_algerian_exports_opt <- ggplot() +
  geom_line(
    aes(x = year, y = exports, color = "A"),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  geom_line(
    aes(
      x = year,
      y = exports_hat_ses_opt,
      color = "B"
    ),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  geom_line(
    aes(
      x = year,
      y = exports_hat_holt_opt,
      color = "C"
    ),
    data = algeria_economy,
    linewidth = 1.25
  ) +
  labs(
    x = NULL,
    y = "Algerian Exports (\\% of GDP)",
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
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/algerian_exports.pdf"),
  plot = p_algerian_exports,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/algerian_exports_ses_alpha_pt8.pdf"),
  plot = p_algerian_exports_ses_alpha_pt8,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/algerian_exports_ses_alphas.pdf"),
  plot = p_algerian_exports_ses,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/algerian_exports_holt.pdf"),
  plot = p_algerian_exports_holt,
  width = 8,
  height = 4.5
)
