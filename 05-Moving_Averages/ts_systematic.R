# %%
library(tidyverse)
library(here)
library(fs)
library(fixest)
library(patchwork)
library(kfbmisc)

days <- seq(ymd("2020-12-21"), ymd("2025-01-10"), by = "1 day")
set.seed(20250330)

#' Draw eps_t = rho eps_{t-1} + v_t
#'
#' @param T Integer number of observations to draw
#' @param sd Standard deviation of AR(1) process
#' @param mean Mean of AR(1) process
#' @param rho Vector of autocorrelation coefficients
rAR <- function(T, mean = 0, sd = 1, rho = 0.75) {
  x <- arima.sim(
    list(ar = rho),
    n = T,
    rand.gen = function(n) rnorm(n, mean = 0, sd = sd)
  )
  x <- as.numeric(x)
  return(x)
}

smoothed_rAR <- function(
  T,
  mean = 0,
  sd = 1,
  rho = 0.75,
  .before = 30,
  .after = 30
) {
  x <- rAR(
    length(days),
    mean = 0,
    sd = 0.25,
    rho = c(0.1, 0.3, 0.15, 0.05, 0.05)
  )

  slider::slide_dbl(
    .x = as.numeric(x),
    .f = base::mean,
    .before = .before,
    .after = .after,
    .complete = TRUE
  )
}


mu_t <- function(days) {
  x <- days - min(days)
  mu_t <-
    92 +
    (x * 0.0005) +
    (month(days) == 11) * 0.2 +
    (month(days) == 12) * 0.4 +
    (month(days) == 1) * 0.2
  mu_t <- as.numeric(mu_t)
  mu_t <- slider::slide_dbl(
    mu_t,
    mean,
    .before = 30,
    .after = 30,
    .complete = TRUE
  )
}

# %%
true_mu <- mu_t(days)

y = true_mu +
  smoothed_rAR(
    T = length(days),
    mean = 0,
    sd = 0.001,
    rho = c(0.1, 0.3, 0.15, 0.05, 0.05)
  )

samples <- lapply(1:200, function(i) {
  true_mu +
    smoothed_rAR(
      T = length(days),
      mean = 0,
      sd = 0.001,
      rho = c(0.1, 0.3, 0.15, 0.05, 0.05)
    )
})

geom_line_for_sample <- function(days, y, alpha = 0.6) {
  geom_line(
    aes(x = days[!is.na(y)], y = y[!is.na(y)]),
    color = kfbmisc::tailwind_color("zinc-400"),
    linewidth = 0.3,
    alpha = alpha
  )
}

# %%
(plot_orig <- ggplot() +
  geom_line(
    aes(x = days[!is.na(y)], y = y[!is.na(y)]),
    color = kfbmisc::tailwind_color("zinc-800"),
    alpha = 1
  ) +
  labs(
    title = "Observed Time Series",
    x = NULL,
    y = NULL
  ) +
  scale_x_date(expand = expansion(mult = 0, add = 0)) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain", margin = margin(b = 10))
  ))

# %%
(plot_extra_samples_1 <- plot_orig +
  geom_line_for_sample(days, samples[[1]], alpha = 0.6) +
  labs(title = "Observed Time Series + 1 Extra Sample"))

(plot_extra_samples_2 <- plot_orig +
  geom_line_for_sample(days, samples[[1]], alpha = 0.6) +
  geom_line_for_sample(days, samples[[2]], alpha = 0.6) +
  labs(title = "Observed Time Series + 2 Extra Samples"))

(plot_extra_samples_3 <- plot_orig +
  geom_line_for_sample(days, samples[[1]], alpha = 0.6) +
  geom_line_for_sample(days, samples[[2]], alpha = 0.6) +
  geom_line_for_sample(days, samples[[3]], alpha = 0.6) +
  labs(title = "Observed Time Series + 3 Extra Samples"))

(plot_extra_samples_5 <- plot_orig +
  geom_line_for_sample(days, samples[[1]], alpha = 0.6) +
  geom_line_for_sample(days, samples[[2]], alpha = 0.6) +
  geom_line_for_sample(days, samples[[3]], alpha = 0.6) +
  geom_line_for_sample(days, samples[[4]], alpha = 0.6) +
  geom_line_for_sample(days, samples[[5]], alpha = 0.6) +
  labs(title = "Observed Time Series + 5 Extra Samples"))

# %%
# Multiple
ggplot_lines <- lapply(
  samples[1:25],
  function(sample) geom_line_for_sample(days[!is.na(y)], sample, alpha = 0.6)
)

(plot_extra_samples_and_mu <- plot_orig +
  ggplot_lines +
  geom_line(
    aes(x = days, y = true_mu),
    color = kfbmisc::kyle_color("magenta"),
    linewidth = 1.2
  ) +
  labs(
    title = "Observed Time Series + 25 Extra Samples; Systematic component: {\\color[HTML]{B3114B} $\\mu_t$}"
  ))


# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/time_series_sampling_orig.pdf"),
  plot_orig,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/time_series_sampling_extra_sample_1.pdf"),
  plot_extra_samples_1,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/time_series_sampling_extra_sample_2.pdf"),
  plot_extra_samples_2,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/time_series_sampling_extra_sample_5.pdf"),
  plot_extra_samples_5,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/time_series_sampling_and_mu.pdf"),
  plot_extra_samples_and_mu,
  width = 8,
  height = 4
)
