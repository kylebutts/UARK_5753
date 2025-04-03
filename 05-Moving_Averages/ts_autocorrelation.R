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

mu_t <- function(days, trending = TRUE) {
  x <- days - min(days)
  mu_t <- rep(92, length(x))
  if (trending == TRUE) {
    mu_t <- mu_t +
      (x * 0.0005) +
      (month(days) == 11) * 0.2 +
      (month(days) == 12) * 0.4 +
      (month(days) == 1) * 0.2
  }
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
y1 = mu_t(days, trending = FALSE) +
  smoothed_rAR(
    T = length(days),
    mean = 0,
    sd = 0.001,
    rho = c(0.1, 0.3, 0.15, 0.05, 0.05)
  )

y2 = mu_t(days, trending = FALSE) +
  rnorm(length(days), mean = 0, sd = 0.02)

y3 = mu_t(days, trending = TRUE) +
  rnorm(length(days), mean = 0, sd = 0.02)

df <- data.frame(
  day = days[!is.na(y1)],
  y1 = y1[!is.na(y1)],
  y2 = y2[!is.na(y1)],
  y3 = y3[!is.na(y1)]
)
df <- as_tibble(df)

# Here, we take y3 and reswidualize it to show that there is no more autocorrelation
est_ts <- feols(
  y3 ~ day + i(week(day)),
  data = df
)
df$y3_resid <- resid(est_ts, na.rm = FALSE)


# %%
# Figures ----
(ts_y1 <- ggplot() +
  geom_hline(
    yintercept = mean(df$y1, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_line(
    aes(x = day, y = y1),
    data = df,
    color = kfbmisc::kyle_color("magenta"),
  ) +
  labs(
    title = "Observed Time Series",
    x = NULL,
    y = "$y_t - \\hat{\\mu}_t$"
  ) +
  scale_x_date(expand = expansion(mult = 0, add = 0)) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain", margin = margin(b = 10))
  ))


(autocov_y1 <- ggplot() +
  geom_hline(
    yintercept = mean(df$y1, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_vline(
    xintercept = mean(df$y1, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_point(
    aes(
      x = (df$y1)[1:(nrow(df) - 1)],
      y = (df$y1)[2:nrow(df)],
    ),
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  labs(
    x = "$y_t$",
    y = "$y_{t+1}$",
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain", margin = margin(b = 10))
  ))


# %%
(ts_y2 <- ggplot() +
  geom_hline(
    yintercept = mean(df$y2, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_line(
    aes(x = day, y = y2),
    data = df,
    color = kfbmisc::kyle_color("magenta"),
  ) +
  labs(
    title = "Observed Time Series",
    x = NULL,
    y = "$y_t - \\hat{\\mu}_t$"
  ) +
  scale_x_date(expand = expansion(mult = 0, add = 0)) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain", margin = margin(b = 10))
  ))

(autocov_y2 <- ggplot() +
  geom_hline(
    yintercept = mean(df$y2, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_vline(
    xintercept = mean(df$y2, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_point(
    aes(
      x = (df$y2)[1:(nrow(df) - 1)],
      y = (df$y2)[2:nrow(df)],
    ),
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  labs(
    x = "$y_t$",
    y = "$y_{t+1}$",
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain", margin = margin(b = 10))
  ))

# %%
(ts_y3 <- ggplot() +
  geom_hline(
    yintercept = mean(df$y3, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_line(
    aes(x = day, y = y3),
    data = df,
    color = kfbmisc::kyle_color("magenta"),
  ) +
  labs(
    title = "Observed Time Series",
    x = NULL,
    y = "$y_t$"
  ) +
  scale_x_date(expand = expansion(mult = 0, add = 0)) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain", margin = margin(b = 10))
  ))

(autocov_y3 <- ggplot() +
  geom_hline(
    yintercept = mean(df$y3, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_vline(
    xintercept = mean(df$y3, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_point(
    aes(
      x = (df$y3)[1:(nrow(df) - 1)],
      y = (df$y3)[2:nrow(df)],
    ),
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  labs(
    x = "$y_t$",
    y = "$y_{t+1}$",
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain", margin = margin(b = 10))
  ))

# %%
(ts_y3_resid <- ggplot() +
  geom_hline(
    yintercept = mean(df$y3_resid, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_line(
    aes(x = day, y = y3_resid),
    data = df,
    color = kfbmisc::kyle_color("magenta"),
  ) +
  labs(
    title = "Residualized Time Series",
    x = NULL,
    y = "$y_t - \\hat{\\mu}_t$"
  ) +
  scale_x_date(expand = expansion(mult = 0, add = 0)) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain", margin = margin(b = 10))
  ))

(autocov_y3_resid <- ggplot() +
  geom_hline(
    yintercept = mean(df$y3_resid, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_vline(
    xintercept = mean(df$y3_resid, na.rm = TRUE),
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_point(
    aes(
      x = (df$y3_resid)[1:(nrow(df) - 1)],
      y = (df$y3_resid)[2:nrow(df)],
    ),
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  labs(
    x = "$y_t - \\hat{\\mu}_t$",
    y = "$y_{t+1} - \\hat{\\mu}_{t+1}$",
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain", margin = margin(b = 10))
  ))


# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/ex_autocov_ts_y1.pdf"),
  ts_y1,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/ex_autocov_y1.pdf"),
  autocov_y1,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/ex_autocov_ts_y2.pdf"),
  ts_y2,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/ex_autocov_y2.pdf"),
  autocov_y2,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/ex_autocov_ts_y3.pdf"),
  ts_y3,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/ex_autocov_y3.pdf"),
  autocov_y3,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/ex_autocov_ts_y3_resid.pdf"),
  ts_y3_resid,
  width = 8,
  height = 4
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/ex_autocov_y3_resid.pdf"),
  autocov_y3_resid,
  width = 8,
  height = 4
)
