# %%
library(tidyverse)
library(here)
library(fs)
library(fixest)
library(patchwork)

#' # Regression Inference
# %%
dgp <- function(n = 100, sd_eps = sqrt(1.5)) {
  df <- tibble(
    x = rnorm(n, 1, sd = 1)
  )
  df$eps <- rnorm(nrow(df), 0, sd = sd_eps)
  df$y <- df$x * 1 + df$eps
  return(df)
}
gen_reg <- function(n = 100) {
  df <- dgp(n = n)
  coef(feols(y ~ x, data = df))
}
coef_to_ggplot_line <- function(coef, alpha = 0.05) {
  geom_abline(
    intercept = coef["(Intercept)"],
    slope = coef["x"],
    linewidth = 1.2, 
    color = kfbmisc::kyle_color("purple"),
    alpha = alpha
  )
}

set.seed(20240915)
df <- dgp(n = 100)
fit <- coef(feols(y ~ x, data = df))

reg_samples <- lapply(1:2500, function(i) {
  gen_reg(n = 100)
})
beta_1s <- unlist(map(reg_samples, \(coef) coef[["x"]]))

eval_x <- 1.5
y_0 <- 1.5
forecasts <- unlist(lapply(reg_samples, function(coef) coef[["(Intercept)"]] + coef[["x"]] * eval_x))


# %% 
# \var{\hat{\beta}}
100 * var(do.call(
  "rbind", reg_samples
))

W = cbind(1, df$x)
Sigma = 1.5 * diag(nrow = 1000)
crossprod(W) / 1000
(t(W) %*% Sigma %*% W) / 1000
vcov = solve(t(W) %*% W) %*% 
  (t(W) %*% Sigma %*% W) %*% 
  solve(t(W) %*% W)

std_errors = sqrt(diag(vcov))

# %%
(plot_orig_reg <- ggplot() +
  geom_point(
    aes(x = x, y = y),
    data = df,
    shape = 1
  ) +
  geom_abline(
    intercept = fit["(Intercept)"],
    slope = fit["x"],
    linewidth = 1.2, color = kfbmisc::kyle_color("purple")
  ) +
  labs(
    title = "Original Sample",
    x = NULL, y = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain", margin = margin(b = 10))
  ))

plot_extra_samples_1 <- plot_orig_reg +
  coef_to_ggplot_line(reg_samples[[1]], alpha = 0.25) +
  labs(title = "Original Sample + 1 Extra Sample")

plot_extra_samples_2 <- plot_orig_reg +
  coef_to_ggplot_line(reg_samples[[1]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[2]], alpha = 0.25) +
  labs(title = "Original Sample + 2 Extra Samples")

(plot_extra_samples_3 <- plot_orig_reg +
  coef_to_ggplot_line(reg_samples[[1]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[2]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[3]], alpha = 0.25) +
  labs(title = "Original Sample + 3 Extra Samples"))

(plot_extra_samples_5 <- plot_orig_reg +
  coef_to_ggplot_line(reg_samples[[1]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[2]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[3]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[4]], alpha = 0.25) +
  coef_to_ggplot_line(reg_samples[[5]], alpha = 0.25) +
  labs(title = "Original Sample + 5 Extra Samples"))

ggplot_reg_lines <- lapply(reg_samples, function(coef) coef_to_ggplot_line(coef, alpha = 0.25))

(plot_extra_samples_100 <- plot_orig_reg +
  ggplot_reg_lines[1:100] +
  labs(title = "Original Sample + 100 Extra Samples"))

(plot_sample_distribution <-
  ggplot() +
  geom_histogram(
    aes(x = beta_1s),
    bins = 60,
    fill = kfbmisc::kyle_color("purple")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "$\\hat{\\beta}_1$ for sample $b$",
    y = "Count",
    title = "Original Sample + 2500 Extra Samples"
  ) +
  kfbmisc::theme_kyle(base_size = 14)
)

(plot_forecasts <-  ggplot() +
  geom_histogram(
    aes(x = forecasts),
    bins = 60,
    fill = kfbmisc::kyle_color("purple")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "$\\hat{\\beta}_0 + 1.5 * \\hat{\\beta}_1$ for sample $b$",
    y = "Count",
    title = "Original Sample + 2500 Extra Samples"
  ) +
  kfbmisc::theme_kyle(base_size = 14)
)

# %%
kfbmisc::tikzsave(
  here("03-Regression/figures/ex_inference_orig_reg.pdf"),
  plot_orig_reg,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("03-Regression/figures/ex_inference_extra_sample_1.pdf"),
  plot_extra_samples_1,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("03-Regression/figures/ex_inference_extra_sample_2.pdf"),
  plot_extra_samples_2,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("03-Regression/figures/ex_inference_extra_sample_5.pdf"),
  plot_extra_samples_5,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("03-Regression/figures/ex_inference_extra_sample_100.pdf"),
  plot_extra_samples_100,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("03-Regression/figures/ex_inference_sample_distribution.pdf"),
  plot_sample_distribution,
  width = 8, height = 4
)
kfbmisc::tikzsave(
  here("03-Regression/figures/ex_inference_forecasts.pdf"),
  plot_forecasts,
  width = 8, height = 4
)
