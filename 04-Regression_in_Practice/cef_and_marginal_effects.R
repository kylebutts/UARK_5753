# %% 
library(tidyverse)
library(here)
library(kfbmisc)
library(patchwork)
set.seed(20240210)

pkgs <- system.file(c("tikzsave/paper.sty", "tikzsave/math.sty"), package = "kfbmisc")
options(tikzMetricPackages = c(
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[T1]{fontenc}",
  "\\usetikzlibrary{calc}",
  sprintf("\\usepackage{%s}", xfun::sans_ext(pkgs))
))

# `with` + tidy-evaluation
# with_data <- function(data, expr) {
#   quo <- rlang::enquo(expr)
#   rlang::eval_tidy(quo, data)
# }
# df |> 
#   with_data(.data$age)

# %% 
# Linear CEF
N <- 250
age <- pmin(pmax(rnorm(n = N, mean = 38, sd = 13), 22), 67)
age <- round(age)
# college <- sample(c(0, 1), size = N, prob = c(0.7, 0.3), replace = TRUE)

linear_cef <- function(age) {
  Ey <- 10 + 0.3 * age
  return(Ey)
}
D_linear_cef <- function(age) {
  d <- 0.3
  return(d)
}
wage <- linear_cef(age) + rnorm(N, mean = 0, sd = 2)
# plot(age, wage)

df <- data.frame(
  age = age,
  wage = wage
)

(plot_linear_cef <- ggplot() + 
  geom_point(
    aes(x = age, y = wage),
    data = df, 
    shape = 21, size = 2.5,
    color = kfbmisc::tailwind_color("zinc-600")
  ) + 
  stat_function(
    fun = linear_cef,
    linewidth = 2, 
    color = kfbmisc::kyle_color("blue")
  ) + 
  annotate("label",
    x = 66,
    y = linear_cef(65) - 5,
    hjust = 1,
    color = kfbmisc::kyle_color("blue"),
    label = "$\\expec{w_i}{\\text{Age}_i}$",
    size = 5, label.size = NA, fill = NA
  ) +
  scale_x_continuous(limits = c(22, 67), expand = c(0, 1)) +
  labs(x = NULL, y = "$w_i$") +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v")
)

(plot_marginal_effect <- ggplot() + 
  stat_function(
    fun = D_linear_cef,
    linewidth = 2, 
    color = kfbmisc::kyle_color("blue")
  ) + 
  annotate("label",
    x = 55,
    y = D_linear_cef(62) + 0.2,
    hjust = 0,
    color = kfbmisc::kyle_color("blue"),
    label = "$\\frac{\\partial}{\\partial \\text{age}}\\expec{w_i}{\\text{Age}_i}$",
    size = 5, label.size = NA, fill = NA
  ) +
  scale_x_continuous(limits = c(22, 67), expand = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = NULL, y = "Marginal Effect") +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v")
)

(plot_combined <- (plot_linear_cef / plot_marginal_effect) + 
  plot_layout(heights = c(1, 0.5))
)

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/ex_linear_cef"),
  plot_combined, width = 8, height = 5
)



# %% 
# Quadratic CEF
N <- 250
age <- pmin(pmax(rnorm(n = N, mean = 38, sd = 13), 22), 67)
age <- round(age)
# college <- sample(c(0, 1), size = N, prob = c(0.7, 0.3), replace = TRUE)

quadratic_cef <- function(age) {
  Ey <- -30 + 3 * age - 1.5/45 * age^2
  return(Ey)
}
D_quadratic_cef <- function(age) {
  d <- 3 - 3/45 * age
  return(d)
}
wage <- quadratic_cef(age) + rnorm(N, mean = 0, sd = 2)
# plot(age, wage)

df <- data.frame(
  age = age,
  wage = wage
)

(plot_quadratic_cef <- ggplot() + 
  geom_point(
    aes(x = age, y = wage),
    data = df, 
    shape = 21, size = 2.5,
    color = kfbmisc::tailwind_color("zinc-600")
  ) + 
  stat_function(
    fun = quadratic_cef,
    linewidth = 2, 
    color = kfbmisc::kyle_color("blue")
  ) + 
  annotate("label",
    x = 63,
    y = quadratic_cef(65) - 2,
    hjust = 1,
    color = kfbmisc::kyle_color("blue"),
    label = "$\\expec{w_i}{\\text{Age}_i}$",
    size = 5, label.size = NA, fill = NA
  ) +
  scale_x_continuous(limits = c(22, 67), expand = c(0, 1)) +
  labs(x = NULL, y = "$w_i$") +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v")
)

(plot_marginal_effect <- ggplot() + 
  stat_function(
    fun = D_quadratic_cef,
    linewidth = 2, 
    color = kfbmisc::kyle_color("blue")
  ) + 
  annotate("label",
    x = 55,
    y = D_quadratic_cef(62) + 1,
    hjust = 0,
    color = kfbmisc::kyle_color("blue"),
    label = "$\\frac{\\partial}{\\partial \\text{age}}\\expec{w_i}{\\text{Age}_i}$",
    size = 5, label.size = NA, fill = NA
  ) +
  scale_x_continuous(limits = c(22, 67), expand = c(0, 1)) +
  labs(x = NULL, y = "Marginal Effect") +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v")
)

(plot_combined <- (plot_quadratic_cef / plot_marginal_effect) + 
  plot_layout(heights = c(1, 0.5))
)

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/ex_quadratic_cef"),
  plot_combined, width = 8, height = 5
)

# %% 
# `plogis` CEF
N <- 250
age <- pmin(pmax(rnorm(n = N, mean = 38, sd = 13), 22), 67)
age <- round(age)
# college <- sample(c(0, 1), size = N, prob = c(0.7, 0.3), replace = TRUE)

plogis_cef <- function(age) {
  Ey <- 10 + 30 * plogis(-1 + 0.5 * (age - 44.5) / 3)

  return(Ey)
  # %% 
  # library(marginaleffects)
  # set.seed(48103)
  # N <- 250
  # X <- rnorm(N, sd = 2)
  # p <- plogis(-1 + 0.5 * X)
  # Y <- rbinom(N, 1, p)
  # df <- data.frame(y = p, x = X)
  # plot(df$x, df$y)
}
D_plogis_cef <- function(age) {
  d <- 30 * 0.5 / 3 * dlogis(-1 + 0.5 * (age - 44.5) / 3)
  return(d)
}
wage <- plogis_cef(age) + rnorm(N, mean = 0, sd = 2)
# plot(age, wage)

df <- data.frame(
  age = age,
  wage = wage
)

(plot_plogis_cef <- ggplot() + 
  geom_point(
    aes(x = age, y = wage),
    data = df, 
    shape = 21, size = 2.5,
    color = kfbmisc::tailwind_color("zinc-600")
  ) + 
  stat_function(
    fun = plogis_cef,
    linewidth = 2, 
    color = kfbmisc::kyle_color("blue")
  ) + 
  annotate("label",
    x = 63,
    y = plogis_cef(65) + 1,
    hjust = 1,
    color = kfbmisc::kyle_color("blue"),
    label = "$\\expec{w_i}{\\text{Age}_i}$",
    size = 5, label.size = NA, fill = NA
  ) +
  scale_x_continuous(limits = c(22, 67), expand = c(0, 1)) +
  labs(x = NULL, y = "$w_i$") +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v")
)

(plot_marginal_effect <- ggplot() + 
  stat_function(
    fun = D_plogis_cef,
    linewidth = 2, 
    color = kfbmisc::kyle_color("blue")
  ) + 
  annotate("label",
    x = 57,
    y = D_plogis_cef(62) + 0.6,
    hjust = 0,
    color = kfbmisc::kyle_color("blue"),
    label = "$\\frac{\\partial}{\\partial \\text{age}}\\expec{w_i}{\\text{Age}_i}$",
    size = 5, label.size = NA, fill = NA
  ) +
  scale_x_continuous(limits = c(22, 67), expand = c(0, 1)) +
  labs(x = NULL, y = "Marginal Effect") +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v")
)

(plot_combined <- (plot_plogis_cef / plot_marginal_effect) + 
  plot_layout(heights = c(1, 0.5))
)

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/ex_plogis_cef"),
  plot_combined, width = 8, height = 5
)


