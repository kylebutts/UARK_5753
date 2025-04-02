# %%
library(tidyverse)
library(fixest)
library(here)
library(kfbmisc)
library(patchwork)
set.seed(20240210)

# Setup TikzDevice
tikzDevice::setTikzDefaults()
default_packages <- getOption("tikzLatexPackages")
packages <- c(
  system.file("tikzsave/paper.sty", package = "kfbmisc"),
  system.file("tikzsave/math.sty", package = "kfbmisc")
)
pkg_tex <- sprintf("\\usepackage{%s}", fs::path_ext_remove(packages))
options(
  "tikzLatexPackages" = c(default_packages, "\\usepackage{bm}\n", pkg_tex)
)


# %%
# Linear CEF
N <- 50
age <- runif(n = N, min = 15, max = 24)
cef <- \(age) {
  0 + 0.5 * (age - 14) + 2 * (age > 18)
}
drinks_per_month <- cef(age) + rnorm(N, mean = 0, sd = 0.2)
drinks_per_month <- round(drinks_per_month * 5) / 5

df <- data.frame(
  age = age,
  drinks_per_month = drinks_per_month
)

predict_grid <- data.frame(age = seq(min(df$age), max(df$age), 0.05))

est_4 <- feols(drinks_per_month ~ poly(age, 4), data = df)
predict_grid$drinks_hat <- predict(est_4, newdata = predict_grid)

est_15 <- feols(drinks_per_month ~ poly(age, 15), data = df)
predict_grid$drinks_hat_15 <- predict(est_15, newdata = predict_grid)


(plot_poly_fit <- ggplot() +
  geom_point(
    aes(x = age, y = drinks_per_month),
    data = df,
    shape = 21,
    size = 2.5,
    color = kfbmisc::tailwind_color("zinc-600")
  ) +
  stat_function(
    fun = cef,
    linewidth = 2,
    color = kfbmisc::kyle_color("blue")
  ) +
  geom_line(
    aes(x = age, y = drinks_hat),
    data = predict_grid,
    linewidth = 2,
    color = kfbmisc::kyle_color("navy")
  ) +
  labs(
    x = "$\\text{Age}_i$",
    y = "$\\text{Drinks per Month}_i$",
    title = "{\\color[HTML]{0188AC} $\\expec{\\text{Drinks per Month}_i}{\\text{Age}_i}$}, {\\color[HTML]{002C55} 4th-order Polynomial fit}"
  ) +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v") +
  theme(
    plot.title = element_text(size = rel(1))
  ))

(plot_poly_fit_15 <- ggplot() +
  geom_point(
    aes(x = age, y = drinks_per_month),
    data = df,
    shape = 21,
    size = 2.5,
    color = kfbmisc::tailwind_color("zinc-600")
  ) +
  stat_function(
    fun = cef,
    linewidth = 2,
    color = kfbmisc::kyle_color("blue")
  ) +
  geom_line(
    aes(x = age, y = drinks_hat),
    data = predict_grid,
    linewidth = 2,
    color = kfbmisc::kyle_color("navy")
  ) +
  geom_line(
    aes(x = age, y = drinks_hat_15),
    data = predict_grid,
    linewidth = 2,
    color = kfbmisc::kyle_color("yellow")
  ) +
  labs(
    x = "$\\text{Age}_i$",
    y = "$\\text{Drinks per Month}_i$",
    title = "{\\color[HTML]{0188AC} $\\expec{\\text{Drinks per Month}_i}{\\text{Age}_i}$}, {\\color[HTML]{002C55} 4th-order Polynomial fit}, {\\color[HTML]{ffc517} 15th-order Polynomial fit}"
  ) +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v") +
  theme(
    plot.title = element_text(size = rel(1))
  ))


(plot_forecast_extended <- ggplot() +
  geom_point(
    aes(x = age, y = drinks_per_month),
    data = df,
    shape = 21,
    size = 3,
    color = kfbmisc::tailwind_color("zinc-600")
  ) +
  stat_function(
    fun = cef,
    linewidth = 2,
    color = kfbmisc::kyle_color("blue")
  ) +
  stat_function(
    fun = function(x) {
      predict(est_4, newdata = data.frame(age = x))
    },
    linewidth = 2,
    color = kfbmisc::kyle_color("navy")
  ) +
  # stat_function(
  #   fun = function(x) {
  #     predict(est_15, newdata = data.frame(age = x))
  #   },
  #   linewidth = 2,
  #   color = kfbmisc::kyle_color("yellow")
  # ) +
  labs(
    x = "$\\text{Age}_i$",
    y = "$\\text{Drinks per Month}_i$",
    title = "{\\color[HTML]{0188AC} $\\expec{\\text{Drinks per Month}_i}{\\text{Age}_i}$}, {\\color[HTML]{002C55} 4th-order Polynomial fit}"
  ) +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v") +
  scale_x_continuous(limits = c(13, 26)) +
  theme(
    plot.title = element_text(size = rel(1))
  ))

(plot_forecast_extended_15 <- ggplot() +
  geom_point(
    aes(x = age, y = drinks_per_month),
    data = df,
    shape = 21,
    size = 3,
    color = kfbmisc::tailwind_color("zinc-600")
  ) +
  stat_function(
    fun = cef,
    linewidth = 2,
    color = kfbmisc::kyle_color("blue")
  ) +
  stat_function(
    fun = function(x) {
      predict(est_4, newdata = data.frame(age = x))
    },
    linewidth = 2,
    color = kfbmisc::kyle_color("navy")
  ) +
  stat_function(
    fun = function(x) {
      predict(est_15, newdata = data.frame(age = x))
    },
    linewidth = 2,
    color = kfbmisc::kyle_color("yellow")
  ) +
  labs(
    x = "$\\text{Age}_i$",
    y = "$\\text{Drinks per Month}_i$",
    title = "{\\color[HTML]{0188AC} $\\expec{\\text{Drinks per Month}_i}{\\text{Age}_i}$}, {\\color[HTML]{002C55} 4th-order Polynomial fit}, {\\color[HTML]{ffc517} 15th-order Polynomial fit}"
  ) +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v") +
  scale_x_continuous(limits = c(13, 26)) +
  theme(
    plot.title = element_text(size = rel(1))
  ))

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/ex_jump_vs_polynomial.pdf"),
  plot_poly_fit,
  width = 8,
  height = 4.5
)

kfbmisc::tikzsave(
  here(
    "04-Regression_in_Practice/figures/ex_jump_vs_polynomial_overfitting.pdf"
  ),
  plot_poly_fit_15,
  width = 8,
  height = 4.5
)

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/ex_extend_polynomial.pdf"),
  plot_forecast_extended,
  width = 8,
  height = 4.5
)

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/ex_extend_polynomial_15.pdf"),
  plot_forecast_extended_15,
  width = 8,
  height = 4.5
)
