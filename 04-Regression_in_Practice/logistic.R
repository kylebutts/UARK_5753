# %%
library(tidyverse)
library(here)
library(fs)
library(fixest)
library(patchwork)


tikzDevice::setTikzDefaults()
default_packages <- getOption("tikzLatexPackages")
packages <- c(
  system.file("tikzsave/paper.sty", package = "kfbmisc"), 
  system.file("tikzsave/math.sty", package = "kfbmisc")
)
pkg_tex <- sprintf("\\usepackage{%s}", fs::path_ext_remove(packages))
options("tikzLatexPackages" = c(default_packages, "\\usepackage{bm}\n", pkg_tex))


# logistic function
# %% 
logistic <- function(x) exp(x) / (exp(x) + 1)
x <- seq(-8, 8, by = 0.01)
logistic_x <- logistic(x)
d_dx_logistic_x <- (logistic(x + 1e-8) - logistic(x)) / 1e-8

(plot_logistic_link <- 
  ggplot() +
  geom_line(
    aes(x = x, y = logistic_x),
    color = kfbmisc::kyle_color("green"),
    linewidth = 2
  ) +
  labs(
    x = "Index (${\\color[HTML]{5C4CBF} W_i' \\beta}$)",
    y = NULL,
    title = "Logistic transformation: ${\\color[HTML]{2DB25F} \\textrm{logistic}}\\left( {\\color[HTML]{5C4CBF} W_i' \\beta} \\right) = \\frac{e^{{\\color[HTML]{5C4CBF} W_i' \\beta}}}{e^{{\\color[HTML]{5C4CBF} W_i' \\beta}} + 1}$",
  ) +
  kfbmisc::theme_kyle(base_size = 14)
)

# %% 
kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/logistic_link_function.pdf"),
  plot_logistic_link, width = 8, height = 4.5
)

# %% 
(plot_logistic_link <- plot_logistic_link + 
  labs(
    title = NULL,
    y = "$\\prob{Y_i = 1}$"
  )
)
(plot_d_dx_logistic_link <- 
  ggplot() +
  geom_line(
    aes(x = x, y = d_dx_logistic_x),
    color = kfbmisc::kyle_color("blue"),
    linewidth = 2
  ) +
  labs(
    x = "Index (${\\color[HTML]{5C4CBF} W_i' \\beta}$)",
    y = "Marginal Effect"
  ) +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v")
)

(plot_combined <- (plot_logistic_link / plot_d_dx_logistic_link) + 
  plot_layout(heights = c(1, 0.5))
)

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/logistic_marginal_effect.pdf"),
  plot_combined, width = 8, height = 5
)


# %% 
est <- feglm(am ~ mpg, data = mtcars, family = "logit")
prediction_grid <- data.frame(mpg = seq(10, 35, by = 0.01)) 
prediction_grid$prob_am <- predict(est, newdata = prediction_grid)

(plot_forecast_mtcars <- ggplot() +
  geom_point(
    aes(x = mpg, y = am), 
    data = mtcars,
    shape = 21
  ) + 
  geom_line(
    aes(x = mpg, y = prob_am), 
    data = prediction_grid,
    linewidth = 1.2,
    color = kfbmisc::kyle_color("green")
  ) + 
  labs(
    x = "Miles-per-gallon",
    y = "Estimated $\\prob{\\text{Automatic}_i = 1}{\\text{MPG}_i}$"
  ) +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "v")
)

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/mtcars_forecast.pdf"),
  plot_forecast_mtcars, width = 8, height = 5
)


