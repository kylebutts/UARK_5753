# %%
library(tidyverse)
library(ISLR)
library(ISLR2)
library(here)
library(kfbmisc)
library(patchwork)
library(fixest)
library(FNN)
library(tinytable)

# Following Ed Rubin's Machine Learning slides
# https://github.com/edrubin/EC607S20?tab=readme-ov-file

# Generate data
n <- 1e3
set.seed(123)
dgp <- function(x) {
  y <- 0.3 * x^2 - 17
  y <- ifelse(y > 2.5, y, 5 - x)
  y <- abs(y)^0.7
  return(y)
}

tmp_df <- tibble(
  x = runif(n = n, min = -10, max = 10),
  er = rnorm(n = n, sd = 1.5),
  y_systematic = dgp(x),
  y = y_systematic + er
)
bins_10_x <- c(-10, quantile(tmp_df$x, seq(0.1, 0.9, by = 0.1)), 10)
tmp_df$x_bins_10 <- cut(tmp_df$x, bins_10_x, include.lowest = TRUE)

# Estimate
predictions <- tibble(
  x = seq(-10, 10, by = 0.01),
  x_bins_10 = cut(x, bins_10_x, include.lowest = TRUE)
)

predictions$y_hat_linear <-
  feols(y ~ x, tmp_df) |>
  predict(newdata = predictions)

predictions$y_hat_poly_4 <-
  feols(y ~ poly(x, 4), tmp_df) |>
  predict(newdata = predictions)

predictions$y_hat_poly_20 <-
  feols(y ~ poly(x, 20), tmp_df) |>
  predict(newdata = predictions)

predictions$y_hat_knn_100 <-
  knn.reg(
    train = as.matrix(tmp_df$x),
    test = as.matrix(predictions$x),
    y = as.matrix(tmp_df$y),
    k = 100
  )$pred

predictions$y_hat_knn_10 <-
  knn.reg(
    train = as.matrix(tmp_df$x),
    test = as.matrix(predictions$x),
    y = as.matrix(tmp_df$y),
    k = 5
  )$pred

predictions$y_hat_knn_5 <-
  knn.reg(
    train = as.matrix(tmp_df$x),
    test = as.matrix(predictions$x),
    y = as.matrix(tmp_df$y),
    k = 5
  )$pred

predictions$y_hat_bins_10 <- tmp_df |>
  feols(y ~ i(x_bins_10)) |>
  predict(newdata = predictions)

# %%
(plot_basic <-
  ggplot() +
  geom_point(
    aes(x, y),
    data = tmp_df,
    size = 1.5,
    shape = 19,
    alpha = 0.05
  ) +
  geom_point(
    aes(x, y),
    data = tmp_df,
    size = 1.5,
    shape = 1,
    alpha = 0.2
  ) +
  labs(x = NULL, y = NULL, title = "Examples of $\\hat{f}$: ") +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain")
  ))


title_parts <- c(
  "\\color[HTML]{5C4CBF}{Line}",
  "\\color[HTML]{FB7185}{Polynomial $\\left( x^4 \\right)$}",
  "\\color[HTML]{0188AC}{Bins of $x$}",
  "\\color[HTML]{2DB25F}{KNN of $x$}"
)

(plot_pred_1 <- plot_basic +
  geom_line(
    aes(x = x, y = y_hat_linear),
    data = predictions,
    linewidth = 1.5,
    color = kfbmisc::kyle_color("purple")
  ) +
  labs(
    title = paste0(
      "Examples of $\\hat{f}$: ",
      paste(title_parts[1], collapse = ", ")
    )
  ))

(plot_pred_2 <- plot_pred_1 +
  geom_line(
    aes(x = x, y = y_hat_poly_4),
    data = predictions,
    linewidth = 1.5,
    color = kfbmisc::kyle_color("rose")
  ) +
  # geom_line(
  #   aes(x = x, y = y_hat_poly_20),
  #   data = predictions, linewidth = 1.5,
  #   color = kfbmisc::kyle_color("magenta")
  # ) +
  labs(
    title = paste0(
      "Examples of $\\hat{f}$: ",
      paste(title_parts[1:2], collapse = ", ")
    )
  ))

(plot_pred_3 <- plot_pred_2 +
  geom_line(
    aes(x = x, y = y_hat_bins_10, group = x_bins_10),
    data = predictions,
    linewidth = 1.5,
    color = kfbmisc::kyle_color("blue")
  ) +
  labs(
    title = paste0(
      "Examples of $\\hat{f}$: ",
      paste(title_parts[1:3], collapse = ", ")
    )
  ))

(plot_pred_4 <- plot_pred_3 +
  geom_line(
    aes(x = x, y = y_hat_knn_100),
    data = predictions,
    linewidth = 1.5,
    color = kfbmisc::kyle_color("green")
  ) +
  labs(
    title = paste0(
      "Examples of $\\hat{f}$: ",
      paste(title_parts[1:4], collapse = ", ")
    )
  ))

# %%
(plot_dgp <-
  ggplot() +
  geom_point(
    aes(x, y),
    data = tmp_df,
    size = 1.5,
    shape = 19,
    alpha = 0.05
  ) +
  geom_point(
    aes(x, y),
    data = tmp_df,
    size = 1.5,
    shape = 1,
    alpha = 0.2
  ) +
  geom_line(
    aes(x = x, y = y_systematic),
    data = tmp_df |> arrange(x),
    color = kfbmisc::tailwind_color("zinc800"),
    linewidth = 1
  ) +
  labs(x = NULL, y = NULL, title = "\\color[HTML]{27272a}{True $f(x)$}") +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain")
  ))

# %%
title_parts <- c(
  "\\color[HTML]{27272a}{True $f(x)$}",
  "\\color[HTML]{5C4CBF}{Line}",
  "\\color[HTML]{ffc517}{Somewhat flexible}",
  "\\color[HTML]{0188AC}{Highly flexible}"
)

(plot_overfitting <- ggplot() +
  geom_point(
    aes(x, y),
    data = tmp_df,
    size = 1.5,
    shape = 19,
    alpha = 0.05
  ) +
  geom_point(
    aes(x, y),
    data = tmp_df,
    size = 1.5,
    shape = 1,
    alpha = 0.2
  ) +
  geom_line(
    aes(x = x, y = y_systematic),
    data = tmp_df |> arrange(x),
    color = kfbmisc::tailwind_color("zinc800"),
    linewidth = 1,
    alpha = 0.6
  ) +
  labs(x = NULL, y = NULL, title = " ") +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain")
  ) +
  geom_line(
    aes(x = x, y = y_hat_linear),
    data = predictions,
    linewidth = 1,
    alpha = 0.6,
    color = kfbmisc::kyle_color("purple")
  ) +
  geom_line(
    aes(x = x, y = y_hat_knn_100),
    data = predictions,
    linewidth = 1,
    alpha = 0.6,
    color = kfbmisc::kyle_color("yellow")
  ) +
  geom_line(
    aes(x = x, y = y_hat_knn_5),
    data = predictions,
    linewidth = 1,
    alpha = 0.6,
    color = kfbmisc::kyle_color("blue")
  ) +
  labs(
    title = paste0("", paste(title_parts, collapse = ", "))
  ))


# %%
kfbmisc::tikzsave(
  here("02-Forecasting/figures/f_examples_plot_raw.pdf"),
  plot = plot_basic,
  width = 8,
  height = 3.8
)

kfbmisc::tikzsave(
  here("02-Forecasting/figures/f_examples_plot_pred_1.pdf"),
  plot = plot_pred_1,
  width = 8,
  height = 3.8
)
kfbmisc::tikzsave(
  here("02-Forecasting/figures/f_examples_plot_pred_2.pdf"),
  plot = plot_pred_2,
  width = 8,
  height = 3.8
)
kfbmisc::tikzsave(
  here("02-Forecasting/figures/f_examples_plot_pred_3.pdf"),
  plot = plot_pred_3,
  width = 8,
  height = 3.8
)
kfbmisc::tikzsave(
  here("02-Forecasting/figures/f_examples_plot_pred_4.pdf"),
  plot = plot_pred_4,
  width = 8,
  height = 3.8
)

kfbmisc::tikzsave(
  here("02-Forecasting/figures/f_examples_plot_dgp.pdf"),
  plot = plot_dgp,
  width = 8,
  height = 3.8
)
kfbmisc::tikzsave(
  here("02-Forecasting/figures/f_examples_overfitting.pdf"),
  plot = plot_overfitting,
  width = 8,
  height = 3.8
)
