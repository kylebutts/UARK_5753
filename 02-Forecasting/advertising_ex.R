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

advertising <- read_csv(here("02-Forecasting/data/Advertising.csv"))

# %%
plot_sales_vs_tv <- advertising |>
  ggplot(aes(x = TV, y = Sales)) +
  geom_point(shape = 21) +
  geom_smooth(
    formula = y ~ x,
    method = stats::lm,
    se = FALSE,
    color = "#5601A5"
  ) +
  labs(x = "TV Spend (\\$)") + 
  kfbmisc::theme_kyle(base_size = 12)

plot_sales_vs_radio <- advertising |>
  ggplot(aes(x = Radio, y = Sales)) +
  geom_point(shape = 21) +
  geom_smooth(
    formula = y ~ x,
    method = stats::lm,
    se = FALSE,
    color = "#5601A5"
  ) +
  labs(x = "Radio Spend (\\$)") + 
  kfbmisc::theme_kyle(base_size = 12)

plot_sales_vs_news <- advertising |>
  ggplot(aes(x = Newspaper, y = Sales)) +
  geom_point(shape = 21) +
  geom_smooth(
    formula = y ~ x,
    method = stats::lm,
    se = FALSE,
    color = "#5601A5"
  ) +
  labs(x = "Newspaper Spend (\\$)") + 
  kfbmisc::theme_kyle(base_size = 12)

plot_sales_bivariate <-
  (plot_sales_vs_tv | plot_sales_vs_radio | plot_sales_vs_news) +
  plot_layout(axes = "collect")

kfbmisc::tikzsave(
  here("02-Forecasting/figures/sales_bivariate.pdf"),
  plot_sales_bivariate,
  width = 8, height = 3.5
)

#' Highlighting error term $\varepsilon$
# %%
est_tv <- feols(Sales ~ TV, advertising)
advertising$Sales_hat <- predict(est_tv)

(plot_sales_tv_residual <- ggplot(data = advertising) +
  geom_abline(
    intercept = coef(est_tv)["(Intercept)"],
    slope = coef(est_tv)["TV"],
    color = "#5601A5"
  ) +
  geom_point(
    aes(x = TV, y = Sales),
    shape = 21
  ) +
  geom_segment(
    aes(x = TV, xend = TV, y = Sales, yend = Sales_hat),
    color = "#b84242"
  ) +
  ggtext::geom_textbox(
    x = 60, y = 22.5, color = "#b84242",
    label = "Error term, $\\hat{\\varepsilon}$, is difference between model $\\hat{f}(X)$ and observed $Y$",
    box.r = unit(0, "in"),
    width = unit(2.5, "in")
  ) +
  kfbmisc::theme_kyle(base_size = 12)
)

kfbmisc::tikzsave(
  here("02-Forecasting/figures/sales_tv_error_term.pdf"),
  plot_sales_tv_residual,
  width = 8, height = 4
)
