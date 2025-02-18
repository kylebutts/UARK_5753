# %%
library(tidyverse)
library(here)
library(fs)
library(fixest)
library(patchwork)

# $\log$-transformation
# %% 
beta_1 <- seq(-0.2, 0.2, by = 0.001)
exp_minus_1 <- exp(beta_1) - 1

(plot_log_linear_marginal_effect_appxoimation <- 
  ggplot() +
  geom_line(
    aes(x = beta_1, y = exp_minus_1, color = "exact"),
    linewidth = 2
  ) +
  geom_line(
    aes(x = beta_1, y = beta_1, color = "approx"),
    linewidth = 2
  ) +
  scale_color_manual(
    values = c("exact" = "#B3114B", "approx" = "#0188AC"), 
    labels = c(
      "exact" = "{\\color[HTML]{B3114B}$\\exp(\\beta_1) - 1$}", 
      "approx" = "{\\color[HTML]{0188AC}$\\beta_1$}"
    ),
    guide = guide_none()
  ) +
  labs(
    title = "Comparison of {\\color[HTML]{B3114B}$\\exp(\\beta_1) - 1$} and {\\color[HTML]{0188AC}$\\beta_1$} for a 1 unit change in $X$",
    x = "$\\beta_1$",
    y = "\\% Change in $Y$",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14)
)

# %% 
kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/log_linear_approximation.pdf"),
  plot_log_linear_marginal_effect_appxoimation,
  width = 8, height = 4.5
)

# %% 
# $\log$-$\log$ interpretation
# beta <- 0.5
beta_1 <- seq(-0.2, 0.2, by = 0.001)
version_true <- (1 + 0.01)^beta_1 - 1
version_approx <- beta_1 * 0.01

(plot_log_log_marginal_effect_approx <- 
  ggplot() +
  geom_line(
    aes(x = beta_1, y = version_true, color = "exact"),
    linewidth = 2
  ) +
  geom_line(
    aes(x = beta_1, y = version_approx, color = "approx"),
    linewidth = 2
  ) +
  scale_color_manual(
    values = c("exact" = "#B3114B", "approx" = "#0188AC"),
    guide = guide_none()
  ) +
  scale_y_continuous(labels = scales::label_percent(suffix = "\\%")) +
  labs(
    title = "Comparison of {\\color[HTML]{B3114B}$\\left(1 + \\frac{X_1 - X_0}{X_0} \\right)^{\\beta_1} - 1$} and {\\color[HTML]{0188AC}$\\beta_1 \\frac{X_1 - X_0}{X_0}$} for 1\\% increase in $X$",
    x = "$\\beta_1$",
    y = "\\% Change in $Y$",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14)
)

# %% 
kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/log_log_approximation.pdf"),
  plot_log_log_marginal_effect_approx,
  width = 8, height = 4.5
)



# %%
set.seed(20240922)
n <- 300

cond_exp <- function(sat_score) {
  15 * exp(
    -2.5 + 0.0035 * sat_score 
  )
}

sat_score = pmin(800, pmax(200, rnorm(n, 500, 100)))
wages = cond_exp(sat_score) * exp(rnorm(n, 0, sd = 0.05))
df = data.frame(sat_score = sat_score, wages = wages)

(plot_wage_sat_raw <- ggplot(data = df) +
  geom_point(
    aes(x = sat_score, y = wages), 
    size = 3, color = "gray30", alpha = 0.5
  ) +
  labs(
    # title = "{True \\color[HTML]{FB7185}$w = \\log(\\text{SAT})$} vs. {\\color[HTML]{FFC517} Linear Approximation}",
    title = "Data on SAT score and wages",
    x = "SAT Score", 
    y = "Wages"
  ) + 
  kfbmisc::theme_kyle(base_size = 14))

(plot_wage_sat_log_vs_linear <- ggplot(data = df) +
  geom_point(
    aes(x = sat_score, y = wages), 
    size = 2, color = "gray30", alpha = 0.5
  ) +
  geom_function(
    fun = cond_exp, 
    color = kfbmisc::kyle_colors["rose"],
    linewidth = 2
  ) + 
  geom_smooth(
    aes(x = sat_score, y = wages), data = df, 
    formula = y ~ x, method = "lm", se = FALSE,
    color = kfbmisc::kyle_colors["yellow"],
    linewidth = 2
  ) + 
  labs(
    # title = "{True \\color[HTML]{FB7185}$w = \\log(\\text{SAT})$} vs. {\\color[HTML]{FFC517} Linear Approximation}",
    title = "{\\color[HTML]{FB7185}True $\\log$-linear CEF} vs. {\\color[HTML]{FFC517} Linear Approximation}",
    x = "SAT Score", 
    y = "Wages"
  ) + 
  kfbmisc::theme_kyle(base_size = 14)
)

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/ex_log_y_raw.pdf"),
  plot_wage_sat_raw,
  width = 8, height = 4.5
)

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/ex_log_y_log_vs_linear.pdf"),
  plot_wage_sat_log_vs_linear,
  width = 8, height = 4.5
)

