library(tidyverse)
library(here)
library(kfbmisc)
library(patchwork)

fs::dir_create(here("01-Linear_Algebra/figures/"))
pkgs <- system.file(c("tikzsave/paper.sty", "tikzsave/math.sty"), package = "kfbmisc")
options(tikzMetricPackages = c(
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[T1]{fontenc}",
  "\\usetikzlibrary{calc}",
  sprintf("\\usepackage{%s}", xfun::sans_ext(pkgs))
))


## Examples of normal distribution ---------------------------------------------
(plot_ex_normal_dist <- ggplot() +
  stat_function(
    fun = function(x) dnorm(x, mean = 0, sd = 1),
    color = kfbmisc::kyle_color("green"), linewidth = 1.5, n = 300
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = 3, sd = 1),
    color = kfbmisc::kyle_color("purple"), linewidth = 1.5, n = 300
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = 0, sd = 2),
    color = kfbmisc::kyle_color("magenta"), linewidth = 1.5, n = 300
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = 0, sd = 3),
    color = kfbmisc::kyle_color("rose"), linewidth = 1.5, n = 300
  ) +
  scale_x_continuous(
    limits = c(-10, 10), breaks = seq(-10, 10, by = 2), expand = expansion(0, 0)
  ) +
  scale_y_continuous(limits = c(0, 0.5), expand = expansion(0, 0)) +
  labs(
    y = "Density",
    title = "PDFs: {\\color[HTML]{2DB25F} $Z = \\mathcal{N}(0, 1)$}, {\\color[HTML]{5C4CBF} $\\mathcal{N}(3, 1)$}, {\\color[HTML]{B3114B} $\\mathcal{N}(0, 4)$}, {\\color[HTML]{FB7185} $\\mathcal{N}(0, 9)$}"
  ) +
  theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "plain", size = rel(1 / 1.125),
      margin = margin(0, 0, 32, 0)
    ),
  )
)


kfbmisc::tikzsave(
  here("01-Linear_Algebra/figures/ex_normal_dist.pdf"),
  plot = plot_ex_normal_dist, width = 8, height = 3.8
)


## Multivariate-normal ---------------------------------------------------------
library(mvtnorm)

Sigma_independent <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
draws_independent <- mvtnorm::rmvnorm(
  n = 1000000,
  sigma = Sigma_independent
)
draws_independent <- draws_independent |>
  as_tibble() |>
  setNames(c("x1", "x2"))

Sigma_weak_pos_corr <- matrix(c(1, 0.25, 0.25, 1), nrow = 2, byrow = TRUE)
draws_weak_pos_corr <- mvtnorm::rmvnorm(
  n = 1000000,
  sigma = Sigma_weak_pos_corr
)
draws_weak_pos_corr <- draws_weak_pos_corr |>
  as_tibble() |>
  setNames(c("x1", "x2"))

Sigma_strong_neg_corr <- matrix(c(1, -0.75, -0.75, 1), nrow = 2, byrow = TRUE)
draws_strong_neg_corr <- mvtnorm::rmvnorm(
  n = 1000000,
  sigma = Sigma_strong_neg_corr
)
draws_strong_neg_corr <- draws_strong_neg_corr |>
  as_tibble() |>
  setNames(c("x1", "x2"))


(heatmap_independent <- ggplot() +
  geom_bin_2d(
    aes(x = x1, y = x2),
    data = draws_independent,
    bins = 50
  ) +
  labs(
    x = NULL, y = NULL,
    title = "$\\begin{bmatrix}x_1 \\\\ x_2\\end{bmatrix} \\sim \\mathcal{N}\\left(\\begin{bmatrix}0 \\\\ 0\\end{bmatrix}, \\begin{bmatrix}1 & 0 \\\\ 0 & 1\\end{bmatrix}\\right)$"
  ) +
  guides(fill = guide_none()) +
  scale_fill_viridis_c(option = "inferno") +
  coord_fixed(xlim = c(-5, 5), ylim = c(-5, 5)) +
  theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "plain", size = rel(1 / 1.125),
      margin = margin(16, 0, 32, 0), hjust = 0.5
    ),
  )
)
kfbmisc::tikzsave(
  here("01-Linear_Algebra/figures/mvnorm_heatmap_independent.pdf"),
  plot = heatmap_independent, width = 5, height = 5.5
)


(heatmap_weak_pos_corr <- ggplot() +
  geom_bin_2d(
    aes(x = x1, y = x2),
    data = draws_weak_pos_corr,
    bins = 50
  ) +
  labs(
    x = NULL, y = NULL,
    title = "$\\begin{bmatrix}x_1 \\\\ x_2\\end{bmatrix} \\sim \\mathcal{N}\\left(\\begin{bmatrix}0 \\\\ 0\\end{bmatrix}, \\begin{bmatrix}1 & 0.25 \\\\ 0.25 & 1\\end{bmatrix}\\right)$"
  ) +
  guides(fill = guide_none()) +
  scale_fill_viridis_c(option = "inferno") +
  coord_fixed(xlim = c(-5, 5), ylim = c(-5, 5)) +
  theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "plain", size = rel(1 / 1.125),
      margin = margin(16, 0, 32, 0), hjust = 0.5
    ),
  )
)
kfbmisc::tikzsave(
  here("01-Linear_Algebra/figures/mvnorm_heatmap_weak_pos_corr.pdf"),
  plot = heatmap_weak_pos_corr, width = 5, height = 5.5
)


(heatmap_strong_neg_corr <- ggplot() +
  geom_bin_2d(
    aes(x = x1, y = x2),
    data = draws_strong_neg_corr,
    bins = 50
  ) +
  labs(
    x = NULL, y = NULL,
    title = "$\\begin{bmatrix}x_1 \\\\ x_2\\end{bmatrix} \\sim \\mathcal{N}\\left(\\begin{bmatrix}0 \\\\ 0\\end{bmatrix}, \\begin{bmatrix}1 & -0.75 \\\\ -0.75 & 1\\end{bmatrix}\\right)$"
  ) +
  guides(fill = guide_none()) +
  scale_fill_viridis_c(option = "inferno") +
  coord_fixed(xlim = c(-5, 5), ylim = c(-5, 5)) +
  theme_kyle(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "plain", size = rel(1 / 1.125),
      margin = margin(16, 0, 32, 0), hjust = 0.5
    ),
  )
)
kfbmisc::tikzsave(
  here("01-Linear_Algebra/figures/mvnorm_heatmap_strong_neg_corr.pdf"),
  plot = heatmap_strong_neg_corr, width = 5, height = 5.5
)
