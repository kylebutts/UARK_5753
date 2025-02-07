library(tidyverse)
library(here)
library(kfbmisc)

# Setup TikzDevice
tikzDevice::setTikzDefaults()
default_packages <- getOption("tikzLatexPackages")
packages <- c(system.file("tikzsave/paper.sty", package = "kfbmisc"), system.file("tikzsave/math.sty", package = "kfbmisc"))
pkg_tex <- sprintf("\\usepackage{%s}", fs::path_ext_remove(packages))
options("tikzLatexPackages" = c(default_packages, "\\usepackage{bm}\n", pkg_tex))

#
set.seed(20240207)
sample_means <- map_dbl(1:2500, function(b) {
  x <- rnorm(n = 50, mean = 3, sd = 1)
  return(mean(x))
})
# E(x) = 10 * p
mean_sample_means <- 3
# \sigma_x / sqrt(n)
sd_sample_means <- sqrt(1) / sqrt(50)

(sample_dist <- ggplot() +
  geom_histogram(
    aes(x = sample_means, y = after_stat(density)),
    bins = 50,
    fill = kfbmisc::tailwind_color("zinc-300")
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_sample_means, sd = sd_sample_means),
    color = kfbmisc::kyle_color("blue"), linewidth = 1.5, n = 300
  ) +
  geom_vline(
    xintercept = 3.0,
    color = kfbmisc::kyle_color("blue"),
    linetype = "dashed",
    linewidth = 1.5
  ) +
  annotate("label",
    x = mean_sample_means + 1 / 10 * sd_sample_means,
    y = dnorm(0, sd = sd_sample_means) * 1.1,
    hjust = 0,
    color = kfbmisc::kyle_color("blue"),
    label = "$\\mathbb{E}\\big[\\bar{X}_b\\big]$",
    size = 5, label.size = NA, fill = NA
  ) +
  annotate("segment",
    x = mean_sample_means,
    xend = mean_sample_means + 1 * sd_sample_means,
    y = dnorm(1 * sd_sample_means, sd = sd_sample_means),
    yend = dnorm(1 * sd_sample_means, sd = sd_sample_means),
    color = kfbmisc::kyle_color("blue"),
    linetype = "dashed",
    linewidth = 1.5
  ) +
  # annotate("label",
  #   x = mean_sample_means - 1 / 2 * sd_sample_means,
  #   y = dnorm(1 * sd_sample_means, sd = sd_sample_means),
  #   vjust = 1,
  #   color = kfbmisc::kyle_color("blue"),
  #   label = "$\\sigma_{\\bar{X}_b}$",
  #   size = 5, label.size = NA, fill = NA
  # ) +
  annotate("label",
    x = mean_sample_means + 1 / 2 * sd_sample_means,
    y = dnorm(1 * sd_sample_means, sd = sd_sample_means),
    vjust = 1,
    color = kfbmisc::kyle_color("blue"),
    label = "$\\sigma_{\\bar{X}_b}$",
    size = 5, label.size = NA, fill = NA
  ) +
  scale_x_continuous(
    breaks = round(3 + -3:3 * sd_sample_means, 2),
  ) +
  scale_y_continuous(
    breaks = c(),
    limits = c(0, dnorm(0, sd = sd_sample_means) * 1.1),
    expand = expansion(c(0, 0.1), c(0, 0))
  ) +
  labs(y = NULL, x = "Sample Distribution of $\\bar{X}_b$") +
  kfbmisc::theme_kyle(base_size = 14, grid_minor = "", grid = "")
)

kfbmisc::tikzsave(
  here("02-Forecasting/figures/sample_dist.pdf"),
  sample_dist, width = 5, height = 3.5
)

