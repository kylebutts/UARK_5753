# %%
library(tidyverse)
library(here)
library(kfbmisc)
fs::dir_create(here("00-Review_Probability_and_Statistics/figures/"))

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

## Examples of normal distribution ----
# %%
(plot_ex_normal_dist <- ggplot() +
  stat_function(
    fun = function(x) dnorm(x, mean = 0, sd = 1),
    color = kfbmisc::kyle_color("green"),
    linewidth = 1.5,
    n = 300
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = 3, sd = 1),
    color = kfbmisc::kyle_color("purple"),
    linewidth = 1.5,
    n = 300
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = 0, sd = 2),
    color = kfbmisc::kyle_color("blue"),
    linewidth = 1.5,
    n = 300
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = 0, sd = 3),
    color = kfbmisc::kyle_color("magenta"),
    linewidth = 1.5,
    n = 300
  ) +
  scale_x_continuous(
    limits = c(-10, 10),
    breaks = seq(-10, 10, by = 2),
    expand = expansion(0, 0)
  ) +
  scale_y_continuous(limits = c(0, 0.5), expand = expansion(0, 0)) +
  labs(
    y = "Density",
    title = "PDFs: {\\color[HTML]{2DB25F} $Z = \\mathcal{N}(0, 1)$}, {\\color[HTML]{0188AC} $\\mathcal{N}(3, 1)$}, {\\color[HTML]{0188AC} $\\mathcal{N}(0, 4)$}, {\\color[HTML]{B3114B} $\\mathcal{N}(0, 9)$}"
  ) +
  theme_kyle(base_size = 14) +
  theme(plot.title = element_text(face = "plain", margin = margin(b = 32, unit = "pt"))))


## Integrating density function ----
# %%
curr_density <- function(x) dnorm(x, mean = 10, sd = 2)
(plot_ex_probability_leq <- ggplot() +
  stat_function(
    fun = curr_density,
    geom = "area",
    linewidth = 0,
    fill = kfbmisc::kyle_color("purple"),
    alpha = 0.3,
    xlim = c(0, 12),
    n = 300
  ) +
  stat_function(
    fun = curr_density,
    color = kfbmisc::kyle_color("purple"),
    linewidth = 1,
    n = 300
  ) +
  scale_x_continuous(
    limits = c(2, 18),
    expand = expansion(0, 0)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    y = NULL,
    x = NULL,
    title = r'( $\prob{X \leq 12}$ )'
  ) +
  theme_kyle(base_size = 14) +
   theme(plot.title = element_text(face = "plain", margin = margin(b = 32, unit = "pt"))))

# %%
curr_density <- function(x) dnorm(x, mean = 10, sd = 2)
(plot_ex_probability_between <- ggplot() +
  stat_function(
    fun = curr_density,
    geom = "area",
    linewidth = 0,
    fill = kfbmisc::kyle_color("purple"),
    alpha = 0.3,
    xlim = c(0, 12),
    n = 300
  ) +
  stat_function(
    fun = curr_density,
    geom = "area",
    linewidth = 0,
    fill = "red",
    alpha = 0.3,
    xlim = c(0, 9),
    n = 300
  ) +
  stat_function(
    fun = curr_density,
    color = kfbmisc::kyle_color("purple"),
    linewidth = 1,
    n = 300
  ) +
  scale_x_continuous(
    limits = c(2, 18),
    expand = expansion(0, 0),
    breaks = c(9, 12),
    labels = c("$\\underline{x}$", "$\\bar{x}$")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    y = NULL,
    x = NULL,
    title = r'( $\prob{\underline{x} \leq X \leq \bar{x}} = \prob{X \leq \bar{x}} - \prob{\underline{x} \leq X } $  )'
  ) +
  theme_kyle(base_size = 14) +
   theme(plot.title = element_text(face = "plain", margin = margin(b = 32, unit = "pt"))))

# %%
# from svnmiller
(plot_68_95_99_rule <- ggplot() +
  # +/- 1
  stat_function(
    fun = dnorm,
    xlim = c(-1, 1),
    geom = "area",
    fill = "#d2382c",
    alpha = 0.5
  ) +
  # +/- 2
  stat_function(
    fun = dnorm,
    xlim = c(-2, 2),
    geom = "area",
    fill = "#d2382c",
    alpha = 0.4
  ) +
  # +/- 3
  stat_function(
    fun = dnorm,
    xlim = c(-3, 3),
    geom = "area",
    fill = "#d2382c",
    alpha = 0.3
  ) +
  # Normal curve
  stat_function(
    fun = dnorm,
    color = kfbmisc::tailwind_color("zinc-600"),
    linewidth = 1.5
  ) +
  # 68%
  annotate(
    "label",
    x = 0,
    y = 0.44,
    size = 4,
    label = "$68\\%$",
    linewidth = 0
  ) +
  annotate(
    "path",
    x = c(-1, -1, -0.3),
    y = c(dnorm(-1), 0.44, 0.44),
    color = kfbmisc::tailwind_color("zinc-600"),
    linewidth = 1.2
  ) +
  annotate(
    "path",
    x = c(1, 1, 0.3),
    y = c(dnorm(-1), 0.44, 0.44),
    color = kfbmisc::tailwind_color("zinc-600"),
    linewidth = 1.2
  ) +
  # 95%
  annotate(
    "label",
    x = 0,
    y = 0.5,
    size = 4,
    label = "$95\\%$",
    linewidth = 0
  ) +
  annotate(
    "path",
    x = c(-2, -2, -0.3),
    y = c(dnorm(-2), 0.5, 0.5),
    color = kfbmisc::tailwind_color("zinc-600"),
    linewidth = 1.2
  ) +
  annotate(
    "path",
    x = c(2, 2, 0.3),
    y = c(dnorm(-2), 0.5, 0.5),
    color = kfbmisc::tailwind_color("zinc-600"),
    linewidth = 1.2
  ) +
  # 99.7%
  annotate(
    "label",
    x = 0,
    y = 0.56,
    size = 4,
    label = "$99.7\\%$",
    linewidth = 0
  ) +
  annotate(
    "path",
    x = c(-3, -3, -0.35),
    y = c(dnorm(-3), 0.56, 0.56),
    color = kfbmisc::tailwind_color("zinc-600"),
    linewidth = 1.2
  ) +
  annotate(
    "path",
    x = c(3, 3, 0.35),
    y = c(dnorm(-3), 0.56, 0.56),
    color = kfbmisc::tailwind_color("zinc-600"),
    linewidth = 1.2
  ) +
  #
  # scale_x_continuous(limits = c(-4, 4), breaks = -4:4) +
  scale_x_continuous(
    limits = c(-4, 4),
    breaks = -4:4,
    labels = c(
      sprintf("$%s\\sigma$", -4:-1),
      "$\\mu$",
      sprintf("$+%s\\sigma$", 1:4)
    )
  ) +
  scale_y_continuous(
    limits = c(0, 0.63),
    expand = expansion(0, 0),
    labels = NULL,
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = r'( The \ {\color[HTML]{DD6862}\rule{0.2cm}{0.2cm} $68\%$}\ \ {\color[HTML]{E58B85}\rule{0.2cm}{0.2cm} $95\%$}\ \ {\color[HTML]{F1C3C0}\rule{0.2cm}{0.2cm} $99.7\%$} Rule )'
  ) +
  theme_kyle(base_size = 14, grid = "") +
  theme(
    plot.title = element_text(face = "plain"),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  ))


## Export ----
# %%
kfbmisc::tikzsave(
  here("00-Review_Probability_and_Statistics/figures/ex_probability_leq.pdf"),
  plot = plot_ex_probability_leq,
  width = 8,
  height = 3.8
)
kfbmisc::tikzsave(
  here(
    "00-Review_Probability_and_Statistics/figures/ex_probability_between.pdf"
  ),
  plot = plot_ex_probability_between,
  width = 8,
  height = 3.8
)
kfbmisc::tikzsave(
  here("00-Review_Probability_and_Statistics/figures/ex_normal_dist.pdf"),
  plot = plot_ex_normal_dist,
  width = 8,
  height = 3.8
)
kfbmisc::tikzsave(
  here("00-Review_Probability_and_Statistics/figures/68_95_99.pdf"),
  plot = plot_68_95_99_rule,
  width = 8,
  height = 3.8
)
