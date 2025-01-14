# %%
library(tidyverse)
library(here)
library(kfbmisc)

fs::dir_create(here("Notes/Review_Probability_and_Statistics/figures/"))

pkgs <- system.file(c("tikzsave/paper.sty", "tikzsave/math.sty"), package = "kfbmisc")
options(tikzMetricPackages = c(
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[T1]{fontenc}",
  "\\usetikzlibrary{calc}",
  sprintf("\\usepackage{%s}", xfun::sans_ext(pkgs))
))

colors <- c(
  "red_pink" = "#e64173",
  "turquoise" = "#20B2AA",
  "orange" = "#FFA500",
  "red" = "#fb6107",
  "blue" = "#3b3b9a",
  "green" = "#8bb174",
  "purple" = "#6A5ACD",
  "zinc50" = "#fafafa",
  "zinc100" = "#f4f4f5",
  "zinc200" = "#e4e4e7",
  "zinc300" = "#d4d4d8",
  "zinc400" = "#a1a1aa",
  "zinc500" = "#71717a",
  "zinc600" = "#52525b",
  "zinc700" = "#3f3f46",
  "zinc800" = "#27272a",
  "zinc900" = "#18181b",
  "zinc950" = "#09090b"
)

#' ## Examples of normal distribution
# %%
(plot_ex_normal_dist <- ggplot() +
  stat_function(
    fun = function(x) dnorm(x, mean = 0, sd = 1),
    color = colors["green"], linewidth = 1.5, n = 300
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = 3, sd = 1),
    color = colors["purple"], linewidth = 1.5, n = 300
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = 0, sd = 2),
    color = colors["turquoise"], linewidth = 1.5, n = 300
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = 0, sd = 3),
    color = colors["red_pink"], linewidth = 1.5, n = 300
  ) +
  scale_x_continuous(
    limits = c(-10, 10), breaks = seq(-10, 10, by = 2), expand = expansion(0, 0)
  ) +
  scale_y_continuous(limits = c(0, 0.5), expand = expansion(0, 0)) +
  labs(
    y = "Density",
    title = "PDFs: {\\color[HTML]{8bb174} $Z = \\mathcal{N}(0, 1)$}, {\\color[HTML]{6A5ACD} $\\mathcal{N}(3, 1)$}, {\\color[HTML]{20B2AA} $\\mathcal{N}(0, 4)$}, {\\color[HTML]{e64173} $\\mathcal{N}(0, 9)$}"
  ) +
  theme_kyle(base_size = 14) +
  theme(plot.title = element_text(face = "plain"))
)

kfbmisc::tikzsave(
  here("Notes/Review_Probability_and_Statistics/figures/ex_normal_dist.pdf"),
  plot = plot_ex_normal_dist, width = 8, height = 3.8
)


#' ## Integrating density function
# %%
curr_density <- function(x) dnorm(x, mean = 10, sd = 2)
(plot_ex_probability_leq <- ggplot() +
  stat_function(
    fun = curr_density,
    geom = "area", size = 0,
    fill = colors["purple"], alpha = 0.3,
    xlim = c(0, 12), n = 300
  ) +
  stat_function(
    fun = curr_density,
    color = colors["purple"], linewidth = 1, n = 300
  ) +
  scale_x_continuous(
    limits = c(2, 18), expand = expansion(0, 0)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    y = NULL, x = NULL,
    title = r'( $\prob{X \leq 12}$ )'
  ) +
  theme_kyle(base_size = 14) +
  theme(plot.title = element_text(face = "plain"))
)

kfbmisc::tikzsave(
  here("Notes/Review_Probability_and_Statistics/figures/ex_probability_leq.pdf"),
  plot = plot_ex_probability_leq, width = 8, height = 3.8
)

# %%
curr_density <- function(x) dnorm(x, mean = 10, sd = 2)
(plot_ex_probability_between <- ggplot() +
  stat_function(
    fun = curr_density,
    geom = "area", size = 0,
    fill = colors["purple"], alpha = 0.3,
    xlim = c(0, 12), n = 300
  ) +
  stat_function(
    fun = curr_density,
    geom = "area", size = 0,
    fill = "red", alpha = 0.3,
    xlim = c(0, 9), n = 300
  ) +
  stat_function(
    fun = curr_density,
    color = colors["purple"], linewidth = 1, n = 300
  ) +
  scale_x_continuous(
    limits = c(2, 18), expand = expansion(0, 0),
    breaks = c(9, 12), labels = c("$\\underline{x}$", "$\\bar{x}$")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    y = NULL, x = NULL,
    title = r'( $\prob{\underline{x} \leq X \leq \bar{x}} = \prob{X \leq \bar{x}} - \prob{\underline{x} \leq X } $  )'
  ) +
  theme_kyle(base_size = 14) +
  theme(plot.title = element_text(face = "plain"))
)

kfbmisc::tikzsave(
  here("Notes/Review_Probability_and_Statistics/figures/ex_probability_between.pdf"),
  plot = plot_ex_probability_between, width = 8, height = 3.8
)



# %%
# from svnmiller
(plot_68_95_99_rule <- ggplot() +
  # +/- 1
  stat_function(
    fun = dnorm, xlim = c(-1, 1), linewidth = 0,
    geom = "area", fill = "#d2382c", alpha = 0.5
  ) +
  # +/- 2
  stat_function(
    fun = dnorm, xlim = c(-2, 2), linewidth = 0,
    geom = "area", fill = "#d2382c", alpha = 0.4
  ) +
  # +/- 3
  stat_function(
    fun = dnorm, xlim = c(-3, 3), linewidth = 0,
    geom = "area", fill = "#d2382c", alpha = 0.3
  ) +
  # Normal curve
  stat_function(fun = dnorm, color = "black", linewidth = 1.5) +
  # 68%
  annotate("label", x = 0, y = 0.44, size = 4, label = "$68\\%$", label.size = NA) +
  annotate("path", x = c(-1, -1, -0.3), y = c(dnorm(-1), 0.44, 0.44), color = "black", linewidth = 1.2) +
  annotate("path", x = c(1, 1, 0.3), y = c(dnorm(-1), 0.44, 0.44), color = "black", linewidth = 1.2) +
  # 95%
  annotate("label", x = 0, y = 0.5, size = 4, label = "$95\\%$", label.size = NA) +
  annotate("path", x = c(-2, -2, -0.3), y = c(dnorm(-2), 0.5, 0.5), color = "black", linewidth = 1.2) +
  annotate("path", x = c(2, 2, 0.3), y = c(dnorm(-2), 0.5, 0.5), color = "black", linewidth = 1.2) +
  # 99.7%
  annotate("label", x = 0, y = 0.56, size = 4, label = "$99.7\\%$", label.size = NA) +
  annotate("path", x = c(-3, -3, -0.35), y = c(dnorm(-3), 0.56, 0.56), color = "black", linewidth = 1.2) +
  annotate("path", x = c(3, 3, 0.35), y = c(dnorm(-3), 0.56, 0.56), color = "black", linewidth = 1.2) +
  #
  # scale_x_continuous(limits = c(-4, 4), breaks = -4:4) +
  scale_x_continuous(
    limits = c(-4, 4),
    breaks = -4:4,
    labels = c(sprintf("$%s\\sigma$", -4:-1), "$\\mu$", sprintf("$+%s\\sigma$", 1:4))
  ) +
  scale_y_continuous(
    limits = c(0, 0.68),
    expand = expansion(0, 0)
  ) +
  labs(
    x = NULL, y = NULL,
    title = r'( The {\color[HTML]{DD6862}\rule{0.2cm}{0.2cm} $68\%$} {\color[HTML]{E58B85}\rule{0.2cm}{0.2cm} $95\%$} {\color[HTML]{F1C3C0}\rule{0.2cm}{0.2cm} $99.7\%$} Rule )'
  ) +
  theme_kyle(base_size = 14) +
  theme(plot.title = element_text(face = "plain"))
)

kfbmisc::tikzsave(
  here("Notes/Review_Probability_and_Statistics/figures/68_95_99.pdf"),
  plot = plot_68_95_99_rule, width = 8, height = 3.8
)

# %%
