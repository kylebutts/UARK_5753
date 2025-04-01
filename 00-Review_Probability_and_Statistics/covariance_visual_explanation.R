library(tidyverse)

set.seed(20250329)
N <- 20
x <- runif(N, 6, 10)
y <- 0.3 * x + rnorm(N, 0, sd = 0.5)

ybar <- mean(y)
xbar <- mean(x)

df <- tibble(x = x, y = y) |>
  mutate(
    dev_x = x - xbar,
    dev_y = y - ybar,
    color = case_when(
      dev_x * dev_y >= 0 ~ "green",
      TRUE ~ "red"
    )
  )

(cov_explanation <- ggplot(df, aes(x = x, y = y)) +
  geom_rect(
    aes(
      xmin = ifelse(dev_x > 0, xbar, x),
      xmax = ifelse(dev_x > 0, x, xbar),
      ymin = ifelse(dev_y > 0, ybar, y),
      ymax = ifelse(dev_y > 0, y, ybar),
      fill = color,
      color = color
    ),
    alpha = 0.3,
  ) +
  geom_point(
    color = kfbmisc::tailwind_color("zinc-600")
  ) +
  geom_hline(
    yintercept = ybar,
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  geom_vline(
    xintercept = xbar,
    linetype = "dashed",
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  annotate(
    "label",
    label = "$x > \\bar{x}$ and $y > \\bar{y}$",
    x = max(df$x) + 0.1,
    y = max(df$y) + 0.1,
    size = 4,
    label.size = NA,
    hjust = 1,
    vjust = 0
  ) +
  annotate(
    "label",
    label = "$x < \\bar{x}$ and $y < \\bar{y}$",
    x = min(df$x) - 0.1,
    y = min(df$y) - 0.1,
    size = 4,
    label.size = NA,
    hjust = 0,
    vjust = 1
  ) +
  annotate(
    "label",
    label = "$x > \\bar{x}$ and $y < \\bar{y}$",
    x = max(df$x) + 0.1,
    y = min(df$y) - 0.1,
    size = 4,
    label.size = NA,
    hjust = 1,
    vjust = 1
  ) +
  annotate(
    "label",
    label = "$x < \\bar{x}$ and $y > \\bar{y}$",
    x = min(df$x) - 0.1,
    y = max(df$y) + 0.1,
    size = 4,
    label.size = NA,
    hjust = 0,
    vjust = 0
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = expansion(mult = 0, add = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0, add = 0.25)) +
  labs(
    title = "Covariance Visualization",
    x = NULL,
    y = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14))

kfbmisc::tikzsave(
  "00-Review_Probability_and_Statistics/figures/covariance_visualized.pdf",
  plot = cov_explanation,
  width = 8,
  height = 5
)
