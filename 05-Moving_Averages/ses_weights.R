# %%
library(tidyverse)
library(here)

ses_weights <- function(alpha = 0.5, t) {
  alpha * (1 - alpha)^t
}

t <- c(0, 1, 2, 3, 4, 5, 6, 7)
x_labels <-
  c("$y_t$", sprintf("$y_{t-%s}$", t[-1]))

weights <- tibble(alpha = c(0.9, 0.75, 0.5, 0.25, 0.1)) |>
  group_by(alpha) |>
  reframe(
    t = t,
    w = ses_weights(alpha = alpha, t = t)
  ) |>
  mutate(
    alpha = sprintf("$\\alpha = %0.2f$", alpha)
  )

(p_ses_weights <- ggplot(weights) +
  geom_hline(yintercept = 0, color = kfbmisc::tailwind_color("zinc-700")) +
  geom_line(
    aes(x = t, y = w, group = alpha, color = alpha),
    linewidth = 1.25
  ) +
  geom_point(
    aes(x = t, y = w, group = alpha, color = alpha)
  ) +
  labs(
    x = NULL,
    y = "Weight put on observation",
    color = "$\\leftarrow$ More memory \\qquad \\qquad \\qquad \\qquad \\quad Quicker updating $\\rightarrow$"
  ) +
  scale_x_continuous(
    breaks = t,
    minor_breaks = c(t - 0.5, 7.5),
    labels = x_labels,
    expand = c(0, 0.5)
  ) +
  scale_color_manual(
    values = kfbmisc::tailwind_color(c(
      "zinc-200",
      "zinc-400",
      "zinc-500",
      "zinc-600",
      "zinc-800"
    ))
  ) +
  kfbmisc::theme_kyle(base_size = 14, grid = "h", legend = "top") +
  theme(
    legend.title.position = "top",
    legend.title = element_text(size = rel(1.125))
  ))

# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/ses_weights_for_different_alpha.pdf"),
  plot = p_ses_weights,
  width = 8,
  height = 4.5
)
