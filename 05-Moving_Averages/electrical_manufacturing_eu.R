# %%
library(tidyverse)
library(here)
library(forecast)

# From `elecequip` from `fpp2` package
df <- read_csv(here("05-Moving_Averages/data/electrical_manufacturing_eu.csv"))

# %%
(p_raw_q <- ggplot() +
  geom_line(
    aes(x = day, y = q_manufactured),
    data = df,
    linewidth = 1,
    color = kfbmisc::tailwind_color("zinc-400")
  ) +
  labs(
    x = NULL,
    y = "Quantity Produced in the EU",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))


# %%
# Using last-periods value
T <- nrow(df)
df$q_hat_last <- c(NA, df$q_manufactured[-T])

df$q_hat_avg_last_3 <-
  slider::slide_dbl(
    df$q_manufactured,
    mean,
    .before = 3L,
    .after = -1L,
    .complete = TRUE
  )

(p_q_hat_last <- ggplot() +
  geom_line(
    aes(x = day, y = q_manufactured, color = "A"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = day, y = q_hat_last, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  labs(
    x = NULL,
    y = "Quantity Produced in the EU",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "$\\hat{y}_t = y_{t-1}$"
    )
  ) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(p_q_hat_avg_last_3 <- ggplot() +
  geom_line(
    aes(x = day, y = q_manufactured, color = "A"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = day, y = q_hat_avg_last_3, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  labs(
    x = NULL,
    y = "Quantity Produced in the EU",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "$\\hat{y}_t = \\sum_{k=1}^3 \\frac{1}{3} y_{t-k}$"
    )
  ) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

# %%
df$q_hat_moving_avg_3 <-
  slider::slide_dbl(
    df$q_manufactured,
    mean,
    .before = 1L,
    .after = 1L,
    .complete = TRUE
  )

df$q_hat_moving_avg_9 <-
  slider::slide_dbl(
    df$q_manufactured,
    mean,
    .before = 4L,
    .after = 4L,
    .complete = TRUE
  )

df$q_hat_moving_avg_25 <-
  slider::slide_dbl(
    df$q_manufactured,
    mean,
    .before = 12L,
    .after = 12L,
    .complete = TRUE
  )

(p_q_hat_moving_avg_3 <- ggplot() +
  geom_line(
    aes(x = day, y = q_manufactured, color = "A"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = day, y = q_hat_moving_avg_3, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  labs(
    x = NULL,
    y = "Quantity Produced in the EU",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "$\\hat{y}_t = \\sum_{k=-1}^1 \\frac{1}{3} y_{t+k}$"
    )
  ) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(p_q_hat_moving_avg_9 <- ggplot() +
  geom_line(
    aes(x = day, y = q_manufactured, color = "A"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = day, y = q_hat_moving_avg_3, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  geom_line(
    aes(x = day, y = q_hat_moving_avg_9, color = "C"),
    data = df,
    linewidth = 1.2
  ) +
  labs(
    x = NULL,
    y = "Quantity Produced in the EU",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("blue")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "$\\hat{y}_t = \\sum_{k=-1}^1 \\frac{1}{3} y_{t+k}$",
      "C" = "$\\hat{y}_t = \\sum_{k=-4}^4 \\frac{1}{9} y_{t+k}$"
    )
  ) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(p_q_hat_moving_avg_25 <- ggplot() +
  geom_line(
    aes(x = day, y = q_manufactured, color = "A"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = day, y = q_hat_moving_avg_3, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  geom_line(
    aes(x = day, y = q_hat_moving_avg_9, color = "C"),
    data = df,
    linewidth = 1.2
  ) +
  geom_line(
    aes(x = day, y = q_hat_moving_avg_25, color = "D"),
    data = df,
    linewidth = 1.2
  ) +
  labs(
    x = NULL,
    y = "Quantity Produced in the EU",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("blue"),
      "D" = kfbmisc::kyle_color("yellow")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "$\\hat{y}_t = \\sum_{k=-1}^1 \\frac{1}{3} y_{t+k}$",
      "C" = "$\\hat{y}_t = \\sum_{k=-4}^4 \\frac{1}{9} y_{t+k}$",
      "D" = "$\\hat{y}_t = \\sum_{k=-12}^{12} \\frac{1}{25} y_{t+k}$"
    )
  ) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

# %%
simple_exp_smooth <- function(x, alpha = 0.5) {
  x_hat <- c(x[1])
  for (t in 2:length(x)) {
    x_hat[t] <-
      alpha * x[t - 1] + (1 - alpha) * x_hat[t - 1]
  }
  x_hat[1] <- NA
  return(x_hat)
}

df$q_hat_ses_alpha_0_8 = simple_exp_smooth(
  df$q_manufactured,
  alpha = 0.8
)
df$q_hat_ses_alpha_0_3 = simple_exp_smooth(
  df$q_manufactured,
  alpha = 0.3
)
df$q_hat_ses_alpha_0_1 = simple_exp_smooth(
  df$q_manufactured,
  alpha = 0.1
)


(p_q_hat_ses_alpha_0_8 <- ggplot() +
  geom_line(
    aes(x = day, y = q_manufactured, color = "A"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = day, y = q_hat_ses_alpha_0_8, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  labs(
    x = NULL,
    y = "Quantity Produced in the EU",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "SES with $\\alpha = 0.8$"
    )
  ) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))


(p_q_hat_ses_alphas <- ggplot() +
  geom_line(
    aes(x = day, y = q_manufactured, color = "A"),
    data = df,
    linewidth = 1
  ) +
  geom_line(
    aes(x = day, y = q_hat_ses_alpha_0_8, color = "B"),
    data = df,
    linewidth = 1.2
  ) +
  geom_line(
    aes(x = day, y = q_hat_ses_alpha_0_3, color = "C"),
    data = df,
    linewidth = 1.2
  ) +
  geom_line(
    aes(x = day, y = q_hat_ses_alpha_0_1, color = "D"),
    data = df,
    linewidth = 1.2
  ) +
  labs(
    x = NULL,
    y = "Quantity Produced in the EU",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("blue"),
      "D" = kfbmisc::kyle_color("yellow")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "SES with $\\alpha = 0.8$",
      "C" = "SES with $\\alpha = 0.3$",
      "D" = "SES with $\\alpha = 0.1$"
    )
  ) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))


# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/electrical_q_raw.pdf"),
  plot = p_raw_q,
  width = 8,
  height = 4.5
)

kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/electrical_q_hat_last.pdf"),
  plot = p_q_hat_last,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/electrical_q_hat_avg_last_3.pdf"),
  plot = p_q_hat_avg_last_3,
  width = 8,
  height = 4.5
)

kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/electrical_q_hat_moving_average_3.pdf"),
  plot = p_q_hat_moving_avg_3,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/electrical_q_hat_moving_average_9.pdf"),
  plot = p_q_hat_moving_avg_9,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/electrical_q_hat_moving_average_25.pdf"),
  plot = p_q_hat_moving_avg_25,
  width = 8,
  height = 4.5
)

kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/electrical_q_hat_ses_alpha_0_8.pdf"),
  plot = p_q_hat_ses_alpha_0_8,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/electrical_q_hat_ses_alphas.pdf"),
  plot = p_q_hat_ses_alphas,
  width = 8,
  height = 4.5
)


# %%
library(tsibble)
df$yearmonth <- tsibble::yearmonth(df$day)
as_tsibble(
  df,
  index = yearmonth
)
