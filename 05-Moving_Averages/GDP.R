# %%
library(tidyverse)
library(here)
library(forecast)

df <-
  here("05-Moving_Averages/data/FRED/GDP.csv") |>
  read_csv(show_col_types = FALSE) |>
  janitor::clean_names() |>
  filter(date >= date("1976-01-01"))

recessions <- tibble::tibble(
  start_date = as.Date(c(
    "1980-02-01",
    "1981-08-01",
    "1990-08-01",
    "2001-04-01",
    "2008-01-01",
    "2020-03-01"
  )),
  end_date = as.Date(c(
    "1980-07-01",
    "1982-11-01",
    "1991-03-01",
    "2001-11-01",
    "2009-06-01",
    "2020-04-01"
  )),
)


# recessions <-
#   here("05-Moving_Averages/data/FRED/USREC.csv") |>
#   read_csv(show_col_types = FALSE) |>
#   janitor::clean_names() |>
#   filter(date >= date("1976-01-01"))
#
# # Function to get intervals of evenly spaced dates
# find_intervals <- function(dates) {
#   intervals <- NULL
#
#   start_date <- dates[1]
#   for (i in 2:length(dates)) {
#     # Calculate the difference in months between consecutive dates
#     diff_days <- (dates[i] - dates[i - 1])
#
#     # If the difference is greater than 1, mark the end of the interval
#     if (diff_days > 31) {
#       intervals <- bind_rows(
#         intervals,
#         tibble(start_date = start_date, end_date = dates[i - 1])
#       )
#       start_date <- dates[i] # Start a new interval
#     }
#   }
#
#   intervals <- bind_rows(
#     intervals,
#     tibble(start_date = start_date, end_date = dates[length(dates)])
#   )
#
#   # Return the intervals
#   return(intervals)
# }
#
# # Call the function with the recessions vector
# recessions <- recessions |>
#   filter(usrec == 1) |>
#   pull(date) |>
#   find_intervals()

# %%
(p_gdp <- ggplot() +
  geom_rect(
    aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
    data = recessions,
    fill = kfbmisc::tailwind_color("zinc-500"),
    alpha = 0.3
  ) +
  geom_line(
    aes(x = date, y = gdp),
    data = df,
    linewidth = 1.25,
    color = kfbmisc::kyle_color("magenta")
  ) +
  scale_x_date(limits = date(c("1976-01-01", "2024-12-31"))) +
  labs(
    x = NULL,
    y = "U.S. GDP"
  ) +
  kfbmisc::theme_kyle(base_size = 14))

# %%
df$gdp_hat_ses_alpha_pt8 <- ses(df$gdp, alpha = 0.8)$fitted
df$gdp_hat_ses_alpha_pt5 <- ses(df$gdp, alpha = 0.5)$fitted
df$gdp_hat_ses_alpha_pt2 <- ses(df$gdp, alpha = 0.2)$fitted
df$gdp_hat_holt_opt <- holt(df$gdp)$fitted

# %%
(p_gdp_ses <- ggplot() +
  geom_rect(
    aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
    data = recessions,
    fill = kfbmisc::tailwind_color("zinc-500"),
    alpha = 0.3
  ) +
  geom_line(
    aes(x = date, y = gdp, color = "A"),
    data = df,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = date, y = gdp_hat_ses_alpha_pt8, color = "B"),
    data = df,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = date, y = gdp_hat_ses_alpha_pt2, color = "C"),
    data = df,
    linewidth = 1.25
  ) +
  scale_x_date(limits = date(c("1976-01-01", "2024-12-31"))) +
  labs(
    x = NULL,
    y = "U.S. GDP",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "A" = kfbmisc::tailwind_color("zinc-300"),
      "B" = kfbmisc::kyle_color("magenta"),
      "C" = kfbmisc::kyle_color("blue")
    ),
    labels = c(
      "A" = "$y_t$",
      "B" = "SES with $\\alpha = 0.8$",
      "C" = "SES with $\\alpha = 0.2$"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))


(p_gdp_ses_and_holt <- ggplot() +
  geom_rect(
    aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
    data = recessions,
    fill = kfbmisc::tailwind_color("zinc-500"),
    alpha = 0.3
  ) +
  geom_line(
    aes(x = date, y = gdp, color = "A"),
    data = df,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = date, y = gdp_hat_ses_alpha_pt8, color = "B"),
    data = df,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = date, y = gdp_hat_ses_alpha_pt2, color = "C"),
    data = df,
    linewidth = 1.25
  ) +
  geom_line(
    aes(x = date, y = gdp_hat_holt_opt, color = "D"),
    data = df,
    linewidth = 1.25
  ) +
  scale_x_date(limits = date(c("1976-01-01", "2024-12-31"))) +
  labs(
    x = NULL,
    y = "U.S. GDP",
    color = NULL
  ) +
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
      "C" = "SES with $\\alpha = 0.2$",
      "D" = "Holt Method with Optimal $\\hat{\\alpha}$ and $\\hat{\\beta}$"
    )
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/gdp.pdf"),
  plot = p_gdp,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/gdp_ses_alphas.pdf"),
  plot = p_gdp_ses,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/gdp_ses_and_holt.pdf"),
  plot = p_gdp_ses_and_holt,
  width = 8,
  height = 4.5
)

# %%
