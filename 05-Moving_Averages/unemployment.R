# %%
library(tidyverse)
library(here)

unemployment <-
  here("05-Moving_Averages/data/FRED/UNRATE.csv") |>
  read_csv(show_col_types = FALSE) |>
  janitor::clean_names() |>
  filter(date >= date("1976-01-01"))

set.seed(20241023)
unemployment <- unemployment |>
  mutate(
    unrate_shuffled = sample(unrate, size = n(), replace = TRUE)
  )

recessions <-
  here("05-Moving_Averages/data/FRED/USREC.csv") |>
  read_csv(show_col_types = FALSE) |>
  janitor::clean_names() |>
  filter(date >= date("1976-01-01"))

# Function to get intervals of evenly spaced dates
find_intervals <- function(dates) {
  intervals <- NULL

  start_date <- dates[1]
  for (i in 2:length(dates)) {
    # Calculate the difference in months between consecutive dates
    diff_months <- interval(dates[i - 1], dates[i]) %/% months(1)

    # If the difference is greater than 1, mark the end of the interval
    if (diff_months > 1) {
      intervals <- bind_rows(
        intervals,
        tibble(start_date = start_date, end_date = dates[i - 1])
      )
      start_date <- dates[i] # Start a new interval
    }
  }

  intervals <- bind_rows(
    intervals,
    tibble(start_date = start_date, end_date = dates[length(dates)])
  )

  # Return the intervals
  return(intervals)
}

# Call the function with the recessions vector
recessions <- recessions |>
  filter(usrec == 1) |>
  pull(date) |>
  find_intervals()


# %%
(p_unemployment <- ggplot() +
  geom_rect(
    aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
    data = recessions,
    fill = kfbmisc::tailwind_color("zinc-400"),
    alpha = 0.3
  ) +
  geom_line(
    aes(x = date, y = unrate),
    data = unemployment,
    linewidth = 1.25,
    color = kfbmisc::kyle_color("magenta")
  ) +
  scale_x_date(
    limits = date(c("1976-01-01", "2024-12-31")),
    expand = expansion(0, 0)
  ) +
  labs(
    x = NULL,
    y = "Unemployment Rate (\\%)"
  ) +
  kfbmisc::theme_kyle(base_size = 14))

(p_unemployment_rand_shuffle <- ggplot() +
  geom_rect(
    aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
    data = recessions,
    fill = kfbmisc::tailwind_color("zinc-500"),
    alpha = 0.3
  ) +
  geom_line(
    aes(x = date, y = unrate_shuffled),
    data = unemployment,
    linewidth = 1,
    color = kfbmisc::kyle_color("magenta")
  ) +
  scale_x_date(
    limits = date(c("1976-01-01", "2024-12-31")),
    expand = expansion(0, 0)
  ) +
  labs(
    x = NULL,
    y = "\\textbf{\\emph{Shuffled}} Unemployment Rate (\\%)"
  ) +
  kfbmisc::theme_kyle(base_size = 14))

# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/unemployment_rate.pdf"),
  plot = p_unemployment,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/unemployment_rate_rand_shuffle.pdf"),
  plot = p_unemployment_rand_shuffle,
  width = 8,
  height = 4.5
)

# %%
cov(
  unemployment$unrate[2:nrow(unemployment)],
  unemployment$unrate[1:(nrow(unemployment) - 1)]
)

cor(
  unemployment$unrate[2:nrow(unemployment)],
  unemployment$unrate[1:(nrow(unemployment) - 1)]
)

cor(
  unemployment$unrate[13:nrow(unemployment)],
  unemployment$unrate[1:(nrow(unemployment) - 12)]
)

cor(
  unemployment$unrate_shuffled[2:nrow(unemployment)],
  unemployment$unrate_shuffled[1:(nrow(unemployment) - 1)]
)
