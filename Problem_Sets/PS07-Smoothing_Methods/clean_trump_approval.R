# %%
# Source:
# https://www.natesilver.net/p/trump-approval-ratings-nate-silver-bulletin
# Accessed 2025-04-13
library(tidyverse)
library(here)
library(forecast)

raw <- read_csv(here(
  "Problem_Sets/PS07-Smoothing_Methods/raw/silver_bulletin_trump_approval.csv"
))

polls <- raw |>
  mutate(
    start_date = mdy(startdate),
    end_date = mdy(enddate),
    population = case_when(
      population == "LV" ~ "Likely voters",
      population == "RV" ~ "Registered voters",
      population == "V" ~ "Voters",
      population == "A" ~ "U.S. adults"
    )
  ) |>
  select(
    start_date,
    end_date,
    sample_size = samplesize,
    population,
    approve,
    disapprove,
    net,
    weight,
    influence
  )

# Expand to create pseudo complete panel
expanded_polls <- list_rbind(map(split(polls, 1:nrow(polls)), function(row) {
  expanded <- tibble(
    date = seq(row$start_date, row$end_date, "1 day")
  )
  expanded$population = row$population
  expanded$sample_size = row$sample_size
  expanded$approve = row$approve
  expanded$disapprove = row$disapprove
  expanded$net = row$net
  expanded$weight = row$weight
  expanded$influence = row$influence
  return(expanded)
}))

polls_sum <- expanded_polls |>
  summarize(
    .by = date,
    approve = sum(approve * weight / sum(weight)),
    disapprove = sum(disapprove * weight / sum(weight))
  ) |>
  mutate(net = approve - disapprove) |>
  arrange(date) |>
  filter(date >= ymd("2024-01-01"))

# %%
write_csv(
  polls_sum,
  here(
    "Problem_Sets/PS07-Smoothing_Methods/data/silver_bulletin_trump_approval.csv"
  )
)

# %%
ggplot() +
  geom_point(
    aes(x = date, y = approve, color = "A"),
    data = polls_sum,
    shape = 21
  ) +
  geom_line(
    aes(
      x = date,
      y = slide_dbl(
        approve,
        mean,
        .before = 15,
        .after = 0,
        .complete = TRUE
      ),
      color = "A"
    ),
    data = polls_sum,
    size = 1.2
  ) +
  geom_point(
    aes(x = date, y = disapprove, color = "B"),
    data = polls_sum,
    shape = 21
  ) +
  geom_line(
    aes(
      x = date,
      y = slide_dbl(
        disapprove,
        mean,
        .before = 15,
        .after = 0,
        .complete = TRUE
      ),
      color = "B"
    ),
    data = polls_sum,
    size = 1.2
  ) +
  scale_color_manual(
    values = c(
      "A" = "#22c55e",
      "B" = "#f87171"
    ),
    labels = c(
      "A" = "Approve",
      "B" = "Disapprove"
    )
  ) +
  labs(color = NULL, x = NULL, y = "Percent") +
  theme_light(base_size = 14) +
  theme(
    legend.position = "top",
    legend.justification = c(0, 1),
    legend.location = "plot"
  )

# %%
ggplot() +
  geom_point(
    aes(x = date, y = approve, color = "A"),
    data = polls_sum,
    shape = 21
  ) +
  geom_line(
    aes(
      x = date,
      y = ses(approve, alpha = 0.2)$fitted,
      color = "A"
    ),
    data = polls_sum,
    size = 1.2
  ) +
  geom_point(
    aes(x = date, y = disapprove, color = "B"),
    data = polls_sum,
    shape = 21
  ) +
  geom_line(
    aes(
      x = date,
      y = ses(disapprove, alpha = 0.2)$fitted,
      color = "B"
    ),
    data = polls_sum,
    size = 1.2
  ) +
  scale_color_manual(
    values = c(
      "A" = "#22c55e",
      "B" = "#f87171"
    ),
    labels = c(
      "A" = "Approve",
      "B" = "Disapprove"
    )
  ) +
  labs(color = NULL, x = NULL, y = "Percent") +
  theme_light(base_size = 14) +
  theme(
    legend.position = "top",
    legend.justification = c(0, 1),
    legend.location = "plot"
  )
