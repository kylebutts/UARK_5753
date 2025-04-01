# %%
library(tidyverse)
library(fixest)
library(here)

# %%
raw <- haven::read_dta("Exams/S2025_Midterm/cps.dta")
df <- raw |>
  # Employed workers
  filter(labforce == 2) |>
  filter(incwage != 99999999) |>
  filter(incwage != 0) |>
  #  Race variables
  filter(race %in% c(100, 200, 300, 651, 652)) |>
  mutate(
    race = case_when(
      hispan > 0 ~ "Hispanic",
      race == 100 ~ "White",
      race == 200 ~ "Black",
      race == 300 ~ "Native",
      race %in% c(651, 652) ~ "Asian or Pacific Islander"
    )
  ) |>
  # Education
  # at least 10th grade
  filter(educ >= 50) |>
  mutate(
    has_college_experience = educ >= 81
  )

feols(
  has_college_experience ~ i(race, ref = "White"),
  df,
  vcov = "HC1"
)

feols(
  incwage ~ i(race, ref = "White"),
  df,
  vcov = "HC1"
)

est_quadratic <- feols(
  incwage ~ age + age^2,
  data = df,
  vcov = "HC1"
)
pred_grid <- tibble(age = 15:85)
pred_grid$incwage_hat <-
  predict(est_quadratic, newdata = pred_grid)

sum <- df |>
  summarize(.by = age, incwage_mean = mean(incwage, na.rm = TRUE))


(plot_wage_age <- ggplot() +
  geom_line(
    aes(x = age, y = incwage_hat),
    data = pred_grid,
    color = kfbmisc::kyle_color("magenta")
  ) +
  geom_point(aes(x = age, y = incwage_mean), data = sum, shape = 21) +
  scale_y_continuous(
    labels = scales::label_currency(
      prefix = "\\$",
      scale_cut = scales::cut_long_scale()
    )
  ) +
  labs(x = "Age", y = NULL) +
  kfbmisc::theme_kyle(base_size = 14))

kfbmisc::tikzsave(
  here("Exams/S2025_Midterm/figures/plot_wage_age.pdf"),
  plot_wage_age,
  width = 6,
  height = 3
)
