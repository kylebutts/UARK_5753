# Main ----
library(tidyverse)
# From https://archive.ics.uci.edu/dataset/45/heart+disease
df <- read_csv("data/raw.csv")

df <- df |>
  mutate(
    chest_pain = case_when(
      cp == 1 ~ "typical angina",
      cp == 2 ~ "atypical angina",
      cp == 3 ~ "non-anginal pain",
      cp == 4 ~ "asymptomatic"
    ),
    .after = cp
  ) |>
  mutate(
    has_heart_problem = case_when(
      num == 0 ~ 0,
      num >= 1 ~ 1
    )
  ) |>
  select(
    -cp,
    -ca,
    cholestoral = chol,
    is_male = sex,
    resting_blood_pressure = trestbps,
    has_high_fasting_blood_pressure = fbs,
    -restecg,
    maximum_heart_rate_achieved = thalach,
    exercise_induced_angina = exang,
    -oldpeak,
    -slope,
    -thal,
    heart_diagnosis = num
  )

write_csv(df, "data/heart_disease.csv")
