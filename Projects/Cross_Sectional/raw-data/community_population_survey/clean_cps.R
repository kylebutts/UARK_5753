# %%
library(tidyverse)
library(here)

raw <- here(
  "Projects/Cross_Sectional/raw-data/community_population_survey/cps.dta"
) |>
  haven::read_dta() |>
  haven::zap_labels() |>
  haven::zap_label() |>
  haven::zap_formats()

df <- raw |>
  select(
    -cpsid,
    -cpsidp,
    -cpsidv,
    -asecflag,
    -asecwth,
    -asecwt,
    -region,
    -statecensus,
    -pernum,
    -nativity,
    -empstat,
    -diffeye,
    -diffhear,
    -diffphys,
    -occly,
    -workly,
    -hourwage,
    -month,
    -serial
  ) |>
  mutate(
    male = as.numeric(sex == 1),
    .keep = "unused"
  ) |>
  mutate(
    marital_status = case_when(
      marst == 1 ~ "Married, Spouse Present",
      marst == 2 | marst == 3 ~ "Separated",
      marst == 4 ~ "Divorced",
      marst == 5 ~ "Widowed",
      marst == 6 ~ "Single"
    ),
    .keep = "unused"
  ) |>
  mutate(
    veteran_status = if_else(vetstat == 0, NA, as.numeric(vetstat == 2)),
    .keep = "unused"
  ) |>
  mutate(citizen_status = as.numeric(citizen != 5), .keep = "unused") |>
  # Employed workers
  filter(labforce == 2) |>
  select(-labforce) |>
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
    ),
    .keep = "unused"
  ) |>
  # Education
  # at least 10th grade
  filter(educ >= 50) |>
  mutate(
    has_college_experience = as.numeric(educ >= 81),
    years_educ = case_when(
      educ == 50 ~ 10,
      educ == 60 ~ 11,
      educ == 71 ~ 12,
      educ == 73 ~ 12,
      educ == 81 ~ 14,
      educ == 91 ~ 14,
      educ == 92 ~ 14,
      educ == 111 ~ 16,
      educ == 123 ~ 17,
      educ == 124 ~ 17,
      educ == 125 ~ 21
    ),
    .keep = "unused"
  )

glimpse(df)


fs::dir_create(here(
  "Projects/Cross_Sectional/data/community_population_survey/"
))
write_csv(
  df,
  file = here(
    "Projects/Cross_Sectional/data/community_population_survey/cps.csv"
  )
)
