# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(fixest)
library(here)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nba <- read_csv(here(
  "Projects/Cross_Sectional/data/nba_2k25_player_ratings/players.csv"
)) |>
  mutate(
    height_in = parse_number(height_ft),
    is_starter = as.integer(games_started > 0.5 * games_played),
    is_big = as.integer(position_1 %in% c("C", "PF"))
  )

etable(
  feols(points ~ height_in, nba),
  feols(points ~ height_in + i(position_1), nba),
  vcov = "HC1",
  title = "NBA Player Overall Rating"
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(wooldridge)
data("wage1", package = "wooldridge")
wage1 <- wage1 |>
  mutate(
    postsecondary = as.integer(educ > 16),
    collgrad = as.integer(educ == 16),
    some_college = as.integer(educ > 12),
    log_wage = log(wage),
  ) |>
  filter(educ >= 11)

etable(
  feols(wage ~ some_college + collgrad + postsecondary, wage1),
  feols(wage ~ i(educ), wage1),
  vcov = "HC1"
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(AER)
data("CASchools", package = "AER")

etable(
  feols(english ~ computer, CASchools),
  feols(english ~ computer + income, CASchools),
  vcov = "HC1"
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(camerondata)
data("schooling", package = "camerondata")

df <- schooling |>
  rename(educ = gfill76, ln_wage = wage76,father_educ = fgrade, mother_educ = mgrade)

etable(
  feols(ln_wage ~ educ, data = df),
  feols(ln_wage ~ educ + father_educ + mother_educ, data = df),
  vcov = "HC1"
)

