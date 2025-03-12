# %%
# https://www.kaggle.com/datasets/reinerjasin/nba-2k25-player-complete-dataset?resource=download
library(tidyverse)
library(here)
raw <- here(
  "Projects/Cross_Sectional/raw-data/nba_2k25_players/current_nba_players.csv"
) |>
  read_csv(show_col_types = FALSE)

# https://github.com/KristWangCY/BusinessDataMiningProject/blob/main/2023-2024%20NBA%20Player%20Stats%20-%20Regular.csv

# https://github.com/danielfernandoo07/nbacsv/blob/main/2023_nba_player_stats.csv
stats <- here(
  "Projects/Cross_Sectional/raw-data/nba_2k25_players/2023_nba_player_stats.csv"
) |>
  read_csv(show_col_types = FALSE) |>
  janitor::clean_names()

stats <- here(
  "Projects/Cross_Sectional/raw-data/nba_2k25_players/player_stats_23_24.csv"
) |>
  read_csv(show_col_types = FALSE) |>
  janitor::clean_names() |>
  arrange(player, desc(g)) |>
  # For traded players
  slice(.by = player, 1) |>
  select(
    player,
    age,
    points = pts,
    games_played = g,
    games_started = gs,
    minutes_played = mp,
    field_goals = fg,
    field_goal_attempts = fga,
    field_goal_percentage = fg_percent,
    three_pointers = x3p,
    three_point_attempts = x3pa,
    three_point_percentage = x3p_percent,
    two_pointers = x2p,
    two_point_attempts = x2pa,
    two_point_percentage = x2p_percent,
    effective_field_goal_percentage = e_fg_percent,
    free_throws = ft,
    free_throw_attempts = fta,
    free_throw_percentage = ft_percent,
    offensive_rebounds = orb,
    defensive_rebounds = drb,
    total_rebounds = trb,
    assists = ast,
    steals = stl,
    blocks = blk,
    turnovers = tov,
    personal_fouls = pf
  )

players <- raw |>
  select(
    name,
    team,
    nationality_1,
    jersey_number = jersey,
    position_1,
    position_2,
    archetype,
    height_ft = height_feet,
    weight_lb = weight_lbs,
    wingspan_ft = wingspan_feet,
    season_salary,
    years_in_the_nba,
    birthdate,
    # ratings
    rating_overall = overall,
    rating_close_shot = close_shot,
    rating_mid_range_shot = mid_range_shot,
    rating_three_point_shot = three_point_shot,
    rating_free_throw = free_throw,
    rating_shot_iq = shot_iq,
    rating_speed = speed,
    rating_agility = agility,
    rating_strength = strength,
    rating_stamina = stamina,
    rating_overall_durability = overall_durability,
    rating_pass_accuracy = pass_accuracy,
    rating_ball_handle = ball_handle,
    rating_pass_iq = pass_iq,
    rating_group_defense = group_defense,
    rating_interior_defense = interior_defense,
    rating_perimeter_defense = perimeter_defense,
    rating_steal = steal,
    rating_block = block,
    rating_offensive_rebound = offensive_rebound,
    rating_defensive_rebound = defensive_rebound
  ) |>
  mutate(
    birthdate = mdy(birthdate)
  )


strip_name <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\.", "") |>
    str_replace_all("'", "")
}
merge <- tidylog::left_join(
  players |> mutate(join_name = strip_name(name)),
  stats |> mutate(join_name = strip_name(player)),
  by = "join_name"
) |>
  select(-join_name, -player)

# %%
fs::dir_create(here(
  "Projects/Cross_Sectional/data/nba_2k25_player_ratings/"
))
write_csv(
  merge,
  file = here(
    "Projects/Cross_Sectional/data/nba_2k25_player_ratings/players.csv"
  )
)
