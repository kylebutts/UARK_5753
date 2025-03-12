# %%
library(tidyverse)
library(httr2)
library(jsonlite)
library(dotenv)
library(here)
load_dot_env(file = here(".env"))

movies <- here("Projects/Cross_Sectional/raw-data/movies/movie_data.csv") |>
  read_csv(show_col_types = FALSE) |>
  select(-image_url, -imdb_link, -overview)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clean data ----
# process genres
genres <- jsonlite::fromJSON(paste0(
  "[",
  paste0(replace_na(movies$genres, '[]'), collapse = ", "),
  "]"
))
movies$genre_1 <- map_chr(
  genres,
  function(x) pluck(x, 1, .default = NA_character_)
)
movies$genre_2 <- map_chr(
  genres,
  function(x) pluck(x, 2, .default = NA_character_)
)
movies$genre_3 <- map_chr(
  genres,
  function(x) pluck(x, 3, .default = NA_character_)
)
movies$genre_4 <- map_chr(
  genres,
  function(x) pluck(x, 4, .default = NA_character_)
)
movies$genre_5 <- map_chr(
  genres,
  function(x) pluck(x, 5, .default = NA_character_)
)
movies$genres <- NULL

# process production countries
production_countries <- jsonlite::fromJSON(paste0(
  "[",
  paste0(replace_na(movies$production_countries, '[]'), collapse = ", "),
  "]"
))
movies$production_country_1 <- map_chr(
  production_countries,
  function(x) pluck(x, 1, .default = NA_character_)
)
movies$production_country_2 <- map_chr(
  production_countries,
  function(x) pluck(x, 2, .default = NA_character_)
)
movies$production_country_3 <- map_chr(
  production_countries,
  function(x) pluck(x, 3, .default = NA_character_)
)
movies$production_country_4 <- map_chr(
  production_countries,
  function(x) pluck(x, 4, .default = NA_character_)
)
movies$production_country_5 <- map_chr(
  production_countries,
  function(x) pluck(x, 5, .default = NA_character_)
)
movies$production_countries <- NULL

spoken_languages <- jsonlite::fromJSON(paste0(
  "[",
  paste0(replace_na(movies$spoken_languages, '[]'), collapse = ", "),
  "]"
))
movies$spoken_language_1 <- map_chr(
  spoken_languages,
  function(x) pluck(x, 1, .default = NA_character_)
)
movies$spoken_language_2 <- map_chr(
  spoken_languages,
  function(x) pluck(x, 2, .default = NA_character_)
)
movies$spoken_languages <- NULL

movies <- movies |>
  mutate(
    year_released = year_released |> na_if("null") |> parse_number(),
    vote_count = vote_count |> na_if("null") |> parse_number(),
    vote_average = vote_average |> na_if("null") |> parse_number(),
    popularity = popularity |> na_if("null") |> parse_number()
  )

movies <- movies |>
  filter(year_released >= 2010) |>
  filter(vote_count >= 10)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Add TMDB Data ----
get_tmdb_movie_info <- function(tmdb_id = 1064213) {
  url <- sprintf("https://api.themoviedb.org/3/movie/%s", tmdb_id)
  authorization = sprintf('Bearer %s', Sys.getenv("TMDB_KEY"))

  res <- request(url) |>
    req_headers(
      'Authorization' = authorization,
      'Content-Type' = 'application/octet-stream',
      'Accept' = 'application/json'
    ) |>
    req_url_query(language = "en-US") |>
    req_perform() |>
    resp_body_json()

  if (
    !is.null(res$production_companies) && length(res$production_companies) >= 1
  ) {
    company_name <- res$production_companies[[1]]$name
    company_country <- res$production_companies[[1]]$origin_country
  } else {
    company_name <- NA
    company_country <- NA
  }
  tibble(
    tmdb_movie_id = tmdb_id,
    tmdb_title = res$title,
    tmdb_runtime = res$runtime,
    tmdb_status = res$status,
    tmdb_is_adult_movie = res$adult,
    tmdb_popularity = res$popularity,
    tmdb_release_date = res$release_date,
    tmdb_budget = res$budget,
    tmdb_revenue = res$revenue,
    tmdb_vote_count = res$vote_count,
    tmdb_vote_average = res$vote_average,
    tmdb_production_company_name = company_name,
    tmdb_production_company_country = company_country
  )
}

scrape_tmdb_slowly_and_safely <- function(tmdb_id) {
  x <- possibly(
    slowly(
      get_tmdb_movie_info,
      rate = rate_delay(1 / 40) # https://developer.themoviedb.org/docs/rate-limiting
    ),
    otherwise = tibble(tmdb_movie_id = tmdb_id)
  )(tmdb_id)
  return(x)
}
# scrape_tmdb_slowly_and_safely(68333)

# %%
# temp <- movies |> slice(1:500)
tictoc::tic()
res <- list_rbind(map(
  movies$tmdb_id,
  function(id) scrape_tmdb_slowly_and_safely(id),
  .progress = TRUE
))
tictoc::toc()
movies <- bind_cols(movies, res)
movies$tmdb_release_date <- ymd(movies$tmdb_release_date)

# Clean up
movies <- movies |>
  select(-`_id`, -tmdb_movie_id, -movie_id) |>
  rename(
    letterboxd_vote_count = vote_count,
    letterboxd_vote_average = vote_average
  ) |>
  select(
    movie_title,
    tmdb_id,
    imdb_id,
    tmdb_link,
    year_released,
    runtime,
    release_date,
    letterboxd_vote_count,
    letterboxd_vote_average,
    popularity,
    everything()
  )

# %%
fs::dir_create(here("Projects/Cross_Sectional/data/movies/"))
write_csv(
  movies,
  file = here("Projects/Cross_Sectional/data/movies/movies.csv")
)
