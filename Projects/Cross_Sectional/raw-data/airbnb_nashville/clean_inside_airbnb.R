# %%
# https://insideairbnb.com/get-the-data/
library(tidyverse)
library(here)
raw <- read_csv(here(
  "Projects/Cross_Sectional/raw-data/airbnb_nashville/nashville_airbnb_listings_2024_10_15.csv"
))

airbnb <- raw |>
  select(
    name,
    description,
    host_since,
    host_response_time,
    host_response_rate,
    host_is_superhost,
    host_neighbourhood,
    room_type,
    price,
    bedrooms,
    beds,
    bathrooms,
    accommodates,
    has_availability,
    availability_30,
    number_of_reviews,
    review_scores_rating,
    review_scores_cleanliness,
    review_scores_accuracy,
    review_scores_checkin,
    review_scores_location,
    review_scores_communication,
    calculated_host_listings_count
  ) |>
  mutate(
    host_response_time = na_if(host_response_time, "N/A"),
    host_response_rate = na_if(host_response_rate, "N/A"),
    host_response_rate = parse_number(host_response_rate),
    price = parse_number(price)
  )


fs::dir_create(here("Projects/Cross_Sectional/data/airbnb_nashville/"))
write_csv(
  airbnb,
  file = here(
    "Projects/Cross_Sectional/data/airbnb_nashville/airbnb_nashville_listings.csv"
  )
)
