library(tidyverse)
library(arrow)
library(here)
library(fs)
fs::dir_create(here("04-Regression_in_Practice/R-day/data/"))

set.seed(20250223)
parcels <- open_dataset(
  "~/Dropbox/Zoning-and-Housing-Supply/data/base/MA-parcels_panel_geocoded.parquet"
)

is_whole_number <- \(x) {
  return(x %% 1 == 0)
}
identical(
  is_whole_number(c(0.01, 1.0000, 3, 1.2)),
  c(FALSE, TRUE, TRUE, FALSE)
)


subset <- parcels |>
  filter(between(year_built, 1901, 2023)) |>
  filter(last_sale_year >= 1901) |>
  filter(total_value < 2e6) |>
  filter(building_value >= 1) |>
  filter(land_value >= 1) |>
  filter(between(use_code, 101, 110)) |>
  filter(between(lot_size_acres, 0.05, 1.5)) |>
  filter(between(building_area, 1, 10000)) |>
  filter(between(z_mls_acres, 0.01, 2)) |>
  filter(between(n_rooms, 3, 10), is_whole_number(n_rooms)) |>
  filter(n_stories <= 3, is_whole_number(n_stories)) |>
  select(-ends_with("per_acre")) |>
  collect()


# ggplot() +
#   geom_point(
#     aes(
#       x = longitude, y = latitude
#     ),
#     data = parcels |> collect() |> slice_sample(n = 100000)
#   ) +
#   kfbmisc::theme_kyle(base_size = 14)

sample <- subset |>
  select(
    fy,
    town,
    census_tract,
    use_code,
    lot_size_acres,
    building_value,
    land_value,
    other_value,
    total_value,
    last_sale_date,
    last_sale_year,
    last_sale_price,
    n_units,
    n_rooms,
    n_stories,
    style,
    year_built,
    building_area,
    residential_area,
    longitude,
    latitude,
    z_zoning_id,
    z_zoning_code,
    z_permit_2fam,
    z_permit_3_4fam,
    z_permit_5_19fam,
    z_permit_20_fam,
    z_dupac,
    z_max_height,
    z_mls_acres,
    z_residential,
    z_mixed,
    z_nonresidential
  ) |>
  collect()

write_parquet(
  sample,
  here("04-Regression_in_Practice/R-day/data/MA_parcels_sample.parquet")
)
