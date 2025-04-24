# %% 
library(tidyverse)
library(fixest)
library(rvest)
library(here)
library(fs)

rows <- here("Problem_Sets/PS08-Prophet/data/bitcoin_daily.html") |>
  read_html() |>
  html_element("tbody") |>
  html_elements("tr")

# row <- rows[[1]]
parse_row <- function(row) {
  date <- row |>
    html_elements("td") |>
    _[[1]] |>
    html_text() |>
    mdy()

  price_open <- row |>
    html_elements("td") |>
    _[[2]] |>
    html_text() |>
    parse_number()

  price_high <- row |>
    html_elements("td") |>
    _[[3]] |>
    html_text() |>
    parse_number()

  price_low <- row |>
    html_elements("td") |>
    _[[4]] |>
    html_text() |>
    parse_number()

  price_close <- row |>
    html_elements("td") |>
    _[[5]] |>
    html_text() |>
    parse_number()

  price_adj_close <- row |>
    html_elements("td") |>
    _[[6]] |>
    html_text() |>
    parse_number()

  volume <- row |>
    html_elements("td") |>
    _[[7]] |>
    html_text() |>
    parse_number()

  tibble(
    date, price_open, price_high, price_low, price_close, price_adj_close, volume
  )
}

# %% 
# https://finance.yahoo.com/quote/BTC-USD/history/?period1=1410912000&period2=1732503478
bitcoin_prices <- map(rows, parse_row) |>
  list_rbind()

fs::dir_create(here("R_Labs/Lab_9/data/"))
write_csv(
  bitcoin_prices, 
  file = here("R_Labs/Lab_9/data/bitcoin_daily.csv")
)

# %% 
bitcoin_prices <- read_csv(here("Problem_Sets/PS08-Prophet/data/bitcoin_daily.csv"))
bitcoin_prices <- bitcoin_prices |>
  filter(date >= ymd("2022-01-01"))

ggplot() +
  geom_line(
    aes(x = date, y = price_close), 
    data = bitcoin_prices
  )

