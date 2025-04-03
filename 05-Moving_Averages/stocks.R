# %%
library(tidyverse)
library(here)
library(quantmod)
library(tidyquant)

# Creates `AAPL` in environment
getSymbols("AAPL", warnings = FALSE, auto.assign = TRUE)
df <- AAPL |>
  as.data.frame() |>
  as_tibble(rownames = "date") |>
  janitor::clean_names() |>
  rename_with(
    function(x) str_replace(x, "aapl_", "")
  ) |>
  mutate(
    date = date(date)
  )

# %%
(p_apple_stock <- ggplot() +
  geom_line(
    aes(x = date, y = open),
    data = df,
    linewidth = 1.25,
    color = kfbmisc::kyle_color("magenta")
  ) +
  scale_x_date() +
  labs(
    x = NULL,
    y = "Apple Stock Price (\\$)"
  ) +
  kfbmisc::theme_kyle(base_size = 14))

# Smoothing average
# %%

# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/apple_stock.pdf"),
  plot = p_apple_stock,
  width = 8,
  height = 4.5
)
