library(tidyverse)
library(tinytable)

set.seed(20240810)
mse_ex <- tibble(
  y = round(runif(5, 0, 10), digits = 1),
  eps = round(rnorm(5, 0, 0.4), digits = 2)
) |>
  mutate(y_hat = y + eps)

tab_mspe_example <- mse_ex |>
  select(y, y_hat, eps) |>
  rename(
    `$y_i$` = y,
    `$\\hat{y}_i$` = y_hat,
    `$\\hat{\\varepsilon}_i$` = eps
  ) |>
  tt() |>
  format_tt(j = 3, fn = function(col) {
    sprintf("\\only<2>{%s}", col)
  }) |>
  theme_tt("striped") |>
  style_tt(tabularray_inner = "colsep = {1em}")

tab_mspe_example |>
  print("latex")

tab_mspe_example |>
  save_tt(here("02-Forecasting/tables/mspe_ex.tex"), overwrite = TRUE)

mspe_caculation_sum <- sprintf(
  "\\frac{1}{%s} \\left( %s \\right)",
  nrow(mse_ex),
  paste(sprintf("%s^2", mse_ex$eps), collapse = " + ")
)
mspe_calculation_string <- sprintf(
  "\\begin{align*}\n  \\text{MSPE} &= %s \\\\\n  &= %s\n\\end{align*}",
  mspe_caculation_sum,
  mean(mse_ex$eps^2)
)
cat(
  mspe_calculation_string,
  file = here("02-Forecasting/inputs/mspe_calculation_ex.tex")
)
