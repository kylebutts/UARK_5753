# %%
library(tidyverse)
library(ISLR)
library(ISLR2)
library(here)
library(kfbmisc)
library(patchwork)
library(fixest)
library(FNN)
library(tinytable)

advertising <- read_csv(here("04-Regression_in_Practice/data/Advertising.csv"))

# %%
plot_sales_vs_tv <- advertising |>
  ggplot(aes(x = TV, y = Sales)) +
  geom_point(shape = 21) +
  geom_smooth(
    method = stats::lm,
    formula = y ~ x,
    se = FALSE,
    color = "#5601A5"
  ) +
  kfbmisc::theme_kyle(base_size = 12)

plot_sales_vs_radio <- advertising |>
  ggplot(aes(x = Radio, y = Sales)) +
  geom_point(shape = 21) +
  geom_smooth(
    method = stats::lm,
    formula = y ~ x,
    se = FALSE,
    color = "#5601A5"
  ) +
  kfbmisc::theme_kyle(base_size = 12)

plot_sales_vs_news <- advertising |>
  ggplot(aes(x = Newspaper, y = Sales)) +
  geom_point(shape = 21) +
  geom_smooth(
    method = stats::lm,
    formula = y ~ x,
    se = FALSE,
    color = "#5601A5"
  ) +
  kfbmisc::theme_kyle(base_size = 12)

(plot_sales_bivariate <- (plot_sales_vs_tv |
  plot_sales_vs_radio |
  plot_sales_vs_news) +
  plot_layout(axes = 'collect'))

kfbmisc::tikzsave(
  here("04-Regression_in_Practice/figures/sales_bivariate.pdf"),
  plot_sales_bivariate,
  width = 8,
  height = 3.5
)


# %%
render_tcblisting <- function(x) {
  sq <- rlang::quo_squash(rlang::enquo(x))
  str <- paste(as.character(sq)[-1], collapse = "\n")
  cat(str)

  tf <- here::here("render_tcblisting_temp.R")
  on.exit({
    files <- fs::dir_ls(here::here(), regexp = "render_tcblisting_temp")
    fs::file_delete(files)
  })
  cat(str, file = tf)
  styler::style_file(path = tf, strict = TRUE, base_indention = 0L)

  # Run reprex
  reprex <- reprex::reprex(
    input = tf,
    venue = "r",
    render = TRUE,
    advertise = FALSE,
    tidyverse_quiet = TRUE,
    html_preview = FALSE
  )

  reprex <- paste0(reprex, collapse = "\n")

  out_str <- sprintf("\\begin{codeblock}\n%s\n\\end{codeblock}", reprex)
  clipr::write_clip(out_str)
}

# %%
cat(render_tcblisting(x = {
  library(tidyverse)
  advertising <- read_csv(
    here::here("04-Regression_in_Practice/data/Advertising.csv"),
    show_col_types = FALSE
  )
  library(fixest)
  feols(Sales ~ TV + Newspaper + Radio, data = advertising)
}))


# %%
etable(
  feols(Sales ~ TV, advertising),
  feols(Sales ~ Radio, advertising, vcov = "HC1"),
  feols(Sales ~ Newspaper, advertising, vcov = "HC1"),
  feols(Sales ~ TV + Radio + Newspaper, advertising, vcov = "HC1"),
  headers = NA,
  tex = TRUE,
  fitstat = c("n", "r2")
)
