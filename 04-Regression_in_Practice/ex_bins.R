# %%
library(fixest)
library(tidyverse)
library(arrow)
library(binsreg)
library(here)
library(kfbmisc)

# %%
# set.seed(20240904)
# parcels <- open_dataset("/Users/kylebutts/Library/CloudStorage/Dropbox/Zoning-and-Housing-Supply/data/base/MA-parcels_panel_geocoded.parquet")
#
# sample <- parcels |>
#   filter(between(year_built, 1901, 2023)) |>
#   filter(total_value < 2e6) |>
#   filter(between(use_code, 101, 110)) |>
#   filter(between(n_rooms, 3, 8)) |>
#   filter(between(lot_size_acres, 0.05, 4)) |>
#   select(latitude, longitude, year_built, total_value, lot_size_acres, n_rooms) |>
#   collect() |>
#   slice_sample(n = 100000)
#
# write_parquet(sample, here("Slides/02_Regression/data/MA_parcels_sample.parquet"))

# %%
parcels <- read_parquet(here("04-Regression_in_Practice/data/MA_parcels_sample.parquet")) 

bins <- function(x, p, s) {
  # Calls `splines::splineDesign` internally
  design <- binsreg:::binsreg.spdes(eval = x, p = p, s = s, knot = knots, deriv = 0)
  return(design)
}

create_predict_grid <- function(knots, n_pts = 5) {
  x <- c(knots[1])
  id <- c(1)
  for (j in 2:(length(knots) - 1)) {
    x <- c(x, knots[j] - 0.001, knots[j])
    id <- c(id, j - 1, j)
  }
  x <- c(x, knots[length(knots)])
  id <- c(id, length(knots) - 1)

  grid <- tibble(bin_id = id, x = as.numeric(x))

  grid <- grid |>
    reframe(
      .by = bin_id,
      x = base::seq(from = as.numeric(x[1]), to = as.numeric(x[2]), length.out = n_pts)
    )
  return(grid)
}

knots <- knots_10 <- knots <- seq(1900, 2020, by = 10)

parcels <- parcels |>
  filter(year_built <= 2020) |>
  filter(total_value <= 1250000) |>
  mutate(
    bin = cut(year_built, knots, include.lowest = TRUE, right = TRUE)
  )


# %%
predictions <- create_predict_grid(knots, n_pts = 30) |>
  rename(year_built = x) |>
  mutate(
    bin = cut(year_built, knots, include.lowest = TRUE, right = TRUE)
  )

est <- feols(
  total_value ~ 0 + i(bin),
  data = parcels
)
predictions$y_hat <- predict(est, newdata = predictions)

est_linear <- feols(
  total_value ~ 1 + year_built,
  data = parcels
)
predictions$y_hat_linear <- predict(est_linear, newdata = predictions)

est_poly <- feols(
  total_value ~ 1 + poly(year_built, 5),
  data = parcels
)
predictions$y_hat_poly <- predict(est_poly, newdata = predictions)

est_bspline_p_1_s_0 <- feols(
  total_value ~ 1 + bins(year_built, p = 1, s = 0),
  data = parcels
)
predictions$y_hat_bspline_p_1_s_0 <- predict(est_bspline_p_1_s_0, newdata = predictions)

est_bspline_p_1_s_1 <- feols(
  total_value ~ 1 + bins(year_built, p = 1, s = 1),
  data = parcels
)
predictions$y_hat_bspline_p_1_s_1 <- predict(est_bspline_p_1_s_1, newdata = predictions)

est_bspline_p_2_s_2 <- feols(
  total_value ~ 1 + bins(year_built, p = 2, s = 2),
  data = parcels
)
predictions$y_hat_bspline_p_2_s_2 <- predict(est_bspline_p_2_s_2, newdata = predictions)

knots <- knots_40 <- seq(1900, 2020, by = 40)
est_bspline_few_knots <- feols(
  total_value ~ 1 + bins(year_built, p = 2, s = 2),
  data = parcels
)
predictions$y_hat_bspline_few_knots <- predict(est_bspline_few_knots, newdata = predictions)


knots <- knots_opt <- binsregselect(
  y = parcels$total_value,
  x = parcels$year_built
)$knot
knots[1] <- knots_opt[1] <- 1900
est_binsreg_opt <- feols(
  total_value ~ 1 + bins(year_built, p = 2, s = 2),
  data = parcels
)
predictions$y_hat_binsreg_opt <- predict(est_binsreg_opt, newdata = predictions)





# FIGURES ----------------------------------------------------------------------
# %%
(p_raw <- ggplot() +
  geom_point(
    aes(x = year_built, y = total_value),
    data = parcels |> slice_sample(n = 5000),
    alpha = 0.1, shape = 20
    # data = parcels,
    # alpha = 0.05, shape = 20
  ) +
  labs(x = "Year Built", y = "Value of Property (\\$100K)") +
  scale_y_continuous(
    labels = scales::label_currency(
      prefix = "\\$", scale_cut = scales::cut_short_scale()
    ),
    expand = expansion(0, c(0, 0.01))
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    axis.title = element_text(size = rel(1), margin = margin(b = 10, unit = "pt"))
  )
)

# %%
(p_split_into_bins <- p_raw +
  geom_vline(
    xintercept = knots_10,
    color = kfbmisc::tailwind_color("zinc-800"),
    linewidth = 1,
    linetype = "dashed"
  )
)

# %%
(p_bins <- p_split_into_bins +
  geom_line(
    aes(x = year_built, y = y_hat, group = bin_id),
    data = predictions |> slice(.by = bin_id, 2:(n()- 1)),
    color = kfbmisc::kyle_color("blue"),
    linewidth = 1.5
  )
)

# %%
(p_add_linear <- p_bins +
  geom_line(
    aes(x = year_built, y = y_hat_linear),
    data = predictions,
    color = kfbmisc::kyle_color("purple"),
    linewidth = 1.5
  )
)

(p_add_poly <- p_add_linear +
  geom_line(
    aes(x = year_built, y = y_hat_poly),
    data = predictions,
    color = kfbmisc::kyle_color("yellow"),
    linewidth = 1.5
  )
)

# %%
(p_bspline_p_1_s_0 <- p_bins +
  geom_line(
    aes(x = year_built, y = y_hat_bspline_p_1_s_0, group = bin_id),
    data = predictions |> slice(.by = bin_id, 2:(n()- 1)),
    color = kfbmisc::kyle_color("green"),
    linewidth = 1.5
  ) + 
  labs(
    title = "{\\color[HTML]{0188AC} $p = 0$, $s = 0$};\\quad {\\color[HTML]{2DB25F} $p = 1$, $s = 0$}"
  )
)

(p_bspline_p_1_s_1 <- p_bspline_p_1_s_0 +
  geom_line(
    aes(x = year_built, y = y_hat_bspline_p_1_s_1),
    data = predictions |> slice(.by = bin_id, 2:(n()- 1)),
    color = kfbmisc::kyle_color("navy"),
    linewidth = 1.5
  ) + 
  labs(
    title = "{\\color[HTML]{0188AC} $p = 0$, $s = 0$};\\quad {\\color[HTML]{2DB25F} $p = 1$, $s = 0$};\\quad {\\color[HTML]{002C55} $p = 1$, $s = 1$}"
  )
)

(p_bspline_p_2_s_2 <- p_bspline_p_1_s_1 +
  geom_line(
    aes(x = year_built, y = y_hat_bspline_p_2_s_2),
    data = predictions |> slice(.by = bin_id, 2:(n()- 1)),
    color = kfbmisc::kyle_color("rose"),
    linewidth = 1.5
  ) + 
  labs(
    title = "{\\color[HTML]{0188AC} $p = 0$, $s = 0$};\\quad {\\color[HTML]{2DB25F} $p = 1$, $s = 0$};\\quad {\\color[HTML]{002C55} $p = 1$, $s = 1$};\\quad {\\color[HTML]{FB7185} $p = 2$, $s = 2$}"
  )
)

# %% 
(p_bspline_few_knots <- p_raw +
  geom_vline(
    xintercept = knots_40,
    color = kfbmisc::tailwind_color("zinc-800"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_line(
    aes(x = year_built, y = y_hat_bspline_p_2_s_2),
    data = predictions |> slice(.by = bin_id, 2:(n()- 1)),
    color = kfbmisc::kyle_color("rose"),
    linewidth = 1.5
  ) + 
  geom_line(
    aes(x = year_built, y = y_hat_bspline_few_knots),
    data = predictions |> slice(.by = bin_id, 2:(n()- 1)),
    color = kfbmisc::kyle_color("magenta"),
    linewidth = 1.5
  ) + 
  labs(
    title = "{\\color[HTML]{FB7185} knots every 10 years}; {\\color[HTML]{B3114B} knots every 40 years}"
  )
)

# %% 
(p_binsreg <- p_raw +
  geom_vline(
    xintercept = knots_opt,
    color = kfbmisc::tailwind_color("zinc-800"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_line(
    aes(x = year_built, y = y_hat_bspline_p_2_s_2),
    data = predictions |> slice(.by = bin_id, 2:(n()- 1)),
    color = kfbmisc::kyle_color("rose"),
    linewidth = 1.5
  ) + 
  geom_line(
    aes(x = year_built, y = y_hat_binsreg_opt),
    data = predictions |> slice(.by = bin_id, 2:(n()- 1)),
    color = kfbmisc::kyle_color("yellow"),
    linewidth = 1.5
  ) + 
  labs(
    title = "{\\color[HTML]{FB7185} knots every 10 years}; {\\color[HTML]{ffc517} binscatter selected}"
  )
)


# %%
kfbmisc::tikzsave(
  here::here("04-Regression_in_Practice/figures/ex_bins_raw.pdf"),
  plot = p_raw, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("04-Regression_in_Practice/figures/ex_bins_split.pdf"),
  plot = p_split_into_bins, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("04-Regression_in_Practice/figures/ex_bins_estimate.pdf"),
  plot = p_bins, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("04-Regression_in_Practice/figures/ex_bins_compare_to_linear.pdf"),
  plot = p_add_linear, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("04-Regression_in_Practice/figures/ex_bins_add_polynomial.pdf"),
  plot = p_add_poly, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("04-Regression_in_Practice/figures/ex_bspline_p_1_s_0.pdf"),
  plot = p_bspline_p_1_s_0, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("04-Regression_in_Practice/figures/ex_bspline_p_1_s_1.pdf"),
  plot = p_bspline_p_1_s_1, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("04-Regression_in_Practice/figures/ex_bspline_p_2_s_2.pdf"),
  plot = p_bspline_p_2_s_2, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("04-Regression_in_Practice/figures/ex_bspline_few_knots.pdf"),
  plot = p_bspline_few_knots, width = 8, height = 4.2
)
kfbmisc::tikzsave(
  here::here("04-Regression_in_Practice/figures/ex_bspline_binscatter.pdf"),
  plot = p_binsreg, width = 8, height = 4.2
)




