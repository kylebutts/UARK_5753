library(tidyverse)
library(ggridges)
library(kfbmisc)
library(here)

data(lincoln_weather, package = "ggridges")
(plot_dist <- ggplot() +
  geom_density(
    aes(x = `Mean Temperature [F]`),
    data = lincoln_weather,
    fill = kfbmisc::tailwind_color("zinc-200")
  ) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  scale_y_continuous(
    labels = NULL, breaks = NULL
  ) + 
  labs(title = "Temperatures in Lincoln NE", y = NULL) +
  kfbmisc::theme_kyle()
)


weather_with_overall <- bind_rows(
  lincoln_weather,
  lincoln_weather |> mutate(Month = "\\textbf{Overall}")
) |>
  mutate(
    Month = forcats::fct(
      Month, levels = c("\\textbf{Overall}", levels(lincoln_weather$Month))
    )
  )

(plot_month_dist <- ggplot() +
  geom_density_ridges_gradient(
    aes(x = `Mean Temperature [F]`, y = `Month`),
    data = weather_with_overall,
    scale = 2, rel_min_height = 0.01,
    fill = kfbmisc::tailwind_color("zinc-200")
  ) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(title = "Temperatures in Lincoln NE", y = NULL) +
  kfbmisc::theme_kyle()
)

kfbmisc::tikzsave(
  here("03-Regression_Theory/figures/temperature_cond_distribution.pdf"),
  plot_month_dist, width = 8, height = 4.5
)


