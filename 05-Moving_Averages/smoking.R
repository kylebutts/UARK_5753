# %%
library(tidyverse)
library(here)
data(california_prop99, package = "synthdid")

# %%
(p_smoking_california <- ggplot() +
  geom_line(
    aes(x = Year, y = PacksPerCapita, group = State, color = State),
    data = california_prop99 |>
      filter(State == "California"),
    linewidth = 1.25,
    # color = kfbmisc::kyle_color("magenta")
  ) +
  scale_y_continuous(limits = c(0, 160)) +
  scale_color_manual(
    values = c(
      "California" = "#002C55",
      "#5C4CBF",
      "#0188AC",
      "#2DB25F"
    )
  ) +
  labs(
    x = NULL,
    y = "Per-capita Cigarette Sales (packs)",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))

(p_smoking <- ggplot() +
  geom_line(
    aes(x = Year, y = PacksPerCapita, group = State, color = State),
    data = california_prop99 |>
      filter(
        State %in%
          c("California", "Arkansas", "Minnesota", "Colorado", "Virginia")
      ),
    linewidth = 1.25
  ) +
  scale_y_continuous(limits = c(0, 160)) +
  scale_color_manual(
    values = c(
      "California" = "#002C55",
      "Arkansas" = "#5C4CBF",
      "Minnesota" = "#0188AC",
      "Colorado" = "#2DB25F",
      "Virginia" = "#FB7185"
    )
  ) +
  labs(
    x = NULL,
    y = "Per-capita Cigarette Sales (packs)",
    color = NULL
  ) +
  kfbmisc::theme_kyle(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 5, 0),
    legend.justification = c(0, 1),
    legend.location = "plot"
  ))


# %%
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/smoking_california.pdf"),
  plot = p_smoking_california,
  width = 8,
  height = 4.5
)
kfbmisc::tikzsave(
  here("05-Moving_Averages/figures/smoking_a_few_states.pdf"),
  plot = p_smoking,
  width = 8,
  height = 4.5
)
