# %%
library(tidyverse)
library(here)
library(tinytable)
library(glue)

# %%
# # get dates
# library(tidyverse)
# first_day <- ymd("2025-01-14")
# last_day <- ymd("2025-05-01")
# spring_break <- c(ymd("2025-02-25"), ymd("2025-02-27"))
# 
# blank_cal <- seq(first_day, last_day, by = "day") |>
#   enframe(name = NULL, value = "day") |>
#   mutate(
#     day_of_week = wday(day, label = TRUE, abbr = FALSE),
#     isoweek = isoweek(day) 
#   ) |>
#   filter(day_of_week %in% c("Tuesday", "Thursday")) |>
#   filter(!(day %in% spring_break)) |>
#   mutate(value = "") |>
#   mutate(
#     .by = isoweek,
#     Dates = paste0(
#       sprintf("%02d/%02d", month(day), day(day)),
#       collapse = " - "
#     )
#   ) |>
#   mutate(
#     .by = isoweek,
#     Week = cur_group_id()
#   ) |>
#   select(Week, Dates, name = day_of_week, value)
# 
# write_csv(
#   blank_cal, here("Syllabus/schedule.csv")
# )

# %%
cal <- here("Syllabus/schedule.csv") |>
  read_csv(show_col_types = FALSE) |>
  pivot_wider(names_from = "name", values_from = "value") |>
  mutate(across(c(Tuesday, Thursday), \(x) replace_na(x, ""))) |>
  mutate(across(
    c(Tuesday, Thursday),
    \(x) str_replace(x, "<br\\s?/>", " \\\\newline ")
  ))


# %%
options(tinytable_theme_placement_latex_float = "H")
tab <- cal |>
  tt(
    width = c(0.1, 0.2, 0.3, 0.3),
    caption = "Tentative Schedule"
  )

print(tab, "html")

# %%
save_tt(tab, here("Syllabus/schedule.tex"), overwrite = TRUE)

# %%
# Write to README.md
readme <- xfun::read_utf8(here("README.md"))
insert_idx <- which(str_detect(readme, "<!-- Schedule -->"))

tab_md_string <- cal |>
  tt() |>
  tinytable:::build_tt("gfm") |>
  (\(x) x@table_string)() |>
  str_split_1("\n")

# Hacky way to change table text
if (insert_idx[2] > insert_idx[1] + 1) {
  rows_to_keep <- setdiff(seq_len(length(readme)), (insert_idx[1] + 1):(insert_idx[2] - 1))
  readme <- readme[rows_to_keep]
}
readme <- append(readme, tab_md_string, after = insert_idx[1])

xfun::write_utf8(readme, here("README.md"))


# %%
# tribble(
#   ~Assignments, ~`Percent of grade`,
#   "Homework", "35\\%",
#   "Midterm", "20\\%",
#   "Midterm", "20\\%",
#   "Final", "25\\%",
# ) |>
#   tt() |>
#   print("gfm")
