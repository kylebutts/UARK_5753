library(tidyverse)
library(here)
set.seed(20240210)

N <- 250
age <- pmin(pmax(rnorm(n = N, mean = 38, sd = 13), 22), 67)
age <- round(age)
college <- sample(c(0, 1), size = N, prob = c(0.7, 0.3), replace = TRUE)
wage <- -30 + 10 * college + 3 * age - 1.5/45 * age^2 + rnorm(N, mean = 0, sd = 2)
# plot(age, wage)

df <- data.frame(
  age = age,
  college = college, 
  wage = wage
)
write_csv(
  df, here("03-Regression_Theory/r-day/wage_data_sim.csv")
)

