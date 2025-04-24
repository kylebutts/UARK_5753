library(tidyverse)
library(here)
library(cranlogs)
downloads <- cranlogs::cran_downloads(
  package = "ggplot2",
  from = "2014-01-01",
  to = Sys.Date() - days(1)
)
downloads <- downloads |>
  select(package, date, downloads = count) 

write_csv(downloads, here("Problem_Sets/PS08-Prophet/data/ggplot2_downloads.csv"))
