library(tidyverse)
library(readxl)
library(janitor)

divorce <- read_excel("raw_data/mass_divorce_1995-2020.xlsx")

columns <- paste0(2005:2015)
divorce_ma <- 
  divorce |> 
  select("County of Judgement", columns) |> 
  pivot_longer(cols = columns, names_to = "year", values_to = "divorce") |> 
  clean_names() |> 
  mutate(year = as.integer(year)) |> 
  rename(county = county_of_judgement,
         total_divorce = divorce) |> 
  filter(!(county %in% c("(blank)", "Grand Total"))) |> 
  mutate(county = str_to_title(county),
         state = "MA")

save(divorce_ma, file = "raw_data/divorce_ma.RData")

