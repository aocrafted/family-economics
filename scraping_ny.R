## scrape data and wrangle

library(httr)
library(rvest)
library(tidyverse)
library(janitor)

# URL
base_url <- "https://www.health.ny.gov/statistics/vital_statistics/"

# target years and tables
years <- 2005:2015
table_names <- list(
  # table01a = "pop_fem",
  table02 = "pop_all",
  table50 = "divorce_ground",
  table51 = "divorce_duration"
)

# fetch data
fetch_table_data <- function(year, table_name, file_prefix
                             #, save_dir
                             ) {
  # construct URL
  url <- paste0(base_url, year, "/", table_name, ".htm")
  
  # HTTP request
  response <- GET(url, 
                  user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36"),
                  add_headers(Referer = base_url))
  
  # check HTTP status code
  if (status_code(response) != 200) {
    cat("HTTP Error: ", url, "\n")
    return(NULL)
  }
  
  page <- content(response, as = "text") |> read_html()
  
  # extract table01a
  table_node <- page |> html_element("table")
  if (is.null(table_node)) {
    cat("No table found in the page - ", url, "\n")
    return(NULL)
  }
  
  data <- table_node |> html_table() |> clean_names() |> as_tibble()
  
  # name data
  object_name <- paste0(file_prefix, "_", year) # eg: "divorce_duration_2013"
  assign(object_name, data, envir = .GlobalEnv)
  
  # # create a data directory
  # if (!dir.exists(save_dir)) {
  #   dir.create(save_dir)
  # }
  # 
  # # save csv
  # csv_file <- file.path(save_dir, paste0(object_name, ".csv"))
  # write.csv(data, csv_file, row.names = FALSE)
  # 
  # # save RData
  # rdata_file <- file.path(save_dir, paste0(object_name, ".RData"))
  # saveRDS(data, rdata_file)
  # 
  # cat("Data saved: Object name - ", object_name, ", CSV - ", csv_file, ", RData - ", rdata_file, "\n")
}

# save directory
# save_dir <- file.path(getwd(), "raw_data")

# for all year and table
for (year in years) {
  for (table_name in names(table_names)) {
    file_prefix <- table_names[[table_name]] 
    fetch_table_data(year, table_name, file_prefix
                     # , save_dir
                     )
  }
}


# wrangling
# divorce by marriage duration data
custom_operations <- function(data) {
  data |> 
    select(-any_of("county_2")) |>          
    rename(county = any_of("x"),
           after_separation_by_agreement = any_of("after_separation_by_agreenment")) |>         
    mutate(across(-county, ~ as.integer(str_remove_all(., ","))))
}

years <- 2005:2015

divorce_duration <- years |> 
  map_dfr(~ {
    get(paste0("divorce_duration_", .x)) |> 
      row_to_names(row_number = 1) |>       
      clean_names() |>                      
      custom_operations() |>                
      mutate(year = .x) |>                  
      filter(!(county %in% c("New York State", 
                             "New York City",
                             "",
                             "Rest of State"))) 
  }) |> 
  bind_rows() |> 
  mutate(county = str_remove_all(county, "\\+"),
         county = str_trim(county))

# divorce by grounds
divorce_grounds <- 
  years |> 
  map_dfr(~ {
    get(paste0("divorce_ground_", .x)) |> 
      row_to_names(row_number = 1) |>       
      clean_names() |>                      
      custom_operations() |>                
      mutate(year = .x) |>                  
      filter(!(county %in% c("New York State", 
                             "New York City",
                             "",
                             "Rest of State"))) 
  }) |> 
  bind_rows() |> 
  mutate(county = str_remove_all(county, "\\+"),
         county = str_trim(county))

# total population
# total_population <- 
#   years |> 
#   map_dfr(~ {
#     get(paste0("pop_all_", .x)) |> 
#       clean_names() |> 
#       rename_with(~ "population_estimate", matches(paste0("x", .x, "_population_estimate"))) |> 
#       mutate(
#         across(-county, ~ as.integer(str_remove_all(., ","))),
#         year = .x) |> 
#       mutate(
#         county = str_replace_all(county,                   
#                                  c("Colmbia" = "Columbia", 
#                                    "Genessee" = "Genesee"))) |> 
#       filter(
#         !(county %in% c(
#           "New York State", 
#           "New York City",
#           "",
#           "Rest of State",
#           "* Population per square mile",
#           "N.Y. State"
#         ))
#       )}) |> 
#   select(county, population_estimate, year) |> 
#   mutate(county = str_remove_all(county, "\\+"))


divorce_ny <-
  divorce_duration |>
  full_join(divorce_grounds, join_by(year, county)) |>
  # full_join(total_population, join_by(year, county)) |> 
  select(-total.y) |> 
  rename(total_divorce = total.x,
         duration_not_stated = not_stated.x,
         ground_not_stated = not_stated.y) |> 
  mutate(state = "NY")

save(divorce_ny, file = "raw_data/divorce_ny.RData")
