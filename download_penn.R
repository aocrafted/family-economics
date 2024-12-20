library(httr)

# base URL
base_url <- "https://www.pa.gov/content/dam/copapwp-pagov/en/health/documents/topics/healthstatistics/vitalstatistics/marriagedivorce/documents/Marriage_Divorce_"

save_dir <- file.path(getwd(), "raw_data")

if (!dir.exists(save_dir)) {
  dir.create(save_dir)
}

# function to download pdfs
download_pdf <- function(year, base_url, save_dir) {
  # construct url
  pdf_url <- paste0(base_url, year, ".pdf")
  
  # save in this path
  pdf_file <- file.path(save_dir, paste0("Marriage_Divorce_", year, ".pdf"))
  
  # download pdf
  if (!file.exists(pdf_file)) {
    response <- GET(pdf_url, write_disk(pdf_file, overwrite = TRUE))
    
    if (status_code(response) == 200) {
      cat("download succeeded:", pdf_file, "\n")
    } else {
      cat("download failed (ステータスコード:", status_code(response), "):", pdf_url, "\n")
    }
  } else {
    cat("skipped since it already exists:", pdf_file, "\n")
  }
}

# download by year
years <- 2005:2015
for (year in years) {
  download_pdf(year, base_url, save_dir)