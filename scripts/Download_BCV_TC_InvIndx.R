# Extract VES/USD data from BCV (Indice de inversion)

# Load packages ----------
library(rvest)
library(tidyverse)
library(lubridate)
library(purrr)
library(glue)

#Functions ----------------

# Scrapes a single page from BCV's investment index site ---------------
scrape_bcv_fx_page <- function(page_number) {
  # Construct full URL for the page
  url <- glue("https://www.bcv.org.ve/estadisticas/indice-de-inversion?page={page_number}")
  
  # Read and parse the table from the page
  raw_table <- read_html(url) %>%
    html_element("table") %>%
    html_table()
  
  # Clean and convert relevant columns
  clean_table <- raw_table %>%
    select(fecha = 1, tasa = 2) %>%
    mutate(
      fecha = lubridate::dmy(fecha),
      tasa = str_replace_all(tasa, "\\.", "") %>%
        str_replace(",", ".") %>%
        as.numeric()
    )
  
  return(clean_table)
}

# Identify max number of pages from BCV website (manual) -----------------
max_page <- 27

# Scrape all pages  -----------
bcv_scraped_data <- map_dfr(0:max_page, function(i) {
  message(glue("→ Scraping page {i + 1} of {max_page + 1}"))
  scrape_bcv_fx_page(i)
})

# Remove duplicates, just in case -----------
bcv_scraped_data <- bcv_scraped_data %>%
  distinct(fecha, .keep_all = TRUE) %>%  # Drop any repeated dates
  arrange(fecha)                         # Sort chronologically

# Save -------------
write_csv(bcv_scraped_data, "data/output/ves_usd_fx_indx.csv")

# clean Up -------
rm(bcv_scraped_data, max_page, scrape_bcv_fx_page)