# Import global oil price benchmarks from IMF WEO

library(tidyverse)
library(rsdmx)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "oil_03_price_imf"
source_id <- "imf"

indicator_map <- tribble(
  ~target_variable, ~flow_id, ~country, ~indicator_code, ~key_extra, ~frequency,
  "oil_brent_price", "WEO", "G001", "POILBRE", NA_character_, "A",
  "oil_wti_price", "WEO", "G001", "POILWTI", NA_character_, "A",
  "oil_apsp_price", "WEO", "G001", "POILAPSP", NA_character_, "A"
)

oil_price_imf <- fetch_imf_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(oil_price_imf, dataset_id)
