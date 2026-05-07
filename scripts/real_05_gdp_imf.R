# Import GDP and GDP per-capita indicators from IMF WEO

library(tidyverse)
library(rsdmx)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "real_05_gdp_imf"
source_id <- "imf"

indicator_map <- tribble(
  ~target_variable, ~flow_id, ~country, ~indicator_code, ~key_extra, ~frequency,
  "gdp_current_usd", "WEO", "VEN", "NGDPD", NA_character_, "A",
  "gdp_current_lcu", "WEO", "VEN", "NGDP", NA_character_, "A",
  "gdp_constant_lcu", "WEO", "VEN", "NGDP_R", NA_character_, "A",
  "gdp_real_growth", "WEO", "VEN", "NGDP_RPCH", NA_character_, "A",
  "gdp_per_capita_usd", "WEO", "VEN", "NGDPDPC", NA_character_, "A",
  "gdp_per_capita_constant_lcu", "WEO", "VEN", "NGDPRPC", NA_character_, "A",
  "gdp_per_capita_ppp", "WEO", "VEN", "PPPPC", NA_character_, "A"
)

gdp_imf <- fetch_imf_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(gdp_imf, dataset_id)
