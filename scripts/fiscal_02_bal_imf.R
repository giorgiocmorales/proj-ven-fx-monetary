# Import fiscal balance indicators from IMF WEO

library(tidyverse)
library(rsdmx)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "fiscal_02_bal_imf"
source_id <- "imf"

indicator_map <- tribble(
  ~target_variable, ~flow_id, ~country, ~indicator_code, ~key_extra, ~frequency,
  "general_government_balance_lcu", "WEO", "VEN", "GGXCNL", NA_character_, "A",
  "general_government_balance_gdp", "WEO", "VEN", "GGXCNL_NGDP", NA_character_, "A",
  "general_government_primary_balance_lcu", "WEO", "VEN", "GGXONLB", NA_character_, "A",
  "general_government_primary_balance_gdp", "WEO", "VEN", "GGXONLB_NGDP", NA_character_, "A"
)

bal_imf <- fetch_imf_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(bal_imf, dataset_id)
