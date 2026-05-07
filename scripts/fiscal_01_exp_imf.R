# Import public expenditure indicators from IMF WEO

library(tidyverse)
library(rsdmx)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "fiscal_01_exp_imf"
source_id <- "imf"

indicator_map <- tribble(
  ~target_variable, ~flow_id, ~country, ~indicator_code, ~key_extra, ~frequency,
  "general_government_expenditure_lcu", "WEO", "VEN", "GGX", NA_character_, "A",
  "general_government_expenditure_gdp", "WEO", "VEN", "GGX_NGDP", NA_character_, "A"
)

exp_imf <- fetch_imf_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(exp_imf, dataset_id)
