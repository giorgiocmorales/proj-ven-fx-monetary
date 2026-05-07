# Import total GDP indicators from the World Bank API

library(tidyverse)
library(jsonlite)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "real_06_gdp_wb"
source_id <- "wb"

indicator_map <- tribble(
  ~target_variable, ~indicator_code,
  "gdp_current_usd", "NY.GDP.MKTP.CD",
  "gdp_current_lcu", "NY.GDP.MKTP.CN",
  "gdp_constant_usd", "NY.GDP.MKTP.KD",
  "gdp_real_growth", "NY.GDP.MKTP.KD.ZG",
  "gdp_ppp_current", "NY.GDP.MKTP.PP.CD",
  "gdp_ppp_constant", "NY.GDP.MKTP.PP.KD"
)

gdp_wb <- fetch_wb_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(gdp_wb, dataset_id)
