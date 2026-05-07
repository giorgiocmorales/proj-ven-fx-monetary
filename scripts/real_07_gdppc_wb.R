# Import GDP per-capita indicators from the World Bank API

library(tidyverse)
library(jsonlite)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "real_07_gdppc_wb"
source_id <- "wb"

indicator_map <- tribble(
  ~target_variable, ~indicator_code,
  "gdp_per_capita_current_usd", "NY.GDP.PCAP.CD",
  "gdp_per_capita_constant_usd", "NY.GDP.PCAP.KD",
  "gdp_per_capita_growth", "NY.GDP.PCAP.KD.ZG",
  "gdp_per_capita_ppp_current", "NY.GDP.PCAP.PP.CD",
  "gdp_per_capita_ppp_constant", "NY.GDP.PCAP.PP.KD"
)

gdppc_wb <- fetch_wb_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(gdppc_wb, dataset_id)
