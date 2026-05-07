# Import reserve proxies from the World Bank API

library(tidyverse)
library(jsonlite)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "external_09_res_wb"
source_id <- "wb"

indicator_map <- tribble(
  ~target_variable, ~indicator_code,
  "reserves_related_items", "BN.RES.INCL.CD",
  "reserve_change", "BN.RES.LFAR.CD"
)

reserves_wb <- fetch_wb_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(reserves_wb, dataset_id)
