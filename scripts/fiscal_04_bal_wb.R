# Import fiscal balance indicators from the World Bank API

library(tidyverse)
library(jsonlite)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "fiscal_04_bal_wb"
source_id <- "wb"

indicator_map <- tribble(
  ~target_variable, ~indicator_code,
  "cash_surplus_deficit_current_usd", "GC.BAL.CASH.CD",
  "cash_surplus_deficit_current_lcu", "GC.BAL.CASH.CN",
  "cash_surplus_deficit_gdp", "GC.BAL.CASH.GD.ZS",
  "net_lending_borrowing_current_lcu", "GC.NLD.TOTL.CN",
  "net_lending_borrowing_gdp", "GC.NLD.TOTL.GD.ZS"
)

bal_wb <- fetch_wb_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(bal_wb, dataset_id)
