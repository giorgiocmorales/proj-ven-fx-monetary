# Import public expenditure indicators from the World Bank API

library(tidyverse)
library(jsonlite)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "fiscal_03_exp_wb"
source_id <- "wb"

indicator_map <- tribble(
  ~target_variable, ~indicator_code,
  "expense_current_usd", "GC.XPN.TOTL.CD",
  "expense_current_lcu", "GC.XPN.TOTL.CN",
  "expense_gdp", "GC.XPN.TOTL.GD.ZS",
  "government_consumption_current_usd", "NE.CON.GOVT.CD",
  "government_consumption_constant_usd", "NE.CON.GOVT.KD",
  "government_consumption_gdp", "NE.CON.GOVT.ZS"
)

exp_wb <- fetch_wb_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(exp_wb, dataset_id)
