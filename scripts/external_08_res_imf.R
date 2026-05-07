# Import international reserves and foreign-currency liquidity from IMF

library(tidyverse)
library(rsdmx)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "external_08_res_imf"
source_id <- "imf"

indicator_map <- tribble(
  ~target_variable, ~flow_id, ~country, ~indicator_code, ~key_extra, ~frequency,
  "reserves_usd", "IRFCL", "VEN", "IRFCLDT1_IRFCL65_USD", "S1XS1311", "A"
)

reserves_imf <- fetch_imf_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(reserves_imf, dataset_id)
