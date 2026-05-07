# Import income per-capita indicators from the World Bank API

library(tidyverse)
library(jsonlite)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "real_08_incpc_wb"
source_id <- "wb"

indicator_map <- tribble(
  ~target_variable, ~indicator_code,
  "gni_per_capita_atlas_usd", "NY.GNP.PCAP.CD",
  "gni_per_capita_current_lcu", "NY.GNP.PCAP.CN",
  "gni_per_capita_constant_usd", "NY.GNP.PCAP.KD",
  "gni_per_capita_growth", "NY.GNP.PCAP.KD.ZG",
  "gni_per_capita_ppp_current", "NY.GNP.PCAP.PP.CD",
  "gni_per_capita_ppp_constant", "NY.GNP.PCAP.PP.KD"
)

incpc_wb <- fetch_wb_indicator_set(
  dataset_id = dataset_id,
  source_id = source_id,
  indicator_map = indicator_map
)

write_api_source_result(incpc_wb, dataset_id)
