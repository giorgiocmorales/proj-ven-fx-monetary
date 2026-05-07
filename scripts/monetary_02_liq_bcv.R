# Extract liquidity data from BCV

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "monetary_02_liq_bcv"
source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/indicadores_sector_monetario/liquidez_monetaria_semanal1.xls"
raw_path <- file.path("data", "raw", "bcv_liq.xls")

downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
download_latest_file(source_url, raw_path)

liquidity_sheets <- bcv_excel_sheets(raw_path) %>%
  discard(~ str_detect(.x, regex("module", ignore_case = TRUE)))

liquidity_raw <- map_dfr(liquidity_sheets, function(sheet_name) {
  sheet <- read_bcv_sheet(raw_path, sheet = sheet_name, col_names = FALSE) %>%
    as_tibble(.name_repair = "unique")

  unit <- case_when(
    str_detect(sheet_name, "Oct2021") ~ "thousand_new_ves",
    str_detect(sheet_name, "2019-2021") ~ "million_ves",
    TRUE ~ "ves"
  )

  sheet %>%
    transmute(
      row_id = row_number(),
      date = parse_bcv_date(...1),
      provisional = is_bcv_provisional(...1),
      monedas_billetes = clean_bcv_numeric(...2),
      depositos_vista = clean_bcv_numeric(...3),
      depositos_ahorro_transferibles = clean_bcv_numeric(...4),
      dinero = clean_bcv_numeric(...5),
      cuasidinero = clean_bcv_numeric(...6),
      liquidez_monetaria = clean_bcv_numeric(...7),
      variacion_pct = clean_bcv_numeric(...8)
    ) %>%
    filter(!is.na(date)) %>%
    pivot_longer(
      cols = -c(row_id, date, provisional),
      names_to = "series_id",
      values_to = "value",
      values_transform = list(value = as.character)
    ) %>%
    filter(!is.na(value)) %>%
    mutate(
      dataset_id = dataset_id,
      source_id = source_id,
      source_url = source_url,
      sheet_name = sheet_name,
      frequency = "weekly",
      unit = if_else(series_id == "variacion_pct", "percent", unit),
      downloaded_at = downloaded_at
    )
})

write_processed_dataset(liquidity_raw, dataset_id)
