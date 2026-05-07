# Extract monetary base data from BCV

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "monetary_01_mb_bcv"
source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/indicadores_sector_monetario/base_monetaria_semanal.xls"
raw_path <- file.path("data", "raw", "bcv_mb.xls")

downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
download_latest_file(source_url, raw_path)

monetary_sheets <- bcv_excel_sheets(raw_path)

monetary_base_raw <- map_dfr(monetary_sheets, function(sheet_name) {
  sheet <- read_bcv_sheet(raw_path, sheet = sheet_name, col_names = FALSE) %>%
    as_tibble(.name_repair = "unique")

  date_map <- sheet[6, -1] %>%
    unlist(use.names = TRUE) %>%
    enframe(name = "col_name", value = "date_raw") %>%
    mutate(
      date = parse_bcv_date(date_raw),
      provisional = is_bcv_provisional(date_raw)
    ) %>%
    filter(!is.na(date)) %>%
    select(col_name, date, provisional)

  unit <- if_else(
    str_detect(sheet_name, regex("Oct|2022|2023|2024|2025|2026", ignore_case = TRUE)),
    "thousand_new_ves",
    "ves"
  )

  sheet %>%
    mutate(
      row_id = row_number(),
      series_label = clean_bcv_text(...1)
    ) %>%
    filter(row_id > 6, !is.na(series_label)) %>%
    select(row_id, series_label, all_of(date_map$col_name)) %>%
    pivot_longer(
      cols = all_of(date_map$col_name),
      names_to = "col_name",
      values_to = "value",
      values_transform = list(value = as.character)
    ) %>%
    left_join(date_map, by = "col_name") %>%
    mutate(
      value = clean_bcv_numeric(value),
      dataset_id = dataset_id,
      source_id = source_id,
      source_url = source_url,
      sheet_name = sheet_name,
      frequency = "weekly",
      unit = unit,
      downloaded_at = downloaded_at
    ) %>%
    filter(!is.na(value))
})

write_processed_dataset(monetary_base_raw, dataset_id)
