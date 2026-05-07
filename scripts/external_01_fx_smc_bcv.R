# Extract VES/USD data from BCV (TC Referencia del Sistema Mercado Cambiario)

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)
library(glue)

source("R/source_helpers.R")

# Clean up ----
rm(list = ls())

# Helper functions ------

# Ensure required folders exist
source("R/source_helpers.R")
ensure_project_dirs()

# Normalize text to reduce encoding/accent fragility in pattern matching
normalize_text <- function(x) {
  x %>%
    as.character() %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    tolower() %>%
    str_squish()
}

extract_date_from_text <- function(x) {
  str_extract(x, "\\d{2}/\\d{2}/\\d{4}") %>% dmy()
}

# Download to data/raw/
download_bcv_file <- function(url, max_attempts = 3) {
  file_name <- basename(url)
  dest_path <- file.path("data/raw", file_name)

  timeout_old <- getOption("timeout")
  on.exit(options(timeout = timeout_old), add = TRUE)
  options(timeout = 120)

  for (attempt in seq_len(max_attempts)) {
    ok <- tryCatch(
      {
        download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE, method = "libcurl")
        TRUE
      },
      error = function(e) FALSE
    )

    if (ok && file.exists(dest_path) && file.info(dest_path)$size > 0) {
      return(dest_path)
    }

    unlink(dest_path)
    if (attempt < max_attempts) {
      Sys.sleep(1.5 * attempt)
    }
  }

  stop(glue("Download failed after {max_attempts} attempts: {basename(url)}"))
}

# Extract currency rows from a single sheet
extract_fx_from_sheet <- function(sheet, file_path, currencies = c("USD", "EUR")) {
  df <- read_excel(file_path, sheet = sheet, col_names = FALSE)
  if (ncol(df) < 6) return(NULL)

  # Keep expected column window while allowing variable row counts.
  df <- df %>% select(1:6)

  col_1 <- df$`...1`
  col_3 <- df$`...3`

  col_1_norm <- normalize_text(col_1)
  col_3_norm <- normalize_text(col_3)

  fecha_operacion_raw <- col_1[grepl("fecha operacion", col_1_norm, ignore.case = TRUE)][1]
  fecha_valor_raw <- col_3[grepl("fecha valor", col_3_norm, ignore.case = TRUE)][1]

  fecha_operacion <- extract_date_from_text(fecha_operacion_raw)
  fecha_valor <- extract_date_from_text(fecha_valor_raw)

  if (is.na(fecha_operacion) || is.na(fecha_valor)) return(NULL)

  currency_rows <- which(str_to_upper(trimws(as.character(df$`...1`))) %in% currencies)
  if (length(currency_rows) == 0) return(NULL)

  result <- map_dfr(currency_rows, function(row_id) {
    bid <- suppressWarnings(as.numeric(df$`...5`[row_id]))
    ask <- suppressWarnings(as.numeric(df$`...6`[row_id]))

    if (is.na(bid) || is.na(ask)) return(NULL)

    tibble(
      sheet_id = sheet,
      currency = str_to_upper(trimws(as.character(df$`...1`[row_id]))),
      fecha_operacion = fecha_operacion,
      fecha_valor = fecha_valor,
      bid = bid,
      ask = ask
    )
  })

  result
}

# Extract all sheets
extract_fx_from_file <- function(file_path) {
  sheets <- excel_sheets(file_path)
  map_dfr(sheets, extract_fx_from_sheet, file_path = file_path)
}

# Process one BCV file: download -> extract -> delete
process_bcv_file <- function(url, database_id) {
  file_path <- tryCatch(
    {
      download_bcv_file(url)
    },
    error = function(e) {
      message(glue("Download failed: {basename(url)} - skipping"))
      return(NULL)
    }
  )

  if (is.null(file_path)) return(NULL)

  result <- tryCatch(
    extract_fx_from_file(file_path),
    error = function(e) {
      message(glue("XLS parse failed: {basename(file_path)} - check manually"))
      file.copy(file_path, file.path("data/manual_fix", basename(file_path)), overwrite = TRUE)
      return(NULL)
    }
  )

  if (!is.null(result) && nrow(result) > 0) {
    result$database_id <- database_id
  }

  unlink(file_path)
  return(result)
}

# Main execution block -------------

# URL + ID list
bcv_files <- tribble(
  ~url, ~database_id,
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2a20_smc.xls", "2020Q1",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b20_smc.xls", "2020Q2",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c20_smc.xls", "2020Q3",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2d20_smc.xls", "2020Q4",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2a21_smc_58.xls", "2021Q1",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b21_smc.xls", "2021Q2",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c21_smc.xls", "2021Q3",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2d21_smc.xls", "2021Q4",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2a22_smc.xls", "2022Q1",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b22_smc.xls", "2022Q2",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c22_smc.xls", "2022Q3",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2d22_smc.xls", "2022Q4",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2a23_smc.xls", "2023Q1",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b23_smc.xls", "2023Q2",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c23_smc_60.xls", "2023Q3",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2d23_smc.xls", "2023Q4",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2a24_smc.xls", "2024Q1",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b24_smc.xls", "2024Q2",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c24_smc.xls", "2024Q3",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2d24_smc.xls", "2024Q4",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2a25_smc.xls", "2025Q1",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b25_smc.xls", "2025Q2",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c25_smc.xls", "2025Q3",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2d25_smc.xls", "2025Q4",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2a26_smc.xls", "2026Q1",
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b26_smc.xls", "2026Q2"
  
)

# Initialize result
ves_fx_bcv <- tibble()

# Loop over all files
for (i in seq_len(nrow(bcv_files))) {
  message(glue("Processing {bcv_files$database_id[i]}"))
  fx_data <- process_bcv_file(bcv_files$url[i], bcv_files$database_id[i])
  if (!is.null(fx_data) && nrow(fx_data) > 0) {
    ves_fx_bcv <- bind_rows(ves_fx_bcv, fx_data)
  }
}

# Manual fix files ------------

# Helper function
process_fixed_file <- function(filepath, database_id) {
  sheets <- excel_sheets(filepath)
  df <- map_dfr(sheets, extract_fx_from_sheet, file_path = filepath)
  if (!is.null(df) && nrow(df) > 0) {
    df$database_id <- database_id
  }
  return(df)
}

manual_fix_files <- tribble(
  ~filepath, ~database_id,
  "data/manual_fix/2_1_2d21_smc.xlsx", "2021Q4",
  "data/manual_fix/2_1_2c22_smc.xlsx", "2022Q3",
  "data/manual_fix/2_1_2d22_smc.xlsx", "2022Q4",
  "data/manual_fix/2_1_2a23_smc.xlsx", "2023Q1",
  "data/manual_fix/2_1_2b23_smc.xlsx", "2023Q2",
  "data/manual_fix/2_1_2c23_smc_60.xlsx", "2023Q3"
)

for (i in seq_len(nrow(manual_fix_files))) {
  fp <- manual_fix_files$filepath[i]
  db <- manual_fix_files$database_id[i]
  if (!file.exists(fp)) {
    message(glue("Manual fix file missing: {fp} - skipping"))
    next
  }
  fx_data <- process_fixed_file(fp, db)
  if (!is.null(fx_data) && nrow(fx_data) > 0) {
    ves_fx_bcv <- bind_rows(ves_fx_bcv, fx_data)
  }
}

# Arrange
ves_fx_bcv <- ves_fx_bcv %>%
  distinct(fecha_valor, currency, database_id, .keep_all = TRUE) %>%
  arrange(fecha_valor, currency)

# Check databases order
unique(ves_fx_bcv$database_id) %>% sort()

# Save file
write_csv(ves_fx_bcv, "data/processed/external_01_fx_smc_bcv.csv")

# Update current (incomplete) quarter -------------

# Load csv
ves_fx_bcv <- read_csv("data/processed/external_01_fx_smc_bcv.csv", show_col_types = FALSE)

url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b26_smc.xls"
database_id <- "2026Q2"

# Process fresh data
fx_data <- process_bcv_file(url, database_id)

# Overwrite current quarter only if fresh pull succeeded
if (!is.null(fx_data) && nrow(fx_data) > 0) {
  ves_fx_bcv <- ves_fx_bcv %>%
    filter(database_id != !!database_id) %>%
    bind_rows(fx_data) %>%
    arrange(fecha_valor, currency)
} else {
  message(glue("Refresh skipped for {database_id}: keeping previous data"))
}

# Check no repeats -----------
nrow(distinct(ves_fx_bcv, fecha_valor, currency)) == nrow(ves_fx_bcv)

# Save -------------
write_csv(ves_fx_bcv, "data/processed/external_01_fx_smc_bcv.csv")

# Clean Up -------
rm(
  fx_data, ves_fx_bcv, database_id, url, bcv_files, manual_fix_files,
  download_bcv_file, extract_fx_from_file, extract_fx_from_sheet,
  process_bcv_file, process_fixed_file, normalize_text, extract_date_from_text
)
