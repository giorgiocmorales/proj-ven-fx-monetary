# Extract VES/USD data from BCV (TC Referencia del Sistema Mercado Cambiario)

# Load packages
library(tidyverse)   
library(readxl)      
library(lubridate)
library(openxlsx)
library(glue)

# Helper functions ------

# Download to data/raw/
download_bcv_file <- function(url) {
  file_name <- basename(url)
  dest_path <- file.path("data/raw", file_name)
  download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
  return(dest_path)
}

# Extract USD row from a single sheet
extract_usd_from_sheet <- function(sheet, file_path) {
  df <- read_excel(file_path, sheet = sheet, range = "B1:G100", col_names = FALSE)
  
  col_1 <- df$`...1`
  col_3 <- df$`...3`
  
  fecha_operacion_raw <- col_1[grepl("fecha operaci[oó]n", col_1, ignore.case = TRUE)][1]
  fecha_valor_raw     <- col_3[grepl("fecha valor", col_3, ignore.case = TRUE)][1]
  
  fecha_operacion <- str_extract(fecha_operacion_raw, "\\d{2}/\\d{2}/\\d{4}") %>% dmy()
  fecha_valor     <- str_extract(fecha_valor_raw,     "\\d{2}/\\d{2}/\\d{4}") %>% dmy()
  
  usd_row <- which(trimws(df$`...1`) == "USD")
  if (length(usd_row) == 0) return(NULL)
  
  bid <- as.numeric(df$`...5`[usd_row[1]])
  ask <- as.numeric(df$`...6`[usd_row[1]])
  
  tibble(
    sheet_id = sheet,
    currency = "USD",
    fecha_operacion = fecha_operacion,
    fecha_valor = fecha_valor,
    usd_bid = bid,
    usd_ask = ask
  )
}

# Extract all sheets
extract_usd_from_file <- function(file_path) {
  sheets <- excel_sheets(file_path)
  map_dfr(sheets, extract_usd_from_sheet, file_path = file_path)
}

# Process one BCV file: download → extract → delete
process_bcv_file <- function(url, database_id) {
  file_path <- tryCatch(
    {
      download_bcv_file(url)
    },
    error = function(e) {
      message(glue("Download failed: {basename(url)} — skipping"))
      return(NULL)
    }
  )
  
  if (is.null(file_path)) return(NULL)
  
  result <- tryCatch(
    extract_usd_from_file(file_path),
    error = function(e) {
      message(glue("XLS parse failed: {basename(file_path)} — check manually"))
      file.copy(file_path, file.path("data/manual_fix", basename(file_path)), overwrite = TRUE)
      return(NULL)
    }
  )
  
  if (!is.null(result)) {
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
  "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c25_smc.xls", "2025Q3"
)

# Initialize result
ves_fx_bcv <- tibble()

# Loop over all files
for (i in seq_len(nrow(bcv_files))) {
  message(glue("▶ Processing {bcv_files$database_id[i]}"))
  usd_data <- process_bcv_file(bcv_files$url[i], bcv_files$database_id[i])
  if (!is.null(usd_data)) {
    ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
  }
}

# Manual fix files ------------

# Helper function
process_fixed_file <- function(filepath, database_id) {
  sheets <- excel_sheets(filepath)
  df <- map_dfr(sheets, extract_usd_from_sheet, file_path = filepath)
  df$database_id <- database_id
  return(df)
}

# 2021Q4
usd_data <- process_fixed_file("data/manual_fix/2_1_2d21_smc.xlsx", "2021Q4")
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)

# 2022Q3
usd_data <- process_fixed_file("data/manual_fix/2_1_2c22_smc.xlsx", "2022Q3")
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)

# 2022Q4
usd_data <- process_fixed_file("data/manual_fix/2_1_2d22_smc.xlsx", "2022Q4")
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)

# 2023Q1
usd_data <- process_fixed_file("data/manual_fix/2_1_2a23_smc.xlsx", "2023Q1")
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)

# 2023Q2
usd_data <- process_fixed_file("data/manual_fix/2_1_2b23_smc.xlsx", "2023Q2")
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)

# 2023Q3
usd_data <- process_fixed_file("data/manual_fix/2_1_2c23_smc_60.xlsx", "2023Q3")
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)

# Arrange
ves_fx_bcv <- ves_fx_bcv %>% arrange(fecha_valor)

# Check databases order
unique(ves_fx_bcv$database_id) %>% sort()

# Update current (incomplete) quarter -------------

# Load csv
ves_fx_bcv <- read_csv("data/output/ves_usd_fx_smc.csv")

url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c25_smc.xls"
database_id <- "2025Q3"

# Process fresh data
usd_data <- process_bcv_file(url, database_id)

# Overwrite: drop old 2025Q3 rows and append new ones
ves_fx_bcv <- ves_fx_bcv %>%
  filter(database_id != !!database_id) %>%
  bind_rows(usd_data) %>% 
  arrange(fecha_valor)

# Save -------------
write_csv(ves_fx_bcv, "data/output/ves_usd_fx_smc.csv")

# Clean Up -------
rm(usd_data, ves_fx_bcv, database_id, url, download_bcv_file, extract_usd_from_file, extract_usd_from_sheet, process_bcv_file)
