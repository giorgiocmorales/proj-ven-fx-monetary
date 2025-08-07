# Load required packages
library(tidyverse)   # Covers: dplyr, purrr, stringr, tibble, readr (optional)
library(readxl)      # Technically part of tidyverse but load explicitly for clarity
library(lubridate)
library(openxlsx)
library(glue)


# Download BCV Excel file to a temporary location ------
download_bcv_file <- function(url) {
  temp_path <- tempfile(fileext = ".xls")
  download.file(url, destfile = temp_path, mode = "wb", quiet = TRUE)
  return(temp_path)
}


# Extract USD row from one sheet ------
extract_usd_from_sheet <- function(sheet, file_path) {
  df <- read_excel(file_path, sheet = sheet, range = "B1:G100", col_names = FALSE)
  
  col_1 <- df$`...1`
  col_3 <- df$`...3`
  
  fecha_operacion_raw <- col_1[grepl("fecha operaci[oó]n", col_1, ignore.case = TRUE)][1]
  fecha_valor_raw     <- col_3[grepl("fecha valor",     col_3, ignore.case = TRUE)][1]
  
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

# Extract all USD rates from a full Excel file ------
extract_usd_from_file <- function(file_path) {
  sheets <- excel_sheets(file_path)
  map_dfr(sheets, extract_usd_from_sheet, file_path = file_path)
}

# Main wrapper: download, extract, delete --------
process_bcv_file <- function(url, database_id) {
  xls_file <- download_bcv_file(url)
  
  safe_extract_all <- function(file_path) {
    tryCatch(
      extract_usd_from_file(file_path),
      error = function(e) {
        message("⚠️ XLS parse failed. Attempting XLSX fallback...")
        
        xlsx_path <- tempfile(fileext = ".xlsx")
        new_wb <- createWorkbook()
        sheet_names <- excel_sheets(file_path)
        
        for (sheet in sheet_names) {
          message(glue("→ Copying sheet: {sheet}"))
          df <- tryCatch(
            read_excel(file_path, sheet = sheet, col_names = FALSE),
            error = function(e) {
              stop(glue("❌ Error copying sheet '{sheet}': {e$message}"))
            }
          )
          addWorksheet(new_wb, sheetName = sheet)
          writeData(new_wb, sheet = sheet, x = df, colNames = FALSE)
        }
        
        saveWorkbook(new_wb, xlsx_path, overwrite = TRUE)
        extract_usd_from_file(xlsx_path)
      }
    )
  }
  
  result <- safe_extract_all(xls_file)
  if (!is.null(result)) {
    result$database_id <- database_id
  }
  
  unlink(xls_file)
  return(result)
}


# Create data tible
ves_fx_bcv <- tibble()

# Q1.2020

# URL set up
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2a20_smc.xls"

# Process the file
usd_data <- process_bcv_file(url, "2020Q1")

# Optionally preview
print(usd_data)

# Append
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

# Q2.2020
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b20_smc.xls"
usd_data <- process_bcv_file(url, "2020Q2")
print(usd_data)
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

# Q3.2020
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c20_smc.xls"
usd_data <- process_bcv_file(url, "2020Q3")
print(usd_data)
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

# Q4.2020
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2d20_smc.xls"
usd_data <- process_bcv_file(url, "2020Q4")
print(usd_data)
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

# Q1.2021
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2a21_smc_58.xls"
usd_data <- process_bcv_file(url, "2021Q1")
print(usd_data)
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

# Q2.2021
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b21_smc.xls"
usd_data <- process_bcv_file(url, "2021Q2")
print(usd_data)
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

# Q3.2021
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c21_smc.xls"
usd_data <- process_bcv_file(url, "2021Q3")
print(usd_data)
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

# Q4.2021
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2d21_smc.xls"
usd_data <- process_bcv_file(url, "2021Q4")
print(usd_data)
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

# Q1.2022
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2a22_smc.xls"
usd_data <- process_bcv_file(url, "2022Q1")
print(usd_data)
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

# Q2.2022
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2b22_smc.xls"
usd_data <- process_bcv_file(url, "2022Q2")
print(usd_data)
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

# Q3.2022
url <- "https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c22_smc.xls"
usd_data <- process_bcv_file(url, "2022Q3")
print(usd_data)
ves_fx_bcv <- bind_rows(ves_fx_bcv, usd_data)
rm(usd_data)

file_path <- download_bcv_file("https://www.bcv.org.ve/sites/default/files/EstadisticasGeneral/2_1_2c22_smc.xls")
sheet <- "30092022"
read_excel(file_path, sheet = sheet)


# Q4.2022
# Q1.2023
# Q2.2023
# Q3.2023
# Q4.2023
# Q1.2024
# Q2.2024
# Q3.2024
# Q4.2024
# Q1.2025
# Q2.2025
# Q3.2025
# Q4.2025