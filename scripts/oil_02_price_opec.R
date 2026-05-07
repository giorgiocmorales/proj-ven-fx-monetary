# Extract oil price tables from OPEC MOMR reports

library(tidyverse)
library(pdftools)

rm(list = ls())

source("R/source_helpers.R")
source("R/opec_helpers.R")
ensure_project_dirs()

dataset_id <- "oil_02_price_opec"
source_id <- "opec"
source_url <- "https://www.opec.org/monthly-oil-market-report.html"
downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

reports <- read_active_opec_reports()

price_map <- tribble(
  ~label, ~variable_id,
  "ORB", "orb",
  "Merey", "merey",
  "North Sea Dated", "north_sea_dated",
  "WTI", "wti",
  "Dubai", "dubai"
)

results <- purrr::pmap(reports, function(...) {
  report <- tibble::as_tibble(list(...))

  tryCatch(
    {
      paths <- opec_download_report(report)
      pdf_text <- pdftools::pdf_text(paths$pdf_path)
      price_page_id <- find_opec_pdf_page(
        pdf_text,
        patterns = c("Table 1 - 1: OPEC Reference Basket", "OPEC Reference Basket and selected crudes"),
        fallback_page = report$page_price[[1]]
      )
      price_page <- pdf_text[price_page_id]

      data <- purrr::pmap_dfr(price_map, function(label, variable_id) {
        extract_opec_price_line(price_page, label, variable_id, report$observation_month[[1]])
      }) %>%
        mutate(
          dataset_id = dataset_id,
          source_id = source_id,
          source_url = source_url,
          source_pdf_url = report$pdf_url[[1]],
          source_appendix_url = report$appendix_url[[1]],
          report_month = report$report_month[[1]],
          date = period_to_opec_date(period),
          period_type = opec_period_type(period),
          unit = "USD/b",
          downloaded_at = downloaded_at
        ) %>%
        select(
          dataset_id, source_id, variable_id, label, period, period_type, date,
          value, unit, report_month, source_url, source_pdf_url,
          source_appendix_url, downloaded_at
        )

      list(data = data, error = tibble())
    },
    error = function(e) {
      list(
        data = tibble(),
        error = tibble(
          dataset_id = dataset_id,
          source_id = source_id,
          report_month = report$report_month[[1]],
          observation_month = report$observation_month[[1]],
          source_pdf_url = report$pdf_url[[1]],
          error_message = conditionMessage(e),
          downloaded_at = downloaded_at
        )
      )
    }
  )
})

oil_prices <- map_dfr(results, "data")
oil_price_errors <- map_dfr(results, "error")

write_processed_dataset(oil_prices, dataset_id)

oil_prices_latest <- oil_prices %>%
  filter(!is.na(date), period_type %in% c("monthly", "year_to_date")) %>%
  arrange(variable_id, period_type, date, report_month) %>%
  group_by(variable_id, period_type, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

write_final_dataset(oil_prices_latest, paste0(dataset_id, "_latest"))

error_path <- file.path("data", "raw", paste0(dataset_id, "_errors.csv"))
if (nrow(oil_price_errors) > 0) {
  write_csv(oil_price_errors, error_path)
} else if (file.exists(error_path)) {
  unlink(error_path)
}
