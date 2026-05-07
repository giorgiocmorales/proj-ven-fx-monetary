# Extract Venezuelan oil production from OPEC MOMR reports

library(tidyverse)
library(pdftools)

rm(list = ls())

source("R/source_helpers.R")
source("R/opec_helpers.R")
ensure_project_dirs()

dataset_id <- "oil_01_prod_opec"
source_id <- "opec"
source_url <- "https://www.opec.org/monthly-oil-market-report.html"
downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

reports <- read_active_opec_reports()

results <- purrr::pmap(reports, function(...) {
  report <- tibble::as_tibble(list(...))

  tryCatch(
    {
      paths <- opec_download_report(report)
      pdf_text <- pdftools::pdf_text(paths$pdf_path)
      production_page <- find_opec_pdf_page(
        pdf_text,
        patterns = c("DoC crude oil production.*Venezuela", "Table 5 - 7: DoC crude oil production.*Venezuela"),
        fallback_page = report$page_production[[1]]
      )
      report_page <- pdf_text[production_page]

      data <- bind_rows(
        extract_opec_venezuela_line(report_page, "secondary_sources"),
        extract_opec_venezuela_line(report_page, "direct_communication")
      ) %>%
        group_by(source_type) %>%
        mutate(
          date = case_when(
            period == "m_current" ~ report$observation_month[[1]],
            period == "m_minus_1" ~ seq(report$observation_month[[1]], length = 2, by = "-1 month")[[2]],
            period == "m_minus_2" ~ seq(report$observation_month[[1]], length = 3, by = "-1 month")[[3]],
            TRUE ~ date
          ),
          period = case_when(
            period == "m_current" ~ opec_period_label(report$observation_month[[1]]),
            period == "m_minus_1" ~ opec_period_label(seq(report$observation_month[[1]], length = 2, by = "-1 month")[[2]]),
            period == "m_minus_2" ~ opec_period_label(seq(report$observation_month[[1]], length = 3, by = "-1 month")[[3]]),
            period == "q_current" ~ paste0(lubridate::quarter(report$observation_month[[1]]), "Q", format(report$observation_month[[1]], "%y")),
            period == "q_minus_1" ~ paste0(lubridate::quarter(seq(report$observation_month[[1]], length = 4, by = "-1 month")[[4]]), "Q", format(seq(report$observation_month[[1]], length = 4, by = "-1 month")[[4]], "%y")),
            period == "q_minus_2" ~ paste0(lubridate::quarter(seq(report$observation_month[[1]], length = 7, by = "-1 month")[[7]]), "Q", format(seq(report$observation_month[[1]], length = 7, by = "-1 month")[[7]], "%y")),
            period == "mom_change" ~ paste0(opec_period_label(report$observation_month[[1]]), "/", opec_previous_month_label(report$observation_month[[1]])),
            TRUE ~ period
          ),
          period_type = case_when(
            str_detect(period, "^\\d{4}$") ~ "annual",
            str_detect(period, "^\\dQ") ~ "quarterly",
            str_detect(period, "^[A-Za-z]{3}") ~ "monthly",
            TRUE ~ "change"
          )
        ) %>%
        ungroup() %>%
        mutate(
          dataset_id = dataset_id,
          source_id = source_id,
          source_url = source_url,
          source_pdf_url = report$pdf_url[[1]],
          source_appendix_url = report$appendix_url[[1]],
          report_month = report$report_month[[1]],
          observation_month = report$observation_month[[1]],
          country = "Venezuela",
          unit = "tb/d",
          downloaded_at = downloaded_at
        ) %>%
        select(
          dataset_id, source_id, source_type, country, period, period_type, date,
          value, unit, report_month, observation_month, source_url, source_pdf_url,
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

oil_prod <- map_dfr(results, "data")
oil_prod_errors <- map_dfr(results, "error")

write_processed_dataset(oil_prod, dataset_id)

oil_prod_latest <- oil_prod %>%
  filter(!is.na(date), period_type %in% c("annual", "quarterly", "monthly")) %>%
  arrange(source_type, country, period_type, date, report_month) %>%
  group_by(source_type, country, period_type, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

write_final_dataset(oil_prod_latest, paste0(dataset_id, "_latest"))

error_path <- file.path("data", "raw", paste0(dataset_id, "_errors.csv"))
if (nrow(oil_prod_errors) > 0) {
  write_csv(oil_prod_errors, error_path)
} else if (file.exists(error_path)) {
  unlink(error_path)
}
