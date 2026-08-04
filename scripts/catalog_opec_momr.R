# Build a candidate catalog of OPEC Monthly Oil Market Report archives.
#
# OPEC's year archive pages are available at
# https://www.opec.org/monthly-oil-market-report-YYYY.html. Local automated
# requests to those pages can be blocked, but their official PDF links use a
# stable month-year convention. This script adds unverified historical PDF
# candidates without activating them for the current extractors.

library(tidyverse)
library(lubridate)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

registry_path <- file.path("registry", "opec_momr_reports.csv")
start_month <- as.Date("2001-01-01")
end_month <- floor_date(Sys.Date(), "month")

existing <- read_csv(
  registry_path,
  col_types = cols(
    report_month = col_date(),
    observation_month = col_date(),
    appendix_url = col_character(),
    pdf_url = col_character(),
    pdf_source = col_character(),
    page_price = col_integer(),
    page_production = col_integer(),
    status = col_character(),
    notes = col_character()
  )
)

report_months <- seq.Date(start_month, end_month, by = "month")

candidates <- tibble(report_month = report_months) %>%
  mutate(
    observation_month = report_month %m-% months(1),
    month_slug = str_to_lower(format(report_month, "%B")),
    year = format(report_month, "%Y"),
    pdf_url = glue::glue("https://www.opec.org/assets/assetdb/momr-{month_slug}-{year}.pdf"),
    appendix_url = if_else(
      report_month >= as.Date("2019-02-01"),
      glue::glue("https://www.opec.org/assets/assetdb/momr-appendix-{month_slug}-{year}.xlsx"),
      NA_character_
    ),
    pdf_source = "official_candidate",
    page_price = NA_integer_,
    page_production = NA_integer_,
    status = "candidate",
    notes = "Generated from OPEC MOMR archive URL convention; verify parser compatibility before activation."
  ) %>%
  select(-month_slug, -year) %>%
  anti_join(existing, by = "report_month")

updated_registry <- bind_rows(existing, candidates) %>%
  arrange(desc(report_month))

write_csv(updated_registry, registry_path)

message(glue::glue(
  "Added {nrow(candidates)} OPEC MOMR archive candidates; ",
  "{sum(updated_registry$status == 'active')} reports remain active."
))
