# Extract VES/EUR data from BCV exchange intervention page

library(rvest)
library(tidyverse)
library(lubridate)
library(glue)

source("R/source_helpers.R")
ensure_project_dirs()

source_url <- "https://www.bcv.org.ve/politica-cambiaria/intervencion-cambiaria"
raw_path <- "data/raw/bcv_fx_intervention.html"
processed_path <- "data/processed/external_03_fx_int_bcv.csv"

parse_bcv_number <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\.", "") %>%
    str_replace(",", ".") %>%
    as.numeric()
}

page <- read_html(source_url)
writeLines(as.character(page), raw_path, useBytes = TRUE)

tables <- html_elements(page, "table") %>%
  html_table(fill = TRUE)

if (length(tables) == 0) {
  stop(glue("No tables found at {source_url}"))
}

fx_intervention <- tables[[1]] %>%
  select(
    date = 1,
    intervention_id = 2,
    rate = 3
  ) %>%
  mutate(
    date = dmy(date),
    currency = "EUR",
    rate = parse_bcv_number(rate),
    source_id = "bcv",
    source_url = source_url,
    downloaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) %>%
  filter(!is.na(date), !is.na(rate)) %>%
  distinct(date, intervention_id, .keep_all = TRUE) %>%
  arrange(date)

write_csv(fx_intervention, processed_path)

rm(source_url, raw_path, processed_path, page, tables, fx_intervention, parse_bcv_number)
