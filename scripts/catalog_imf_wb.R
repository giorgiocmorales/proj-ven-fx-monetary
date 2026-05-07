# Build IMF and World Bank metadata catalogs for later indicator selection

library(tidyverse)
library(rsdmx)
library(jsonlite)
library(xml2)
library(httr2)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

extract_lang_value <- function(x, preferred_lang = "en") {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  if (is.atomic(x) && length(x) == 1) {
    return(as.character(x))
  }

  x_names <- names(x)

  if (!is.null(x_names) && preferred_lang %in% x_names) {
    return(as.character(x[[preferred_lang]]))
  }

  as.character(x[[1]])
}

fetch_wb_indicator_catalog <- function(per_page = 20000) {
  wb_url <- sprintf(
    "https://api.worldbank.org/v2/indicator?format=json&per_page=%s",
    per_page
  )

  wb_raw <- jsonlite::fromJSON(wb_url)

  if (length(wb_raw) < 2 || is.null(wb_raw[[2]])) {
    stop("World Bank indicator endpoint returned no indicator payload.")
  }

  wb_tbl <- as_tibble(wb_raw[[2]])

  tibble(
    source = "wb",
    indicator_id = wb_tbl$id,
    indicator_name = wb_tbl$name,
    source_note = wb_tbl$sourceNote,
    source_organization = wb_tbl$sourceOrganization,
    source_id = wb_tbl$source$id,
    source_name = wb_tbl$source$value,
    topics = purrr::map_chr(wb_tbl$topics, ~ paste(.x$value, collapse = " | "))
  )
}

fetch_imf_dataflow_catalog <- function() {
  imf_dataflows <- readSDMX(providerId = "IMF_DATA", resource = "dataflow")
  flow_list <- slot(imf_dataflows, "dataflows")

  purrr::map_dfr(flow_list, function(flow) {
    tibble(
      flow_id = slot(flow, "id"),
      agency_id = slot(flow, "agencyID"),
      version = slot(flow, "version"),
      flow_name = extract_lang_value(slot(flow, "Name")),
      flow_description = extract_lang_value(slot(flow, "Description")),
      dsd_ref = slot(flow, "dsdRef")
    )
  }) %>%
    arrange(flow_id)
}

build_codelist_catalog <- function(codelists) {
  purrr::map_dfr(codelists, function(codelist) {
    tibble(
      codelist_id = slot(codelist, "id"),
      codelist_agency = slot(codelist, "agencyID"),
      codelist_version = slot(codelist, "version"),
      codelist_name = extract_lang_value(slot(codelist, "Name")),
      code_count = length(slot(codelist, "Code")),
      codelist_object = list(codelist)
    )
  })
}

match_codelist_to_dimension <- function(concept_ref, flow_id, codelist_catalog) {
  concept_token <- stringr::str_to_upper(concept_ref)
  flow_token <- stringr::str_to_upper(flow_id)

  scored <- codelist_catalog %>%
    mutate(
      codelist_token = stringr::str_to_upper(codelist_id),
      codelist_name_token = stringr::str_to_upper(coalesce(codelist_name, "")),
      score =
        if_else(stringr::str_detect(codelist_token, paste0("(^|_)", flow_token, "(_|$)")), 4L, 0L) +
        if_else(stringr::str_detect(codelist_token, paste0("(^|_)", concept_token, "(_|$)")), 3L, 0L) +
        if_else(stringr::str_detect(codelist_name_token, concept_token), 2L, 0L) +
        if_else(stringr::str_detect(codelist_token, concept_token), 1L, 0L)
    ) %>%
    arrange(desc(score), desc(code_count), codelist_id)

  if (nrow(scored) == 0 || scored$score[[1]] <= 0) {
    return(NULL)
  }

  scored[1, ]
}

is_indicator_candidate <- function(concept_ref) {
  !concept_ref %in% c(
    "COUNTRY", "REF_AREA", "COUNTERPART_AREA", "FREQ", "TIME_PERIOD",
    "TIME", "OBS_STATUS", "UNIT_MEASURE", "UNIT_MULT", "CURRENCY",
    "SECTOR", "ACCOUNTING_ENTRY", "COUNTERPART_SECTOR"
  )
}

parse_env_flag <- function(var_name, default = FALSE) {
  value <- Sys.getenv(var_name, unset = if (default) "TRUE" else "FALSE")
  toupper(value) %in% c("TRUE", "T", "1", "YES", "Y")
}

get_requested_imf_flows <- function(imf_dataflow_catalog) {
  core_flows <- c("WEO", "CPI", "ER", "MFS_IR", "IMTS", "IRFCL")
  custom_flows <- strsplit(Sys.getenv("IMF_FLOW_IDS", ""), ",", fixed = TRUE)[[1]] %>%
    trimws() %>%
    discard(~ .x == "")

  if (length(custom_flows) > 0) {
    return(imf_dataflow_catalog %>% filter(flow_id %in% custom_flows))
  }

  scope <- tolower(Sys.getenv("IMF_SCOPE", "core"))

  if (scope == "all") {
    return(imf_dataflow_catalog)
  }

  imf_dataflow_catalog %>%
    filter(flow_id %in% core_flows) %>%
    mutate(flow_id = factor(flow_id, levels = core_flows)) %>%
    arrange(flow_id) %>%
    mutate(flow_id = as.character(flow_id))
}

read_existing_catalog <- function(path) {
  if (!file.exists(path)) {
    return(tibble())
  }

  readr::read_csv(path, show_col_types = FALSE)
}

checkpoint_imf_catalogs <- function(dimension_catalog, code_catalog, error_catalog) {
  readr::write_csv(dimension_catalog, "registry/imf_dimension_catalog.csv")
  readr::write_csv(code_catalog, "registry/imf_code_catalog.csv")
  readr::write_csv(error_catalog, "registry/imf_catalog_errors.csv")
  readr::write_csv(
    dplyr::filter(dimension_catalog, indicator_candidate),
    "registry/imf_indicator_dimensions.csv"
  )
  readr::write_csv(
    dplyr::filter(code_catalog, indicator_candidate),
    "registry/imf_indicator_catalog.csv"
  )
}

extract_code_field <- function(code, field_name) {
  if (!isS4(code) || !field_name %in% slotNames(code)) {
    return(NA_character_)
  }

  value <- slot(code, field_name)

  if (length(value) == 0) {
    return(NA_character_)
  }

  if (is.list(value)) {
    return(extract_lang_value(value))
  }

  as.character(value[[1]])
}

extract_code_label <- function(code) {
  if (!isS4(code)) {
    return(NA_character_)
  }

  for (field_name in c("name", "label", "description")) {
    if (!field_name %in% slotNames(code)) {
      next
    }

    value <- slot(code, field_name)
    label <- extract_lang_value(value)

    if (!is.na(label) && nzchar(label)) {
      return(label)
    }
  }

  NA_character_
}

resolve_imf_structure_ref <- function(flow_row) {
  dataflow_url <- sprintf(
    "https://api.imf.org/external/sdmx/2.1/dataflow/%s/%s/%s/?references=all",
    flow_row$agency_id,
    flow_row$flow_id,
    flow_row$version
  )

  dataflow_xml <- request(dataflow_url) %>%
    req_perform() %>%
    resp_body_string() %>%
    read_xml()

  ref_node <- xml_find_first(
    dataflow_xml,
    ".//*[local-name()='Dataflow']/*[local-name()='Structure']//*[local-name()='Ref'][1]"
  )

  if (inherits(ref_node, "xml_missing")) {
    return(list(
      agency_id = flow_row$agency_id,
      resource_id = flow_row$dsd_ref,
      version = flow_row$version
    ))
  }

  list(
    agency_id = xml_attr(ref_node, "agencyID"),
    resource_id = xml_attr(ref_node, "id"),
    version = xml_attr(ref_node, "version")
  )
}

extract_imf_flow_catalogs <- function(flow_row) {
  structure_ref <- resolve_imf_structure_ref(flow_row)

  dsd <- readSDMX(
    providerId = "IMF_DATA",
    agencyId = structure_ref$agency_id,
    resource = "datastructure",
    resourceId = structure_ref$resource_id,
    version = structure_ref$version,
    references = "descendants"
  )

  dsd_object <- slot(slot(dsd, "datastructures"), "datastructures")[[1]]
  dimensions <- slot(slot(dsd_object, "Components"), "Dimensions")
  codelist_catalog <- build_codelist_catalog(slot(slot(dsd, "codelists"), "codelists"))

  dimension_catalog <- purrr::map_dfr(seq_along(dimensions), function(idx) {
    dimension <- dimensions[[idx]]
    concept_ref <- slot(dimension, "conceptRef")
    matched <- match_codelist_to_dimension(concept_ref, flow_row$flow_id, codelist_catalog)

    tibble(
      flow_id = flow_row$flow_id,
      flow_name = flow_row$flow_name,
      dsd_ref = flow_row$dsd_ref,
      dimension_order = idx,
      concept_ref = concept_ref,
      matched_codelist_id = if (is.null(matched)) NA_character_ else matched$codelist_id[[1]],
      matched_codelist_name = if (is.null(matched)) NA_character_ else matched$codelist_name[[1]],
      matched_code_count = if (is.null(matched)) NA_integer_ else matched$code_count[[1]],
      match_score = if (is.null(matched)) NA_integer_ else matched$score[[1]],
      indicator_candidate = is_indicator_candidate(concept_ref)
    )
  })

  code_catalog <- purrr::map_dfr(seq_len(nrow(dimension_catalog)), function(i) {
    dimension_row <- dimension_catalog[i, ]

    if (is.na(dimension_row$matched_codelist_id)) {
      return(tibble())
    }

    matched_object <- codelist_catalog %>%
      filter(codelist_id == dimension_row$matched_codelist_id) %>%
      pull(codelist_object) %>%
      .[[1]]

    purrr::map_dfr(slot(matched_object, "Code"), function(code_obj) {
      tibble(
        flow_id = dimension_row$flow_id,
        flow_name = dimension_row$flow_name,
        dsd_ref = dimension_row$dsd_ref,
        concept_ref = dimension_row$concept_ref,
        codelist_id = dimension_row$matched_codelist_id,
        indicator_candidate = dimension_row$indicator_candidate,
        code = if (isS4(code_obj)) extract_code_field(code_obj, "id") else as.character(code_obj[[1]]),
        code_name = if (isS4(code_obj)) extract_code_label(code_obj) else NA_character_,
        parent_code = if (isS4(code_obj)) extract_code_field(code_obj, "parentCode") else NA_character_
      )
    })
  })

  list(
    dimensions = dimension_catalog,
    codes = code_catalog
  )
}

run_wb <- parse_env_flag(
  "RUN_WB",
  default = !file.exists("registry/wb_indicator_catalog.csv")
)
run_imf <- parse_env_flag("RUN_IMF", default = TRUE)
resume_imf <- parse_env_flag("IMF_RESUME", default = TRUE)
imf_sleep_seconds <- suppressWarnings(as.numeric(Sys.getenv("IMF_SLEEP_SECONDS", "0.25")))

if (is.na(imf_sleep_seconds) || imf_sleep_seconds < 0) {
  imf_sleep_seconds <- 0.25
}

if (run_wb) {
  message("Fetching World Bank indicator catalog")
  wb_indicator_catalog <- fetch_wb_indicator_catalog()
  readr::write_csv(wb_indicator_catalog, "registry/wb_indicator_catalog.csv")
}

if (run_imf) {
  message("Fetching IMF dataflow catalog")
  imf_dataflow_catalog <- fetch_imf_dataflow_catalog()
  readr::write_csv(imf_dataflow_catalog, "registry/imf_dataflow_catalog.csv")

  target_flows <- get_requested_imf_flows(imf_dataflow_catalog)
  message(sprintf("Target IMF flows: %s", paste(target_flows$flow_id, collapse = ", ")))

  existing_dimensions <- if (resume_imf) read_existing_catalog("registry/imf_dimension_catalog.csv") else tibble()
  existing_codes <- if (resume_imf) read_existing_catalog("registry/imf_code_catalog.csv") else tibble()
  existing_errors <- if (resume_imf) read_existing_catalog("registry/imf_catalog_errors.csv") else tibble()

  completed_flows <- unique(c(
    if ("flow_id" %in% names(existing_dimensions)) existing_dimensions$flow_id else character(),
    if ("flow_id" %in% names(existing_errors)) existing_errors$flow_id else character()
  ))
  pending_flows <- target_flows %>% filter(!flow_id %in% completed_flows)

  message(sprintf(
    "Pending IMF flows: %s of %s",
    nrow(pending_flows),
    nrow(target_flows)
  ))

  if (nrow(pending_flows) == 0) {
    message("No IMF flows left to process.")
  } else {
    imf_dimension_catalog <- existing_dimensions
    imf_code_catalog <- existing_codes
    imf_error_catalog <- existing_errors

    for (i in seq_len(nrow(pending_flows))) {
      flow_row <- pending_flows[i, ]
      message(sprintf(
        "[%s/%s] IMF flow %s",
        i,
        nrow(pending_flows),
        flow_row$flow_id
      ))

      flow_result <- tryCatch(
        {
          result <- extract_imf_flow_catalogs(flow_row)
          list(
            dimensions = result$dimensions,
            codes = result$codes,
            errors = tibble()
          )
        },
        error = function(e) {
          list(
            dimensions = tibble(),
            codes = tibble(),
            errors = tibble(
              flow_id = flow_row$flow_id,
              agency_id = flow_row$agency_id,
              version = flow_row$version,
              dsd_ref = flow_row$dsd_ref,
              error_message = conditionMessage(e)
            )
          )
        }
      )

      imf_dimension_catalog <- bind_rows(imf_dimension_catalog, flow_result$dimensions)
      imf_code_catalog <- bind_rows(imf_code_catalog, flow_result$codes)
      imf_error_catalog <- bind_rows(imf_error_catalog, flow_result$errors)

      checkpoint_imf_catalogs(
        imf_dimension_catalog,
        imf_code_catalog,
        imf_error_catalog
      )

      Sys.sleep(imf_sleep_seconds)
    }
  }
}
