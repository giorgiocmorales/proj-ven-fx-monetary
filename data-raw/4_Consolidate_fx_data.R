# Consolidate FX data

#Load packages ------------
library(tidyverse)
library(lubridate)

# Load data ----------
bcv_fx_smc <- read_csv("data/cleaned/ves_usd_fx_smc.csv",
                       col_types = cols(
                         fecha_valor = col_date(),
                         usd_bid = col_double(),
                         usd_ask = col_double(),
                         database_id = col_character()
                         ))

bcv_fx_indx <- read_csv("data/cleaned/ves_usd_fx_indx.csv",
                        col_types = cols(
                          fecha = col_date(),
                          tasa = col_double()
                        ))

ves_fx_yad <- read_csv("data/cleaned/ves_usd_fx_yad.csv",
                       col_types = cols(
                         date = col_date(),
                         rate = col_double(),
                         implicit_rate = col_double(),
                         avg24h = col_double(),
                         usdbtc = col_double()
                       ))

# Clean data ------------
bcv_fx_smc <- bcv_fx_smc %>%
  rename(date = fecha_valor, rate_smc = usd_ask) %>%
  select(date, rate_smc)

bcv_fx_indx <- bcv_fx_indx %>%
  rename(date = fecha, rate_indx = tasa) %>%
  select(date, rate_indx)

ves_fx_yad <- ves_fx_yad %>%
  rename(date = date, rate_yad = implicit_rate) %>%
  select(date, rate_yad)

# Set combination date range -------------
min_date <- min(bcv_fx_smc$date, bcv_fx_indx$date, ves_fx_yad$date, na.rm = TRUE)
max_date <- max(bcv_fx_smc$date, bcv_fx_indx$date, ves_fx_yad$date, na.rm = TRUE)

full_date_range <- tibble(date = seq.Date(from = min_date, to = max_date, by = "day"))

# Combine data -------------
fx_combined <- full_join(full_date_range, bcv_fx_smc, by = "date") %>%
  full_join(bcv_fx_indx, by = "date") %>%
  full_join(ves_fx_yad, by = "date")

# Fill NAs down -------------
fx_combined <- fx_combined %>%
  arrange(date) %>%
  fill(rate_smc, rate_indx, rate_yad)

# Correct for currency reconversions -------------
fx_combined <- fx_combined %>%
  mutate(rate_smc_adj = rate_smc, 
         rate_smc_adj = if_else(date <= as.Date("2021-10-03"), rate_smc_adj / 1000000, rate_smc_adj)) %>%
  mutate(rate_indx_adj = rate_indx,
         rate_indx_adj = if_else(date <= as.Date("2021-10-03"), rate_indx_adj / 1000000, rate_indx_adj)) %>%
  select(date, rate_smc, rate_smc_adj, rate_indx, rate_indx_adj, rate_yad)

# Save ----------------
write_csv(fx_combined, "data/consolidated/ves_usd_fx_consolidated.csv")

# Clean up
rm(bcv_fx_indx, bcv_fx_smc, ves_fx_yad, full_date_range, fx_combined)

