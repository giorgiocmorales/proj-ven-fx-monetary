# Extract VES/USD data from Yadio API

# Load packages
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

# Fetch url ------
url_hist <- "https://web2.xekura.io/json/VES/hist.json"
resp <- GET(url_hist)

# Get raw text ------
raw_text <- content(resp, "text", encoding = "UTF-8")
data_list <- fromJSON(raw_text)

#Check structure -------
str(data_list)

#Clean ------
ves_hist_clean <- data_list %>%
  mutate(date = mdy(date), implicit_rate = avg24h/usdbtc) %>%
  arrange(date) %>%
  select(date, rate, implicit_rate, avg24h, usdbtc) %>%
  arrange(date)

# Save ------
write_csv(ves_hist_clean, "data/cleaned/ves_usd_fx_yad.csv")

#Clean up -----
rm(data_list, resp, ves_hist_clean, raw_text, url_hist)