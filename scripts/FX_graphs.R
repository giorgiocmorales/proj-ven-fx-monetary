# Graphs

# Load packages

library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(extrafont)

# Load data

fx_consolidated <- read_csv("data/output/ves_usd_fx_consolidated.csv",
                        col_types = cols(
                          date = col_date(),
                          rate_smc = col_double(),
                          rate_smc_adj = col_double(),
                          rate_indx = col_double(),
                          rate_indx_adj = col_double(),
                          rate_yad = col_double())) %>%
  select(date, rate_smc_adj, rate_indx_adj, rate_yad)

# Theme set

theme_set(theme_minimal(base_size = 12))
loadfonts(device = "win")

# FX time series

fx_1 <-
  fx_consolidated %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = rate_smc_adj, color = "TC oficial"), linewidth = 1) +
  geom_line(aes(y = rate_yad, color = "TC no-oficial"), linewidth = 1) +
  scale_color_manual(
    values = c(
      "TC oficial" = "#003A5D",
      "TC no-oficial" = "#D70036"
      )
    ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = c(NA, 250)
  ) +
  scale_x_date(
    limits = c(ymd("2022-01-01"), NA)
  ) +
  labs(
    title = "Tipo de cambio",
    caption = "Fuente: BCV. Yadio. Cálculos propios.",
    y = "VES/USD"
  ) +
  theme(
    text = element_text(family = "Georgia"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )