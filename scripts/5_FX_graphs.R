# Graphs

# Load packages

library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(extrafont)
library(slider)

# Load data ------

fx_consolidated <- read_csv("data/output/ves_usd_fx_consolidated.csv",
                        col_types = cols(
                          date = col_date(),
                          rate_smc = col_double(),
                          rate_smc_adj = col_double(),
                          rate_indx = col_double(),
                          rate_indx_adj = col_double(),
                          rate_yad = col_double())) %>%
  select(date, rate_smc_adj, rate_indx_adj, rate_yad) %>%
  filter(date <= Sys.Date())

# Theme set -----

loadfonts(device = "win")

theme_set(
  theme_minimal(base_size = 12, base_family = "Georgia") +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text = element_text(color = "grey20"),
      plot.title = element_text(size = 14, face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linetype = "dashed", linewidth = 0.15),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 2)),
      plot.margin = margin(10, 10, 10, 10)
    )
)


# Last date ----------

last_date <- max(fx_consolidated$date, na.rm = TRUE)
last_date_label <- format(last_date, "%d-%m-%Y")


# 1 FX time series ---------

fx_1 <-
  fx_consolidated %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = rate_smc_adj, color = "TC oficial"), linewidth = 1) +
  geom_line(aes(y = rate_yad, color = "TC no-oficial"), linewidth = 1) +
  scale_color_manual(
    values = c("TC oficial" = "#003A5D","TC no-oficial" = "#D70036")) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = c(NA, 250),
    expand = c(0,0)) +
  scale_x_date(
    limits = c(ymd("2022-01-01"), NA)) +
  labs(
    title = "Tipos de cambio",
    caption = paste0("Fuente: BCV; Yadio; Cálculos propios.| Actualizado: ", last_date_label),
    y = "VES/USD") +
  theme(
    axis.title.x = element_blank())

ggsave(filename = "output/fx_1.jpeg",
       plot = fx_1,
       width = 8*300,
       height = 6*300,
       units = "px",
       dpi = 300)

# 2 FX time series with gap -------

## Last vals, gap and labels

last_vals <- fx_consolidated %>%
  filter(!is.na(rate_smc_adj), !is.na(rate_yad)) %>%
  filter(date == last_date) %>%
  mutate(gap_pct = rate_yad / rate_smc_adj - 1)

## Graph

fx_2 <-
  fx_consolidated %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = rate_smc_adj, color = "TC oficial"), linewidth = 0.7) +
  geom_line(aes(y = rate_yad, color = "TC no-oficial"), linewidth = 0.7) +
  scale_color_manual(
    values = c("TC oficial" = "#003A5D","TC no-oficial" = "#D70036")) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = c(25, 250),
    breaks = scales::breaks_width(25),
    expand = c(0,0)) +
  scale_x_date(
    limits = c(ymd("2024-01-01"), last_date + 15),
    breaks = seq(ymd("2024-01-01"), last_date + 15, by = "2 months"),
    date_labels = "%b-%y") +
  # Last values and gap
  geom_text(
    data = last_vals,
    aes(x = date + 5, y = (rate_smc_adj + rate_yad)/2, 
        label = percent(gap_pct, accuracy = 0.1, decimal.mark = ",")),
    inherit.aes = FALSE, hjust = 0, size = 3.5, family = "Georgia") +
  geom_text(
    data = last_vals,
    aes(x = date + 5, y = rate_smc_adj,
        label = number(rate_smc_adj, big.mark=".", decimal.mark=",", accuracy=0.1)),
    color = "#003A5D", inherit.aes = FALSE, hjust = 0, size = 3.5, family = "Georgia") +
  geom_text(
    data = last_vals,
    aes(x = date + 5, y = rate_yad,
        label = number(rate_yad, big.mark=".", decimal.mark=",", accuracy=0.1)),
    color = "#D70036", inherit.aes = FALSE, hjust = 0, size = 3.5, family = "Georgia") +
  #Segment
  geom_segment(
    data = last_vals,
    aes(x = date, xend = date,
        y = rate_smc_adj + 5, yend = rate_yad - 5),
    color = "black", linewidth = 0.5,
    arrow = arrow(ends = "both", type = "closed", length = unit(0.025, "inches"))) +
  labs(
    title = "Tipos de cambio",
    caption = paste0("Fuente: BCV; Yadio; Cálculos propios.| Actualizado: ", last_date_label),
    y = "VES/USD") +
  theme(
    axis.title.x = element_blank())

ggsave(filename = "output/fx_2.jpeg",
       plot = fx_2,
       width = 8*300,
       height = 6*300,
       units = "px",
       dpi = 300)

# 3 FX gap -----

fx_3 <-
  fx_consolidated %>%
  mutate(gap = rate_yad/rate_smc_adj-1) %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = gap, color = "Brecha"), linewidth = 0.75, na.rm = TRUE) +
  scale_color_manual(
    values = c(
      "Brecha" = "#003A5D")) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1, decimal.mark = ","),
    limits = c(0, 0.7),
    breaks = seq(0, 0.7, by = 0.1),
    expand = c(0,0)) +
  scale_x_date(
      limits = c(ymd("2024-01-01"), last_date + 15),
      breaks = seq(ymd("2024-01-01"), last_date + 15, by = "2 months"),
      date_labels = "%b-%y") +
  labs(
    title = "Brecha cambiaria",
    subtitle = "Tipo de cambio no-oficial/oficial",
    caption = paste0("Fuente: BCV; Yadio; Cálculos propios.| Actualizado: ", last_date_label),
    y = "Brecha") +
  theme(
    axis.title.x = element_blank())

ggsave(filename = "output/fx_3.jpeg",
       plot = fx_3,
       width = 8*300,
       height = 6*300,
       units = "px",
       dpi = 300)

# 4 Distance to rolling average -----