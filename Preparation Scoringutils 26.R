# Die Ergebnisse der 4 Vergleichsmodelle sollen so gefiltert werden,
# dass sie dem gleichen Format entsprechen wie die Inferno und MoA-Ergebnisse
# 25/26 Saison
rm(list = ls())
library(dplyr)
library(readr)
# Daten einlesen
setwd("C:/Users/runeg/OneDrive/Dokumente/Karlsruher Institut für Technologie - Studium/Bachelorarbeit/Code")

filter_function <- function(df){
  df %>%
    filter(location == "DE") %>%
    mutate(forecast_date   = as.Date(forecast_date),
           target_end_date = as.Date(target_end_date)) %>%
    filter(forecast_date >= as.Date("2025-10-16") &
             forecast_date <= as.Date("2026-03-26")) %>%
    filter(!(forecast_date >= as.Date("2025-12-25") & 
               forecast_date <= as.Date("2026-01-01"))) %>%
    filter(type == "quantile") %>%
    dplyr::select(location, age_group, forecast_date, target_end_date,
                  horizon, type, quantile, value)
}

inferno_ln_pre <- read.csv("Inferno_long_final_26_1.csv")
inferno_nb_pre <- read.csv("Inferno_nb_final_26.csv")



inferno_ln_score_help <- filter_function(inferno_ln_pre)
inferno_nb_score_help <- filter_function(inferno_nb_pre)


write.csv(inferno_ln_score_help, "Inferno_ln_score_help_26_1.csv")
write.csv(inferno_nb_score_help, "Inferno_nb_score_help_26.csv")

