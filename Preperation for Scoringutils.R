# Die Ergebnisse der 4 Vergleichsmodelle sollen so gefiltert werden,
# dass sie dem gleichen Format entsprechen wie die Inferno und MoA-Ergebnisse
# Datum 2024-10-17 bis 2025-05-08
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
    filter(forecast_date >= as.Date("2024-10-17") &
             forecast_date <= as.Date("2025-03-27")) %>%
    filter(!(forecast_date >= as.Date("2024-12-26") & 
               forecast_date <= as.Date("2025-01-02"))) %>%
    filter(type == "quantile") %>%
    dplyr::select(location, age_group, forecast_date, target_end_date,
                  horizon, type, quantile, value)
}

hhh4_pre <- read.csv("hhh4_all.csv")
hhh4_christmas_pre <- read.csv("hhh4_christmas_all.csv")
TSMixer_pre <- read.csv("TSMixer_all.csv")
LightGBM_pre <- read.csv("LightGBM_all.csv")
MeanEnsemble_pre <- read.csv("MeanEnsemble_all.csv")
inferno_pre <- read.csv("Inferno_long_final.csv")
inferno_nb_pre <- read.csv("Inferno_nb_final.csv")
moa_pre <- read.csv("MoA_long_final.csv")
moa_lognormal_pre <- read.csv("MoA_long_final_log_normal.csv")
ensembleComplete_pre <- read.csv("EnsembleComplete_all.csv")
moa2_pre <- read.csv("MoA_long_final2.csv")
moa_lognormal2_pre <- read.csv("MoA_long_final_log_normal2.csv")

inferno_check_pre <- read.csv("Inferno_Check.csv")

# Bei hhh4 gehen die Werte nur bis 2025-03-27, es gibt aber eigentlich 
# Forecasts, die auf Weihnachten basieren. Diese werde ich aber für die 
# Vergleichbarkeit rausfiltern
hhh4_score_help <- filter_function(hhh4_pre)
hhh4_christmas_help <- filter_function(hhh4_christmas_pre)
# Auch bei TSMixer ist das der Fall
# Außerdem gibt es bei TSMixer keine Vorhersagen, die auf der Zeit
# zwischen den Jahren basieren
TSMixer_score_help <- filter_function(TSMixer_pre)
# Bei LightGBM ist es dasselbe wie bei TSMixer
LightGBM_score_help <- filter_function(LightGBM_pre)
# Da MeanEnsemble auch auf den vorherigen basiert, gibt es hier dasselbe Problem
MeanEnsemble_score_help <- filter_function(MeanEnsemble_pre)
# Für MoA und Inferno gibt es nun dieselbe Behandlung
MoA_score_help <- filter_function(moa_pre)
inferno_score_help <- filter_function(inferno_pre)
inferno_nb_score_help <- filter_function(inferno_nb_pre)
MoA_lognormal_score_help <- filter_function(moa_lognormal_pre)
EnsembleComplete_score_help <- filter_function(ensembleComplete_pre)
MoA2_score_help <- filter_function(moa2_pre)
MoA_lognormal2_score_help <- filter_function(moa_lognormal2_pre)

Inferno_check_score_help <- filter_function(inferno_check_pre)

write.csv(Inferno_check_score_help, "Inferno_check_score_help.csv")

write.csv(MoA_lognormal2_score_help, "MoA_lognormal2_score_help.csv")
write.csv(MoA2_score_help, "MoA2_score_help.csv")

write.csv(MoA_lognormal_score_help, "MoA_lognormal_score_help.csv")
write.csv(hhh4_score_help, "hhh4_score_help.csv")
write.csv(hhh4_christmas_help, "hhh4_christmas_score_help.csv")
write.csv(TSMixer_score_help, "TSMixer_score_help.csv")
write.csv(LightGBM_score_help, "LightGBM_score_help.csv")
write.csv(MeanEnsemble_score_help, "MeanEnsemble_score_help.csv")
write.csv(MoA_score_help, "MoA_score_help.csv")
write.csv(inferno_score_help, "Inferno_score_help.csv")
write.csv(inferno_nb_score_help, "Inferno_nb_score_help.csv")
write.csv(EnsembleComplete_score_help, "EnsembleComplete_score_help.csv")
