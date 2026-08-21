rm(list = ls())

# Scoringutils Funktion
library(dplyr)
library(readr)
library(scoringutils)

# Observationen
url <- "https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/refs/heads/main/data/agi/are/latest_data-agi-are.csv"
obs <- read.csv(url) %>%
  filter(location == "DE") %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date, age_group, observed = value)

# Daten einlesen
setwd("C:/Users/runeg/OneDrive/Dokumente/Karlsruher Institut für Technologie - Studium/Bachelorarbeit/Code")

score_help <- function(csv, obs, model_name){
  forecasts <- csv %>% 
    mutate(forecast_date = as.Date(forecast_date),
           target_end_date = as.Date(target_end_date),
           model = model_name)
    
  # Observationen und Forecasts zusammenführen
  scoring_df <- forecasts %>% 
    left_join(obs, by = c("target_end_date" = "date", "age_group")) %>%
    rename(predicted = value, quantile_level = quantile) %>% 
    filter(!is.na(observed)) 
  
  # Formatierung für Scoringutils
  sc_forecast <- as_forecast_quantile(
    scoring_df,
    forecast_unit = c("model","location", "age_group", "forecast_date", 
                      "target_end_date", "horizon")
  )
  
  score(sc_forecast)
}

scores_age <- function(scoring_results){
  summarise_scores(scoring_results, by = c("model", "age_group"))
}



scores_horizon <- function(scoring_results, agegroup){
  scoring_results %>%
   filter(age_group == agegroup) %>%
   summarise_scores(by = c("model", "horizon"))
}

inferno_ln_score_help <- read.csv("Inferno_ln_score_help_26.csv")
hhh4_score_help <- read.csv("KIT-hhh4_26.csv")
LightGBM_score_help <- read.csv("KIT-LightGBM_26.csv")
TSMixer_score_help <- read.csv("KIT-TSMixer_26.csv")
MeanEnsemble_score_help <- read.csv("KIT-MeanEnsemble_26.csv")



inferno_ln_wis <- score_help(inferno_ln_score_help, obs, "Inferno Log-Normal")
summarise_scores(inferno_ln_wis)

LightGBM_wis <- score_help(LightGBM_score_help, obs, "LightGBM")
summarise_scores(LightGBM_wis)

TSMixer_wis <- score_help(TSMixer_score_help, obs, "TSMixer")
summarise_scores(TSMixer_wis)

MeanEnsemble_wis <- score_help(MeanEnsemble_score_help, obs, "MeanEnsemble")
summarise_scores(MeanEnsemble_wis)

all_scores_horizon <- bind_rows(
  scores_horizon(inferno_ln_wis, "00+"),
  scores_horizon(inferno_ln_wis, "00-04"),
  scores_horizon(inferno_ln_wis, "05-14"),
  scores_horizon(inferno_ln_wis, "15-34"),
  scores_horizon(inferno_ln_wis, "35-59"),
  scores_horizon(inferno_ln_wis, "60+"),
)
write.csv(all_scores_horizon, "Scores_Horizon_26.csv")





