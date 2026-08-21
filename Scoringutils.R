rm(list = ls())

# Debugging mit Claude Sonnett 5


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

obs_2 <- obs

obs_2$observed[obs_2$date == "2025-02-02"] <- obs_2$observed[obs_2$date == "2025-02-02"] * 0.6

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

scores_horizon <- function(scoring_results){
  scoring_results %>% 
    filter(age_group != "00+") %>%
    summarise_scores(by = c("model", "horizon"))
}

scores_horizon_age <- function(scoring_results, agegroup){
  scoring_results %>%
    filter(age_group == agegroup) %>%
    summarise_scores(by = c("model", "horizon"))
}

inferno_check <- read.csv("Inferno_check_score_help.csv")


inferno_score_help <- read.csv("Inferno_score_help.csv")
inferno_nb_score_help <- read.csv("Inferno_nb_score_help.csv")
moa_score_help <- read.csv("MoA_score_help.csv")
moa_lognormal_score_help <- read.csv("MoA_lognormal_score_help.csv")

hhh4_score_help <- read.csv("hhh4_score_help.csv")
hhh4_christmas_score_help <- read.csv("hhh4_christmas_score_help.csv")
TSMixer_score_help <- read.csv("TSMixer_score_help.csv")
LightGBM_score_help <- read.csv("LightGBM_score_help.csv")
MeanEnsemble_score_help <- read.csv("MeanEnsemble_score_help.csv")
EnsembleComplete_score_help <- read.csv("EnsembleComplete_score_help.csv")


inferno_check_wis <- score_help(inferno_check, obs_2, "Inferno Check")
summarise_scores(inferno_check_wis)

inferno_wis <- score_help(inferno_score_help, obs, "Inferno Log-Normal")
summarise_scores(inferno_wis)

inferno_nb_wis <- score_help(inferno_nb_score_help, obs, "Inferno Negative Binomial")
summarise_scores(inferno_nb_wis)

moa_wis <- score_help(moa_score_help, obs, "MoA Negative Binomial")
summarise_scores(moa_wis)

moa_lognormal_wis <- score_help(moa_lognormal_score_help, obs, "MoA Log-Normal")
summarise_scores(moa_lognormal_wis)



hhh4_wis <- score_help(hhh4_score_help, obs, "hhh4")
summarise_scores(hhh4_wis)

hhh4_christmas_wis <- score_help(hhh4_christmas_score_help, 
                                 obs, "hhh4_christmas")
summarise_scores(hhh4_christmas_wis)

tsmixer_wis <- score_help(TSMixer_score_help, obs, "TSMixer")
summarise_scores(tsmixer_wis)

lightgbm_wis <- score_help(LightGBM_score_help, obs, "LightGBM")
summarise_scores(lightgbm_wis)

meanensemble_wis <- score_help(MeanEnsemble_score_help, obs, "MeanEnsemble")
summarise_scores(meanensemble_wis)

ensemblecomplete_wis <- score_help(EnsembleComplete_score_help, 
                                   obs,"EnsembleComplete")
summarise_scores(ensemblecomplete_wis)

inferno_scores_age <- bind_rows(
  scores_age(inferno_wis),
  scores_age(inferno_check_wis)
)

write.csv(inferno_scores_age, "Inferno Check Scores.csv")

all_scores_age <- bind_rows(
  scores_age(inferno_wis),
  scores_age(inferno_nb_wis),
  scores_age(moa_wis),
  scores_age(moa_lognormal_wis),
  scores_age(hhh4_wis),
  scores_age(hhh4_christmas_wis),
  scores_age(tsmixer_wis),
  scores_age(lightgbm_wis),
  scores_age(meanensemble_wis),
  scores_age(ensemblecomplete_wis)
)

write.csv(all_scores_age, "Scores_Age.csv")


inferno_scores_horizon <- bind_rows(
  scores_horizon(inferno_wis),
  scores_horizon(inferno_check_wis)
)

write.csv(inferno_scores_horizon, "Inferno Check Scores Horizon.csv")

hori_scores <- function(agegroup){
    all_scores_horizon <- bind_rows(
      scores_horizon_age(inferno_wis, agegroup),
      scores_horizon_age(inferno_nb_wis, agegroup),
      scores_horizon_age(moa_wis, agegroup),
      scores_horizon_age(moa_lognormal_wis, agegroup),
      scores_horizon_age(hhh4_wis, agegroup),
      scores_horizon_age(tsmixer_wis, agegroup),
      scores_horizon_age(lightgbm_wis, agegroup),
      scores_horizon_age(meanensemble_wis, agegroup),
    )
    title <- paste0("Scores_Horizon_", agegroup, ".csv")
    write.csv(all_scores_horizon, title)
}

hori_scores("60+")

all_scores_horizon_00plus <- bind_rows(
  scores_horizon_00plus(inferno_wis),
  scores_horizon_00plus(inferno_nb_wis),
  scores_horizon_00plus(moa_wis),
  scores_horizon_00plus(moa_lognormal_wis),
  scores_horizon_00plus(hhh4_wis),
  scores_horizon_00plus(hhh4_christmas_wis),
  scores_horizon_00plus(tsmixer_wis),
  scores_horizon_00plus(lightgbm_wis),
  scores_horizon_00plus(meanensemble_wis),
  scores_horizon_00plus(ensemblecomplete_wis)
)

write.csv(all_scores_horizon_00plus, "Scores_Horizon_00plus.csv")


