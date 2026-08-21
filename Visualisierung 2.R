rm(list = ls())
# Debugging mit Claude Sonnet 5

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)

# Visualisierungen von Punktvorhersagen und probabilistischen Vorhersagen
url <- "https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/refs/heads/main/data/agi/are/latest_data-agi-are.csv"
obs <- read.csv(url) %>%
  filter(location == "DE") %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date, age_group, observed = value)

obs_2 <- obs

obs_2$observed[obs_2$date == "2025-02-02"] <- obs_2$observed[obs_2$date == "2025-02-02"] * 0.6


# Für Visualisierung vorbereiten
vis_pre <- function(csv, obs){
  forecasts <- csv %>%
    dplyr::select(-1) %>%
    mutate(forecast_date = as.Date(forecast_date),
           target_end_date = as.Date(target_end_date))
  # Zusammenführen
  forecasts_merged <- forecasts %>% 
    left_join(obs, by = c("target_end_date" = "date", "age_group")) %>%
    filter(!is.na(observed)) 
  
  # Weites Format
  forecast_wide <- forecasts_merged %>%
    dplyr::select(location, age_group, forecast_date, target_end_date,
                  horizon, quantile, value, observed) %>%
    pivot_wider(names_from = quantile, values_from = value, 
                names_prefix = "q") %>%
    rename(
      lower_95 = q0.025, lower_80 = q0.1, lower_50 = q0.25, median = q0.5,
      upper_50 = q0.75, upper_80 = q0.9, upper_95 = q0.975
    )

  return(forecast_wide)
}




# Punktvorhersagen
vis_function_point <- function(df, ag){
  plot_data <- df %>% 
    filter(age_group == ag) %>%
    arrange(forecast_date, horizon)
  
  obs_line <- plot_data %>% 
    distinct(target_end_date, observed)
  
  
  ggplot(plot_data, aes(x = target_end_date)) + 
    # Vorhersagen
    geom_line(aes(y = median, group = forecast_date, color = "Forecast"),
              linewidth = 0.7, alpha = 0.6) + 
    geom_point(aes(y = median, color = "Forecast"), size = 1, alpha = 0.6) + 
    #Beobachtete Werte
    geom_line(data = obs_line, aes(y = observed, color = "Observed"), linewidth = 0.7) + 
    geom_point(data = obs_line, aes(y = observed, color = "Observed"), 
               size = 1) + 
    scale_y_continuous(
      limits = c(0, 3e5),
      expand = expansion(mult = c(0, 0.02)),
      breaks = seq(0, 3e6, by = 1e5),
      labels = scales::label_scientific(digits = 2),
      oob = scales::oob_squish
    ) +
    scale_color_manual(values = c("Forecast" = "darkgreen", "Observed" = "black")) + 
    labs(title = NULL, x = "Target Week", y = "Consultations", color = NULL) + 
    theme_minimal() + 
    theme(legend.position = "bottom")
  
}


# Probabilistische Vorhersagen
# Ganze Saison - alle paar Wochen gibt es Vorhersagen, starten nach offset

vis_function_prob <- function(df, ag, interval_weeks = 8, 
                              offset_weeks = 0, fan_color = "darkgreen") {
  
  df_ag <- df %>% filter(age_group == ag)
  
  # verfügbare Daten
  all_dates <- sort(unique(df_ag$forecast_date))
  
  # Startpunkt auf Offset
  start_date <- min(all_dates) + as.difftime(offset_weeks, units = "weeks")
  
  # Target Dates festlegen
  target_dates <- seq(start_date, max(all_dates), by = paste(interval_weeks, "weeks"))
  
  
  selected_dates <- sapply(target_dates, function(d) {
    all_dates[which.min(abs(all_dates - d))]
  }) %>% 
    as.Date(origin = "1970-01-01") %>% 
    unique()
  
  # plot
  plot_data <- df_ag %>% 
    filter(forecast_date %in% selected_dates) %>%
    arrange(forecast_date, horizon)
  
  # Observationen über ganze Saison
  obs_line <- df_ag %>% 
    distinct(target_end_date, observed) %>%
    arrange(target_end_date)
  
  
  ggplot(plot_data, aes(x = target_end_date)) +
    # Markierung der Woche, auf der die Vorhersagen basieren
    geom_vline(xintercept = selected_dates, linetype = "dashed", 
               color = "grey30", linewidth = 0.3) +
    # Unsicherheitsintervalle
    geom_ribbon(aes(ymin = lower_95, ymax = upper_95, group = forecast_date),
                fill = fan_color, alpha = 0.2) +
    geom_ribbon(aes(ymin = lower_80, ymax = upper_80, group = forecast_date),
                fill = fan_color, alpha = 0.3) +
    geom_ribbon(aes(ymin = lower_50, ymax = upper_50, group = forecast_date),
                fill = fan_color, alpha = 0.45) +
    # punktvorhersagen
    geom_line(aes(y = median, group = forecast_date, 
              color = "Forecast"), linewidth = 0.7) +
    geom_point(aes(y = median, group = forecast_date,
               color = "Forecast"), size = 1) +
    # Observationen
    geom_line(data = obs_line, aes(y = observed, 
              color = "Observed"), linewidth = 0.7) +
    geom_point(data = obs_line, aes(y = observed, 
               color = "Observed"), size = 1) +
    # Variation je nach Altersgruppe und Anzahl Konsultationen
    scale_y_continuous(
      limits = c(0, 5e5),
      expand = expansion(mult = c(0, 0.02)),
      breaks = seq(0, 5e5, by = 1e5),
      labels = scales::label_scientific(digits = 2),
      oob = scales::oob_squish
    ) +
    scale_color_manual(values = c("Forecast" = fan_color, "Observed" = "black"),
                       name = NULL)+
    labs(title = NULL, x = "Target Week", y = "Consultations") +
    theme_minimal() +
    theme(legend.position = "bottom")
}



setwd("C:/Users/runeg/OneDrive/Dokumente/Karlsruher Institut für Technologie - Studium/Bachelorarbeit/Code")


# 2026
inferno_26_pre <- read.csv("Inferno_ln_score_help_26.csv")
inferno_26_vis <- vis_pre(inferno_26_pre, obs)

inferno_26_1_pre <- read.csv("Inferno_ln_score_help_26_1.csv")
inferno_26_1_vis <- vis_pre(inferno_26_1_pre, obs)

LightGBM_26_pre <- read.csv("KIT-LightGBM_26.csv")
LightGBM_26_vis <- vis_pre(LightGBM_26_pre, obs)



# Inferno with log-normal distributed consultations
inferno_ln_pre <- read.csv("Inferno_score_help.csv")
inferno_ln_vis <- vis_pre(inferno_ln_pre, obs)

inferno_check_pre <- read.csv("Inferno_check_score_help.csv")
inferno_check_vis <- vis_pre(inferno_check_pre, obs)
inferno_check_vis2 <- vis_pre(inferno_check_pre, obs_2)

# Inferno with negative binomial distributed consultations
inferno_nb_pre <- read.csv("Inferno_nb_score_help.csv")
inferno_nb_vis <- vis_pre(inferno_nb_pre, obs)

# Method of Analogues with uncertainty intervals base on negative binomial distribution
moa_nb_pre <- read.csv("MoA_score_help.csv")
moa_nb_vis <- vis_pre(moa_nb_pre, obs)

# Method of Analogues with uncertainty intervals base on log-normal distribution
moa_ln_pre <- read.csv("MoA_lognormal_score_help.csv")
moa_ln_vis <- vis_pre(moa_ln_pre, obs)

# hhh4
hhh4_pre <- read.csv("hhh4_score_help.csv")
hhh4_vis <- vis_pre(hhh4_pre, obs)
# hhh4_christmas
hhh4_christmas_pre <- read.csv("hhh4_christmas_score_help.csv")
hhh4_christmas_vis <- vis_pre(hhh4_christmas_pre, obs)
# TSMixer
tsmixer_pre <- read.csv("TSMixer_score_help.csv")
tsmixer_vis <- vis_pre(tsmixer_pre, obs)
# LightGBM
lightGBM_pre <- read.csv("LightGBM_score_help.csv")
lightGBM_vis <- vis_pre(lightGBM_pre, obs)
# MeanEnsemble
meanEnsemble_pre <- read.csv("MeanEnsemble_score_help.csv")
meanEnsemble_vis <- vis_pre(meanEnsemble_pre, obs)
# EnsembleComplete
ensembleComplete_pre <- read.csv("EnsembleComplete_score_help.csv")
ensembleComplete_vis <- vis_pre(ensembleComplete_pre, obs)




# Visualisierung Horizonte - ganze Saison
vis_function_point(inferno_ln_vis, "00+")
vis_function_point(inferno_check_vis, "00+")
vis_function_point(inferno_check_vis2, "00+")


vis_function_point(inferno_26_vis, "00-04")
vis_function_point(inferno_26_1_vis, "00+")
vis_function_point(LightGBM_26_vis, "00+")



vis_function_point(inferno_nb_vis, "00+")
vis_function_point(moa_ln_vis, "00+")
vis_function_point(moa_nb_vis, "60+")
vis_function_point(hhh4_vis, "00+")
vis_function_point(hhh4_christmas_vis, "00+") # nur 00+ verfügbar
vis_function_point(lightGBM_vis, "00+")
vis_function_point(tsmixer_vis, "00+")
vis_function_point(meanEnsemble_vis, "00+")
vis_function_point(ensembleComplete_vis, "00+")

# Visualisierung Prob - Final dann offset=1,2,3,4
vis_function_prob(inferno_ln_vis, "60+", interval_weeks = 5, offset_weeks = 4) # Done
vis_function_prob(inferno_nb_vis, "05-14", interval_weeks = 5, offset_weeks = 4) # Done 

vis_function_prob(moa_ln_vis, "05-14", interval_weeks = 5, offset_weeks = 3) # Done
vis_function_prob(moa_nb_vis, "05-14", interval_weeks = 5, offset_weeks = 3) # Done

vis_function_prob(hhh4_vis, "00+", interval_weeks = 5, offset_weeks = 4) # Done
vis_function_prob(hhh4_christmas_vis, "00+", interval_weeks = 5, offset_weeks = 4) # Done

vis_function_prob(lightGBM_vis, "00+", interval_weeks = 5, offset_weeks = 4) # Done

vis_function_prob(tsmixer_vis, "00+", interval_weeks = 5, offset_weeks = 4) # Done

vis_function_prob(meanEnsemble_vis, "00+", interval_weeks = 5, offset_weeks = 4) # Done
vis_function_prob(ensembleComplete_vis, "00+", interval_weeks = 5, offset_weeks = 4) # Done


vis_function_prob(inferno_check_vis, "00+", interval_weeks = 5, offset_weeks = 1)

vis_function_prob(inferno_26_vis, "00-04", interval_weeks = 5, offset_weeks = 1) # Done

