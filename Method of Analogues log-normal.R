rm(list = ls())


# Altersgruppen für gesamt Deutschland
library(AER)
library(MASS)
library(zoo)
library(tidyr)
library(dplyr)

# Daten
url <- "https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/refs/heads/main/data/agi/are/latest_data-agi-are.csv"

df <- read.csv(url)

df_age <- df[df$location == "DE", ] # Nur die Werte aus gesamt DE werden berücksichtigt

# Altersgruppen
# Jede Altersgruppe zu einer Spalte machen
ili_wide_age <- df_age |> 
  pivot_wider(names_from = age_group,
              values_from = value)
# Zeilen in Daten umbenennen und Matrix erstellen
ili_matrix_age <- as.matrix(ili_wide_age[, !colnames(ili_wide_age) %in% 
                                           c("date", "year", "week", "location")])
rownames(ili_matrix_age) <- as.character(ili_wide_age$date)

all_dates <- as.Date(rownames(ili_matrix_age))

# Aufsplitten der Matrix: "00+" wird separat behandelt, da es die Summe
# der übrigen Altersgruppen ist und sonst wegen der größeren Fallzahlen
# die Distanzberechnung dominieren würde
age_cols <- c("00-04", "05-14", "15-34", "35-59", "60+")
ili_matrix_groups <- ili_matrix_age[, age_cols]
ili_matrix_total  <- ili_matrix_age[, "00+", drop = FALSE]

# Zeitfenster

train_end <- as.Date("2017-10-08") # Trainingsdaten bis zu Beginn der relevante Periode 2017/18
test_start <- as.Date("2017-10-15") # Eine Woche später fängt dann das Testdatenset an
test_end <- as.Date("2024-10-06") # Testdaten bis zu Beginn der relevanten Periode 2024/25
covid_start <- as.Date("2020-03-15") # Covid-19 Pause start - Woche vor erstem Lockdown
covid_end <- as.Date("2023-02-05") # Woche nachdem das RKI das Risiko von Covid 19 heruntergesetzt hat und auch die Maskenpflicht an öffentlichen Orten aufhörte

# Indizes: Trainingsperiode = alles bis train_end
train_idx <- which(all_dates <= train_end)

# Testperiode: test_start bis test_end, ohne COVID-Block
test_dates <- all_dates[all_dates >= test_start & all_dates <= test_end &
                          !(all_dates >= covid_start & all_dates <= covid_end)]
test_idx   <- which(all_dates %in% test_dates)

# Activity Matrix

make_activity_matrix_age <- function(mat, T_index, l) {
  rows <- rev((T_index - l):T_index)
  mat[rows, ,drop=FALSE] 
}

# Hilfe Distanzberechnung

compute_distances_age <- function(mat, T_index, l, h, valid_neighbor_idx) {
  # Nachbarn dürfen nur aus dem Trainingsset stammen UND müssen h Wochen
  # Vorhersagezeitraum voraus haben (d.h. t + h muss im Datensatz existieren).
  max_t <- nrow(mat) - h
  t_range <- intersect(valid_neighbor_idx, (l + 1):max_t)
  
  X_T <- make_activity_matrix_age(mat, T_index, l)
  
  dists <- sapply(t_range, function(t) {
    X_t <- make_activity_matrix_age(mat, t, l)
    sum((X_T - X_t)^2)
  })
  
  data.frame(t = t_range, distance = dists) |>
    dplyr::arrange(distance)
}

# Hauptfunktion Method of Analogues
run_moa <- function(mat, all_dates,
                    test_idx, valid_neighbors,
                    l = 4, v = 3, H = 4) {
  
  age_groups   <- colnames(mat)
  n_age        <- length(age_groups)
  results_list <- vector("list", length(test_idx) * H * n_age)
  idx_res      <- 1L
  
  for (T_now in test_idx) {
    
    if (T_now <= l) next
    
    for (h in 1:H) {
      
      target_idx <- T_now + h
      if (target_idx > nrow(mat)) next
      
      distances <- compute_distances_age(mat, T_now,
                                         l = l, h = h,
                                         valid_neighbor_idx = valid_neighbors)
      if (nrow(distances) < v) next
      
      nearest        <- head(distances, v)
      nearest$weight <- (1 / nearest$distance) / sum(1 / nearest$distance)
      
      # Gewichtete Vorhersage je Altersgruppe (funktioniert für 1 oder mehrere Spalten)
      forecast_vec <- Reduce(`+`, lapply(1:v, function(i) nearest$weight[i] * mat[nearest$t[i] + h, ]))
      observed_vec <- mat[target_idx, ]
      
      for (ag in seq_along(age_groups)) {
        results_list[[idx_res]] <- data.frame(
          date_T      = all_dates[T_now],
          date_target = all_dates[target_idx],
          horizon     = h,
          age_group   = age_groups[ag],
          forecast    = forecast_vec[ag],
          observed    = observed_vec[ag]
        )
        idx_res <- idx_res + 1L
      }
    }
  }
  
  dplyr::bind_rows(results_list)
}


# Parameter
l <- 4 # Anzahl vergangener Wochen im Aktivitätsvektor (Basierend auf dem Paper)
v <- 3 # Anzahl nächster Nachbarn (Basierend auf dem Paper)
H <- 4 # maximaler Vorhersagehorizont

# Nur Trainingsindizes werden als potentielle Nachbarn angesehen
# ein Nachbar t braucht l Vorläufer -> t > 1
valid_neighbors_train <- train_idx[train_idx > 1]

# Dispersionsparameter-Schätzung:
# Erste Berechnung: die 5 Altersgruppen gemeinsam (multivariate Distanz)
predictions_groups <- run_moa(ili_matrix_groups, all_dates,
                              test_idx        = test_idx,
                              valid_neighbors = valid_neighbors_train,
                              l = l, v = v, H = H)

# Zweite Berechnung: "00+" separat 
predictions_total <- run_moa(ili_matrix_total, all_dates,
                             test_idx        = test_idx,
                             valid_neighbors = valid_neighbors_train,
                             l = l, v = v, H = H)

# Zusammenführen
predictions_df <- bind_rows(predictions_groups, predictions_total)

# Datensatz splitten
# Nach Altersgruppen
pred_all <- predictions_df |> filter(age_group == "00+")
pred_00_04 <- predictions_df |> filter(age_group == "00-04")
pred_05_14 <- predictions_df |> filter(age_group == "05-14")
pred_15_34 <- predictions_df |> filter(age_group == "15-34")
pred_35_59 <- predictions_df |> filter(age_group == "35-59")
pred_60plus <- predictions_df |> filter(age_group == "60+")
# Nach Horizonten

horizonte <- function(daten){
  
  name_help <- deparse(substitute(daten))

  for(i in 1:4){
    neu <- paste0(name_help, "_", i)
    filtered <- daten |> filter(horizon == i)
    assign(neu, filtered, envir = .GlobalEnv)
  }
}

# 00+
horizonte(pred_all) 
# 00-04
horizonte(pred_00_04)
# 05-14
horizonte(pred_05_14)
# 15-34
horizonte(pred_15_34)
# 35-59
horizonte(pred_35_59)
# 60+
horizonte(pred_60plus)
# Regression
# Funktion um Überdispersionsparameter zu schätzen
fit_sigma_log <- function(df_name){
  df_sigma_log_help <- get(df_name)
  model <- lm(log(observed) ~ -1 + offset(log(forecast)), 
              data = df_sigma_log_help)
  return(summary(model)$sigma) 
}

names_sigma_help <- c(paste0("pred_all_", 1:4),
                      paste0("pred_00_04_", 1:4),
                      paste0("pred_05_14_", 1:4),
                      paste0("pred_15_34_", 1:4),
                      paste0("pred_35_59_", 1:4),
                      paste0("pred_60plus_", 1:4))

sigmas_est <- sapply(names_sigma_help, fit_sigma_log)

sigmas_df_est <- data.frame( modell = names_sigma_help, 
                             sigma_log_est = sigmas_est)

# Ab jetzt nach wie vor die nächsten Werte basierend auf den Punktvorhersagen
# schätzen und die psis darum herum
# Dafür muss ich wieder alle Daten (außer die Covid-Daten)
# für die Punktvorhersagen nutzen
# Ab hier noch mal ein neues Set eröffnen, 
# dass alle Daten nutzt und dann die Methode genau gleich nutzen

punkt_dates <- all_dates[all_dates <= test_end & 
                         !(all_dates >= covid_start & all_dates <= covid_end)]
punkt_idx <- which(all_dates %in% punkt_dates)

# Punktvorhersagen (Mittelwerte)
# Trainingsset: Alles bis zum Ende der Testdaten und ohne Covid
# Vorhersageset: Saison 2024/25 (2024-10-13 bis 2025-05-04)

future_start <- as.Date("2024-10-13")
future_end   <- as.Date("2025-05-04")

# Trainingsset für Phase 2: alles bis Ende der Testdaten ohne Covid
punkt_dates <- all_dates[all_dates <= test_end &
                           !(all_dates >= covid_start & all_dates <= covid_end)]
punkt_idx   <- which(all_dates %in% punkt_dates)


valid_neighbors_full <- punkt_idx[punkt_idx > l]

# Vorhersageset: Saison 2024/25
future_dates <- all_dates[all_dates >= future_start & all_dates <= future_end]
future_idx   <- which(all_dates %in% future_dates)

future_predictions_groups <- run_moa(ili_matrix_groups, all_dates,
                                     test_idx        = future_idx,
                                     valid_neighbors = valid_neighbors_full,
                                     l = l, v = v, H = H)

future_predictions_total <- run_moa(ili_matrix_total, all_dates,
                                    test_idx        = future_idx,
                                    valid_neighbors = valid_neighbors_full,
                                    l = l, v = v, H = H)

future_predictions_df <- bind_rows(future_predictions_groups, future_predictions_total)

# Unsicherheitsintervalle 
sigma_log_lookup <- sigmas_df_est |>
  mutate(
    horizon   = as.integer(sub(".*_(\\d)$", "\\1", modell)),
    age_group = case_when(
      grepl("pred_all_",    modell) ~ "00+",
      grepl("pred_00_04_",  modell) ~ "00-04",
      grepl("pred_05_14_",  modell) ~ "05-14",
      grepl("pred_15_34_",  modell) ~ "15-34",
      grepl("pred_35_59_",  modell) ~ "35-59",
      grepl("pred_60plus_", modell) ~ "60+"
    )
  ) |>
  dplyr::select(age_group, horizon, sigma_log = sigma_log_est)

# Join + Intervalle berechnen
future_intervals_df <- future_predictions_df |>
  left_join(sigma_log_lookup, by = c("age_group", "horizon")) |>
  mutate(
    # 50%-Intervall
    lower_50 = qlnorm(0.25, meanlog = log(forecast), sdlog = sigma_log),
    upper_50 = qlnorm(0.75, meanlog = log(forecast), sdlog = sigma_log),
    # 80%-Intervall
    lower_80 = qlnorm(0.1, meanlog = log(forecast), sdlog = sigma_log),
    upper_80 = qlnorm(0.9, meanlog = log(forecast), sdlog = sigma_log),
    # 95%-Intervall
    lower_95 = qlnorm(0.025, meanlog = log(forecast), sdlog = sigma_log),
    upper_95 = qlnorm(0.975, meanlog = log(forecast), sdlog = sigma_log)
  )

# Aufbereitung für das Zielformat
moa_long_1 <- future_intervals_df |> 
  rename(
    forecast_date = date_T, 
    target_end_date = date_target
  ) |> 
  mutate(location = "DE") |>
  pivot_longer(
    cols = c(lower_95, lower_80, lower_50, upper_50, upper_80, upper_95),
    names_to = "metric",
    values_to = "value") |>
  mutate(
    type = "quantile",
    quantile = case_when(
      metric == "lower_95" ~ 0.025,
      metric == "lower_80" ~ 0.1,
      metric == "lower_50" ~ 0.25,
      metric == "upper_50" ~ 0.75,
      metric == "upper_80" ~ 0.9,
      metric == "upper_95" ~ 0.975
    ) 
  ) |> 
  dplyr::select(location, age_group, forecast_date, target_end_date, 
                horizon, type, quantile, value) |>
  arrange(forecast_date, age_group, horizon, quantile)

moa_median <- future_intervals_df |>
  rename(forecast_date = date_T,
         target_end_date = date_target) |>
  mutate(
    location = "DE",
    type = "quantile",
    quantile = 0.5,
    value = forecast
  ) |>
  dplyr::select(location, age_group, forecast_date, target_end_date, 
                horizon, type, quantile, value)

moa_long <- bind_rows(moa_long_1, moa_median) |>
  arrange(forecast_date, age_group, horizon, quantile)

# Die Daten, an denen die Forecasts gemacht wurden, vom Sonntag auf den 
# folgenden Montag ändern
moa_long_final <- moa_long |> 
  mutate(
    forecast_date = forecast_date + 4
  )

write.csv(moa_long_final, "MoA_long_final_log_normal.csv")


