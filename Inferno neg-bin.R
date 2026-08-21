rm(list = ls())

library(rjags)
library(coda)
library(parallel)
library(dplyr)
library(tidyr)
library(MASS)

# Jags String

inferno_jags_model_nb <- "
model{
  for(t in 1:weeks){
  mu[t] <- exp(theta[t]) # Rücktransformation auf Zählskala
  p[t] <- r / (r + mu[t]) # NB-Parametrisierung über Mittelwert und Size
  y[t] ~ dnegbin(p[t], r)
  
  # Posteriore Prädiktivverteilung
  ypred[t] ~ dnegbin(p[t], r)
  
  
  # Link
  theta[t] <- gamma[t] + delta[t]
  }
  
  # GP-Kovarianzsstruktur, unverändert
  delta[1:weeks] <- mu_s * ones[1:weeks] + invCholUpper %*% Z[1:weeks]
  
  for(t in 1:weeks){
  Z[t] ~ dnorm(0,1) 
  }
  
  # Prior für Saisonmittel
  mu_s ~ dnorm(0, pow(sigma_mu, -2))
  
  }
"


# Daten 
url <- "https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/refs/heads/main/data/agi/are/latest_data-agi-are.csv"

df <- read.csv(url)
# Nur die Werte aus gesamt DE werden berücksichtigt
df <- df[df$location == "DE", ] 
# Alle irrelevanten Daten rauslöschen - Trainingsdaten
# Alles ab KW 13 - KW 40 löschen und für 2015 und 2020 auch die 41
df$date <- as.Date(df$date)
df$week <- as.numeric(df$week)
df$year <- as.numeric(df$year)
# Saison-Namen
season <- c("2012/2013", "2013/2014", "2014/2015", "2015/2016", "2016/2017",
            "2017/2018", "2018/2019", "2023/2024")
# Nur ganze Saisons (24 Wochen nach KW 40, außer 2015), kein Covid
df_train <- df %>% filter(week < 19 | week > 40) %>% 
                     filter(date != "2015-10-11") %>%
                     filter(date < "2019-10-13" | date > "2023-05-10") %>%
                     filter(date < "2024-10-13")

for(i in 1:(nrow(df_train)/30)){
  for(w in 1:30){
    c <- (i-1)*30+w
    df_train$week[c] <- w
  }
}

for(i in 1:6){
  for(s in 1:8){
    for(w in 1:30){
      c <- (i-1)*8*30+(s-1)*30+w
      df_train$season_name[c] <- season[s]
      df_train$season[c] <- s
    }
  }
}


# Testdaten
df_test <- df %>% filter(date >= "2024-10-13" & date <= "2025-05-04")
for(i in 1:(nrow(df_test)/30)){
  for(w in 1:30){
    c <- (i-1)*30+w
    df_test$week[c] <- w
  }
}
# Datensets für Inferno bereit machen
# Jede Spalte eine Saison mit Namen
# Fängt an mit 00+ an
# Danach genauso für die einzelnen Altersgruppen

train_wide <- df_train %>% 
  group_by(season, age_group) %>% 
  arrange(week) %>% 
  mutate(week_nr = row_number()) %>%
  dplyr::select(season, age_group, season_name, week_nr, value) %>%  
  pivot_wider(names_from = week_nr, values_from = value) %>%
  rename_with(~ paste0("week_", .), starts_with(as.character(1:30))) %>%
  ungroup()

# Pro Altersgruppe in eine reine Matrix umwandeln
build_matrix <- function(df_wide, ag){
  sub <- df_wide %>% filter(age_group == ag) %>% arrange(season)
  m <- sub %>% dplyr::select(starts_with("week_"))  %>% as.matrix()
  rownames(m) <- sub$season_name
  m
}

# Pro Altersgruppe eine Matrix erzeugen
agegroupes <- unique(train_wide$age_group)

age_grouped_list <- list()
for(ag in agegroupes){
  age_grouped_list[[ag]] <- list(
    train = build_matrix(train_wide, ag),
    current = NULL
  )
}

for(ag in agegroupes){
  test_sub <- df_test %>% filter(age_group == ag) %>% arrange(week)
  
  current_vec <- rep(NA, 30)
  current_vec[test_sub$week] <- test_sub$value
  
  age_grouped_list[[ag]]$current <- current_vec
}


# Step 1: Schätzung von theta_{s,t} - Benötigt reine Matrix einer Altersgruppe
# Zeilen = Saisons ; Spalten = Wochen
# Train_list ist schon ganz gut, muss, bevor es in die Funktion kommt
# noch gefiltert werden
# weeks = T, aber T ist schon für TRUE gespeichert, weshalb im Folgenden 
# statt T, weeks verwendet wird

step1_estimate_theta <- function(y_log_matrix){
  S <- nrow(y_log_matrix) # Anzahl Saisons
  weeks <- ncol(y_log_matrix) # Anzahl Wochen pro Saison
  
  # gleitender 3-Wochen Durchschnitt beta_hat
  beta_hat <- matrix(NA, S, weeks)
  
  for(s in 1:S){
    beta_hat[s, 1] <- mean(y_log_matrix[s, 1:2])
    beta_hat[s, weeks] <- mean(y_log_matrix[s, (weeks - 1):weeks])
    
    for(t in 2:(weeks-1)){
      beta_hat[s, t] <- mean(y_log_matrix[s, (t-1):(t+1)])
    }
  }
  
  # tau_hat: systematische Abweichung über alle Saisons
  tau_hat <- colMeans(y_log_matrix - beta_hat) # theta_hat: [T]
  
  # Geschätzte wahre Log-Counts
  theta_hat <- sweep(beta_hat, 2, tau_hat, "+") # theta_hat: [S x T]
  
  list(beta_hat = beta_hat, # beta_hat: [S x T] 
       tau_hat = tau_hat, # tau_hat: [T]
       theta_hat = theta_hat) # theta_hat: [S x T]
  
}

# Step 2: Sigma^2 schätzen (Rauschparameter)
# y_log: [S x T]
# theta_hat: [S x T]
step2_estimate_r <- function(y_raw, theta_hat){
  df_fit <- data.frame(
    y = as.vector(y_raw),
    theta = as.vector(theta_hat)
  )
  
  model <- MASS::glm.nb(y ~ -1 + offset(theta), data = df_fit)
  model$theta # NB-Size-Parameter r
}

# Step 3: gamma_t schätzen (typisches saisonales Log_Profil) (was ist das)
# theta_hat: [S x T]
step3_estimate_gamma <- function(theta_hat){
  colMeans(theta_hat) # gamma_hat: [T]
}

# Step 4: Sigma^2_mu schätzen (Saison-zu-Saison-Variabilität)
# theta_hat: [S x T]
# gamma_hat: [T]

step4_estimate_sigma_mu <- function(theta_hat, gamma_hat){
  S <- nrow(theta_hat)
  weeks <- ncol(theta_hat)
  
  # Saisonale Abweichungen vom typischen Profil
  delta_hat <- sweep(theta_hat, 2, gamma_hat, "-") # delta_hat: [S x T]
  
  # Durchschnittliche Abweichung pro Saison
  mu_hat <- rowMeans(delta_hat) # mu_hat: [S]
  
  # Unbiased sample variance der Saisonmittel
  sigma_mu <- sd(mu_hat) # sd() teilt durch (S-1)
  
  list(delta_hat = delta_hat, # delta_hat: [S x T]
       mu_hat = mu_hat, # mu_hat: [S]
       sigma_mu = sigma_mu) # Standardabweichung
}

# Step 5: sigma^2_Sigma, lambda, phi schätzen (GP-Kovarianzparameter)
# delta_hat: [S x T]
# mu_hat: [S]
step5_estimate_covariance <- function(delta_hat, mu_hat){
  S <- nrow(delta_hat)
  weeks <- ncol(delta_hat)
  
  # Marginale Varianz sigma^2_Sigma
  centered <- delta_hat - outer(mu_hat, rep(1, weeks))
  sigma2_S <- sum(centered^2) / (S * weeks - 1)
  
  # GP-Kovarianzfunktion
  # Sigma_{t, t'} = phi * sigma2_S * exp(-lambda*(t-t')^2) für t != t'
  # Sigma_{t, t} = sigma2_S
  build_Sigma <- function(sigma2_S, lambda, phi){
    Sigma <- matrix(0, weeks, weeks)
    for(t in 1:weeks){
      for(tp in 1:weeks){
        if(t == tp){
          Sigma[t, tp] <- sigma2_S
        } else {
          Sigma[t, tp] <- phi * sigma2_S * exp(-lambda * (t-tp)^2)
        }
      }
    }
    Sigma
  }
  
  # Negative Log-Likelihood über alle Trainingssaisons
  neg_log_lik <- function(params){
    lambda <- exp(params[1]) # log-Transformation für Positivität
    phi <- plogis(params[2]) # logit-Transformation für [0,1]
    
    Sigma <- build_Sigma(sigma2_S, lambda, phi)
    
    # Numerische Stabilität: kleine Diagonale - Hinzufügen von Jitter
    # Cholesky-Zerlegung benötigt positiv definite Matrix
    Sigma <- Sigma + diag(1e-8, weeks) 
    
    tryCatch({
      chol_S <- chol(Sigma)
      nll <- 0
      for(s in 1:S){
        d_centered <- delta_hat[s, ] - mu_hat[s]
        # log-Dichte der MVN
        log_det <- 2 * sum(log(diag(chol_S)))
        quad <- sum(backsolve(chol_S, d_centered, transpose = TRUE)^2)
        nll <- nll + 0.5 * (weeks * log(2 * pi) + log_det + quad)
      }
      nll
    }, error = function(e) 1e10)
  }
  
  # Optimierung mit Startpunkten
  opt <- optim(
    par = c(log(0.01), qlogis(0.8)), # Startpunkte
    fn = neg_log_lik, # zu optimierende Funktion
    method = "Nelder-Mead", # Optimierungsalgorithmus 
    # Nutzung von Nelder-Mead, da Likelihood-Funktion mit Matrixoperationen
    # zu komplex für analytische Ableitungen ist. Nelder-Mead basiert auf
    # der Simplexmethode und ist in der Lage, lokale Optima zu finden
    control = list(maxit = 5000, reltol = 1e-8) # maximale Iterationen, Toleranz
  )
  
  # Parameter zurücktransformieren
  lambda_hat <- exp(opt$par[1])
  phi_hat <- plogis(opt$par[2])
  
  # Kovarianzmarix bauen
  Sigma_hat <- build_Sigma(sigma2_S, lambda_hat, phi_hat)
  Sigma_hat <- Sigma_hat + diag(1e-8, weeks) # Numerische Stabilität
  
  list(sigma2_Sigma = sigma2_S, 
       lambda_hat = lambda_hat,
       phi_hat = phi_hat,
       Sigma_hat = Sigma_hat)
}

# Inverse der oberen Cholesky-Zerlegung von sigma^{-1} - JAGS input
compute_invCholUpper <- function(Sigma_hat){
  
  Sigma_inv <- solve(Sigma_hat) # Inverse von Sigma_hat
  
  chol_upper <- chol(Sigma_inv) # Obere Cholesky-Zerlegung von Sigma^{-1}
  
  invCholUpper <- solve(chol_upper) # Inverse der oberen Cholesky-Zerlegung
  
  invCholUpper
}

# 
extend_Sigma <- function(Sigma_hat, lambda_hat, phi_hat, sigma2_S, weeks_total){
  Sigma_ext <- matrix(0, weeks_total, weeks_total)
  for(t in 1:weeks_total){
    for(tp in 1:weeks_total){
      if(t == tp){
        Sigma_ext[t, tp] <- sigma2_S
      } else {
        Sigma_ext[t, tp] <- phi_hat * sigma2_S * exp(-lambda_hat * (t - tp)^2)
      }
    }
  }
  Sigma_ext + diag(1e-8, weeks_total)
}
# Step 6: MCMC-Sampling mit JAGS
# Anzahl Iterationen, Burn-in Länge und Thinning-Faktor basieren auf Paper

step6_sample_forecasts_nb <- function(y_obs, # Beobachtete Counts [T]
                                   params, # Liste mit geschätzten Parametern
                                   n_chains = 1, # Anzahl MCMC-Ketten
                                   n_iter = 25000, # Gesamtzahl Iterationen
                                   n_burnin = 12500, # Burn-in (50% von n_iter)
                                   n_thin = 2,
                                   n_ahead = 4){ # Forecasted weeks
  weeks_obs <- length(y_obs)
  weeks_total <- weeks_obs + n_ahead
  
  y_ext <- c(y_obs, rep(NA, n_ahead))
  
  gamma_ext <- c(params$gamma_hat, 
                 rep(tail(params$gamma_hat, 1), n_ahead))
  
  Sigma_ext <- extend_Sigma(params$Sigma_hat,
                            params$lambda_hat,
                            params$phi_hat,
                            params$sigma2_Sigma, 
                            weeks_total)
  
  invCholUpper <- compute_invCholUpper(Sigma_ext)
  
  jags_data <- list(weeks = weeks_total,
                    y = y_ext,
                    r = params$r_hat,
                    gamma = gamma_ext,
                    invCholUpper = invCholUpper,
                    sigma_mu = params$sigma_mu,
                    ones = rep(1, weeks_total)
    
  )
  
  # Modell kompilieren
  model <- jags.model(textConnection(inferno_jags_model_nb),
                      data = jags_data,
                      n.chains = n_chains,
                      quiet = TRUE
  )
  
  # Burn-in 
  update(model, n.iter = n_burnin, progress.bar = "none")
  
  # Sampling 
  samples <- coda.samples(model = model, 
                          variable.names = 
                            c("ypred", "theta", "mu_s"),
                          n.iter = n_iter - n_burnin,
                          thin = n_thin,
                          progress.bar = "none")
  
  samples
}

# Forecasts für mehrere Horizonte
get_horizon_forecasts <- function(samples, y_current, horizons = 1:4){
  last_obs <- max(which(!is.na(y_current)))
  
  samples_mat <- do.call(rbind, lapply(samples, as.matrix))
  ypred_cols <- grep("^ypred\\[", colnames(samples_mat), value = TRUE)
  ypred_mat <- samples_mat[, ypred_cols, drop = FALSE]
  
  results <- lapply(horizons, function(h){
    target_week <- last_obs + h 
    
    if(target_week > ncol(ypred_mat)){
      
      return(NULL)
    }
    col_name <- paste0("ypred[", target_week, "]")
    draws <- ypred_mat[, col_name]
    
    
    data.frame(
      week_T = last_obs,
      week_target = target_week,
      horizon = h, 
      forecast = mean(draws),
      lower_95 = quantile(draws, 0.025),
      lower_80 = quantile(draws, 0.1),
      lower_50 = quantile(draws, 0.25),
      median = quantile(draws, 0.5),
      upper_50 = quantile(draws, 0.75),
      upper_80 = quantile(draws, 0.9),
      upper_95 = quantile(draws, 0.975),
      row.names = NULL
    )
  }) 
  
  do.call(rbind, Filter(Negate(is.null), results))
}

# Inferno für eine Gruppe

run_inferno_nb <- function(data_wide,
                        y_current,
                        n_ahead = 4,
                        n_chains = 1,
                        n_iter = 25000,
                        n_burnin = 12500,
                        n_thin = 2,
                        verbose = TRUE){
  # Log-Transformation der Trainingsdaten für die Mittelwertstruktur 
  # Steps 1,3,4,5
  y_log <- log(data_wide)
  
  s1 <- step1_estimate_theta(y_log)
  
  # r wird auf rohen counts geschätzt, mit theta_hat als Offset
  r_hat <- step2_estimate_r(data_wide, s1$theta_hat)
  
  gamma_hat <- step3_estimate_gamma(s1$theta_hat)
  
  s4 <- step4_estimate_sigma_mu(s1$theta_hat, gamma_hat)
  
  s5 <- step5_estimate_covariance(s4$delta_hat, s4$mu_hat)
  
  # Zusammenfassung der Parameter
  params <- list(r_hat = r_hat,
                 gamma_hat = gamma_hat,
                 sigma_mu = s4$sigma_mu,
                 delta_hat = s4$delta_hat,
                 mu_hat = s4$mu_hat,
                 sigma2_Sigma = s5$sigma2_Sigma,
                 lambda_hat = s5$lambda_hat,
                 phi_hat = s5$phi_hat,
                 Sigma_hat = s5$Sigma_hat,
                 tau_hat = s1$tau_hat
  )

  samples <- step6_sample_forecasts_nb(y_obs = y_current,
                                       params = params,
                                       n_chains = n_chains,
                                       n_iter = n_iter,
                                       n_burnin = n_burnin,
                                       n_thin = n_thin,
                                       n_ahead = n_ahead)
  
  horizon_fc <- get_horizon_forecasts(samples = samples, 
                                      y_current = c(y_current, 
                                                    rep(NA, n_ahead)))
  
  list(params = params, 
       samples = samples, 
       s1 = s1,
       horizon_forecasts = horizon_fc)
  
}





run_inferno_parallel_nb <- function(grouped_list, 
                        n_cores = 3, # Ich habe leider nur 4
                        # Für self-contained kann man auch detectCores() - 1 machen
                        n_ahead = 4,
                        n_iter = 25000,
                        n_burnin = 12500,
                        n_thin = 2
){
  # Cluster erstellen 
  cl <- makeCluster(n_cores)
  
  # Funktionen und Pakete übergeben
  clusterEvalQ(cl, {library(rjags); library(MASS)})
  clusterExport(cl, varlist = c("run_inferno_nb", "step1_estimate_theta",
                                "step2_estimate_r", 
                                "step3_estimate_gamma", 
                                "step4_estimate_sigma_mu",
                                "step5_estimate_covariance",
                                "compute_invCholUpper", 
                                "extend_Sigma",
                                "step6_sample_forecasts_nb",
                                "get_horizon_forecasts",
                                "inferno_jags_model_nb",
                                "grouped_list",
                                "n_ahead", "n_iter", "n_burnin", "n_thin"),
                envir = environment())
  
  results <- parLapply(cl = cl, X = names(grouped_list),
                       fun = function(ag){
                         age <- grouped_list[[ag]]
                         tryCatch(
                           run_inferno_nb(
                             data_wide = age$train,
                             y_current = age$current,
                             n_ahead = n_ahead,
                             n_chains = 1,
                             n_iter = n_iter,
                             n_burnin = n_burnin,
                             n_thin = n_thin,
                             verbose = FALSE
                           ), 
                           error = function(e){
                             warning(sprintf("Fehler bei Altersgruppe %s: %s", 
                                             ag, e$message))
                             NULL
                           }
                         )
                       })
  stopCluster(cl)
  
  names(results) <- names(grouped_list)
  results
}

run_inferno_multi_nb <- function(grouped_list, last_obs_weeks = 1:30, 
                              n_cores = 3, n_ahead = 4, n_iter = 25000,
                              n_burnin = 12500, n_thin){
  all_results <- list()
  
  for(k in last_obs_weeks){
    grouped_k <- lapply(grouped_list, function(ag){
      current_full <- ag$current
      current_k <- current_full
      if(k < length(current_full)){
        current_k[(k+1):length(current_k)] <- NA
      }
      list(train = ag$train, current = current_k)
    })
    
    res_k <- run_inferno_parallel_nb(grouped_k, n_cores = n_cores,
                                     n_ahead = n_ahead, n_iter = n_iter,
                                     n_burnin = n_burnin, n_thin = n_thin)
    
    all_results[[paste0("week_", k)]] <- res_k
  }
  
  all_results
}







# Ausführen (mit allen Daten aus Saison 24/25)
results_multi_nb <- run_inferno_multi_nb(age_grouped_list, n_cores = 3,
                                         n_ahead = 4, n_iter = 25000, 
                                         n_burnin = 12500, n_thin = 2)

# Zusammenfassung der Forecasts
summarize_forecasts <- function(samples, probs = c(0.025, 0.1, 0.25, 0.5,
                                                   0.75, 0.9, 0.975)){
  # Alle Chains in eine Matrix
  samples_mat <- do.call(rbind, lapply(samples, as.matrix))
  
  # Nur ypred-Spalten
  ypred_cols <- grep("^ypred\\[", colnames(samples_mat), value = TRUE)
  ypred_mat <- samples_mat[, ypred_cols, drop = FALSE]
  
  # Quantile pro Woche
  quants <- apply(ypred_mat, 2, quantile, probs = probs)
  weeks <- ncol(ypred_mat)
  
  result <- data.frame(t = 1:weeks)
  for(p in probs){
    col_name <- paste0("q", p * 100)
    result[[col_name]] <- quants[paste0(p * 100, "%"), ]
  }
  result
}

# mehrwöchige Forecasts

all_horizons <- do.call(rbind, lapply(names(results_multi_nb), function(wk){
  res_k <- results_multi_nb[[wk]]
  
  do.call(rbind, lapply(names(res_k), function(ag){
    if(is.null(res_k[[ag]])) return(NULL)
    dfh <- res_k[[ag]]$horizon_forecasts
    dfh$age_group <- ag
    dfh
  }))
}))

all_horizons$observed <- mapply(function(ag, wt){
  vec <- age_grouped_list[[ag]]$current
  if(wt <= length(vec)) vec[wt] else NA
}, all_horizons$age_group, all_horizons$week_target)


# MAL GUCKEN
# Daten der Wochen hinzufügen - Zielformat Visualisierungen
week_updates <- data.frame(week_count = rep(1:30), Woche = as.Date(rep(NA, 30)))
week_updates$Woche[1] <- as.Date("2024-10-13")
for(i in 2:30){
  week_updates$Woche[i] <- as.Date(week_updates$Woche[i-1] + 7)
}

all_horizons$Woche_T <- week_updates$Woche[match(all_horizons$week_T, 
                                           week_updates$week_count)]
all_horizons$Woche_target <- week_updates$Woche[match(all_horizons$week_target,
                                                week_updates$week_count)]
            

# korrektes Zielformat

all_horizons_long <- all_horizons %>% rename(forecast_date = Woche_T,
                                             target_end_date = Woche_target) %>%
                                      mutate(location = "DE") %>% 
                                      pivot_longer(
                                        cols = c(forecast, lower_95, lower_80,
                                                 lower_50, median, upper_50,
                                                 upper_80, upper_95),
                                        names_to = "metric",
                                        values_to = "value"
                                      ) %>% 
                                      mutate(
                                        type = if_else(metric == "forecast", 
                                                       "mean", "quantile"),
                                        quantile = case_when(
                                          metric == "lower_95" ~ 0.025,
                                          metric == "lower_80" ~ 0.1,
                                          metric == "lower_50" ~ 0.25,
                                          metric == "median" ~ 0.5,
                                          metric == "upper_50" ~ 0.75,
                                          metric == "upper_80" ~ 0.9,
                                          metric == "upper_95" ~ 0.975,
                                          metric == "forecast" ~ NA_real_
                                          )
                                      ) %>% 
                                      dplyr::select(location, age_group, forecast_date,
                                             target_end_date, horizon, type,
                                             quantile, value) %>%
                                      arrange(forecast_date, age_group, horizon,
                                              type, quantile)
inferno_long_final <- all_horizons_long %>%
  mutate(
    forecast_date = forecast_date + 4
  )
                                      

write.csv(inferno_long_final, "Inferno_nb_final.csv")
  
