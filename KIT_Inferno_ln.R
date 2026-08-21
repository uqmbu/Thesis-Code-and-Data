
rm(list = ls())


config <- list(
  data_url      = "https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/refs/heads/main/data/agi/are/latest_data-agi-are.csv",
  location      = "DE",
  model_name    = "KIT-Inferno_ln",   # Name
  output_dir    = "forecasts",        # Ordner für die wöchentlichen CSVs
  first_season  = 2012,               # erste potenzielle Trainingssaison (2012/13)
  covid_seasons = 2019:2022,          # ausgeschlossene Saisons wegen Covid-19: 2019/20 - 2022/23
  season_length = 30,                 # Wochen pro Saison
  n_ahead       = 4,                  # Forecast-Horizonte (Wochen)
  n_chains      = 1,
  n_iter        = 25000,
  n_burnin      = 12500,
  n_thin        = 2,
  n_cores       = max(1, min(parallel::detectCores() - 1, 6)),
  data_cutoff   = NULL                
)

# Pakete (werden bei Bedarf installiert)
# rjags benötigt zusätzlich das Systemprogramm JAGS:
#   https://mcmc-jags.sourceforge.io


required_packages <- c("rjags", "coda", "parallel")
for (p in required_packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}



# Anzahl ISO-Kalenderwochen eines Jahres (52 oder 53) (wichtig für erste Woche)
# Der 28.12. liegt immer in der letzten ISO-Woche des Jahres.
weeks_in_iso_year <- function(year) {
  as.integer(format(as.Date(sprintf("%d-12-28", year)), "%V"))
}

# Startwoche der Saison, die im Jahr `year` beginnt
season_start_week <- function(year) {
  ifelse(weeks_in_iso_year(year) == 53L, 42L, 41L)
}

# ISO-Jahr und ISO-Woche eines Datums
iso_year <- function(date) as.integer(format(date, "%G"))
iso_week <- function(date) as.integer(format(date, "%V"))

# Sonntag (Wochenende) der ISO-Woche `week` im ISO-Jahr `year`
sunday_of_iso_week <- function(year, week) {
  jan4 <- as.Date(sprintf("%d-01-04", year))        # Der 4. Januar liegt immer in KW 1
  wday_iso <- ((as.POSIXlt(jan4)$wday + 6L) %% 7L) + 1L  # Mo = 1, ..., So = 7
  monday_w1 <- jan4 - (wday_iso - 1L)
  monday_w1 + (week - 1L) * 7L + 6L
}


assign_season <- function(year, week) {
  start_this <- season_start_week(year)
  season_year <- ifelse(week >= start_this, year,
                        ifelse(week <= 18L, year - 1L, NA_integer_))
  season_week <- ifelse(week >= start_this, week - start_this + 1L,
                        ifelse(week <= 18L, week + 12L, NA_integer_))
  list(season_year = season_year, season_week = season_week)
}

season_label <- function(season_year) sprintf("%d/%d", season_year, season_year + 1)

# Daten

df <- read.csv(config$data_url)
df$date  <- as.Date(df$date)
df$week  <- as.numeric(df$week)
df$year  <- as.numeric(df$year)

# Nur die Werte aus gesamt DE werden berücksichtigt
df <- df[df$location == config$location, ]

# Optionaler Cutoff, um frühere Datenstände zu reproduzieren
if (!is.null(config$data_cutoff)) {
  df <- df[df$date <= as.Date(config$data_cutoff), ]
}

if (nrow(df) == 0) stop("Keine Daten fuer location ", config$location, " gefunden.")

# Saisonzuordnung (Zeilen ausserhalb der Saisonfenster: NA)
seas <- assign_season(df$year, df$week)
df$season_year <- seas$season_year
df$season_week <- seas$season_week

agegroups <- sort(unique(df$age_group))

# Zeitpunkt des Forecasts bestimmen

# `date` in den Daten ist der Sonntag, mit dem die Meldewoche endet.
last_date <- max(df$date)

# Erste Zielwoche = Woche nach der letzten Datenwoche
target1_date <- last_date + 7
t1 <- assign_season(iso_year(target1_date), iso_week(target1_date))

# Forecasts werden nur erzeugt, solange die erste Zielwoche in einer Saison
# liegt: von KW 42 (53-KW-Jahr) bzw. KW 41 (52-KW-Jahr) bis KW 18.
if (is.na(t1$season_week)) {
  next_season <- iso_year(last_date)
  if (iso_week(last_date) >= season_start_week(next_season)) {
    next_season <- next_season + 1
  }
  start_w <- season_start_week(next_season)
  first_data_sunday <- sunday_of_iso_week(next_season, start_w - 1L)
  first_fc_date <- first_data_sunday + 4  # Donnerstag der ersten Saisonwoche
  stop(sprintf(
    paste0("Letzter Datenstand: %s (KW %d/%d) liegt ausserhalb der Saison.\n",
           "Naechste Saison %s: %d KWs im Jahr %d -> Saisonstart in KW %d.\n",
           "Erster Forecast am %s (Donnerstag der KW %d), sobald Daten bis ",
           "einschliesslich KW %d (%s) vorliegen."),
    last_date, iso_week(last_date), iso_year(last_date),
    season_label(next_season), weeks_in_iso_year(next_season), next_season,
    start_w, first_fc_date, start_w, start_w - 1L, first_data_sunday), 
    call. = FALSE)
}

current_season <- t1$season_year
# Anzahl bereits beobachteter Wochen der laufenden Saison
# (0, wenn die Saison gerade erst beginnt)
last_obs <- t1$season_week - 1L

# Trainingssaisons dynamisch bestimmen:
# Alle vollständigen Saisons vor der laufenden Saison, ohne Covid-Saisons.
# Eine neue, komplett vorliegende Saison wird dadurch automatisch verwendet.

candidate_seasons <- setdiff(config$first_season:(current_season - 1),
                             config$covid_seasons)

season_is_complete <- function(sy) {
  sub <- df[!is.na(df$season_year) & df$season_year == sy, ]
  if (nrow(sub) == 0) return(FALSE)
  # vollständig = für jede Altersgruppe alle 30 Wochen mit positiven Werten
  all(vapply(agegroups, function(ag) {
    s_ag <- sub[sub$age_group == ag & !is.na(sub$value) & sub$value > 0, ]
    length(unique(s_ag$season_week)) == config$season_length
  }, logical(1)))
}

complete_flags <- vapply(candidate_seasons, season_is_complete, logical(1))
train_seasons  <- candidate_seasons[complete_flags]

if (length(train_seasons) < 2) {
  stop("Weniger als zwei vollstaendige Trainingssaisons verfuegbar.")
}
if (any(!complete_flags)) {
  message("Unvollstaendige Saisons uebersprungen: ",
          paste(season_label(candidate_seasons[!complete_flags]), collapse = ", "))
}

message(sprintf("Trainingssaisons (%d): %s", length(train_seasons),
                paste(season_label(train_seasons), collapse = ", ")))
message(sprintf("Laufende Saison %s: %d beobachtete Woche(n), Forecast fuer Saisonwochen %d-%d.",
                season_label(current_season), last_obs,
                last_obs + 1L, last_obs + config$n_ahead))

# Datensets für Inferno bereit machen
# Pro Altersgruppe: Trainingsmatrix [Saisons x 30 Wochen] und der aktuelle
# Saisonvektor (Länge 30, unbeobachtete Wochen = NA)

df_train <- df[!is.na(df$season_year) & df$season_year %in% train_seasons, ]
df_cur   <- df[!is.na(df$season_year) & df$season_year == current_season, ]

build_matrix <- function(ag) {
  sub <- df_train[df_train$age_group == ag, ]
  m <- matrix(NA_real_,
              nrow = length(train_seasons),
              ncol = config$season_length,
              dimnames = list(season_label(train_seasons),
                              paste0("week_", seq_len(config$season_length))))
  m[cbind(match(sub$season_year, train_seasons), sub$season_week)] <- sub$value
  m
}

age_grouped_list <- list()
for (ag in agegroups) {
  cur_sub <- df_cur[df_cur$age_group == ag, ]
  current_vec <- rep(NA_real_, config$season_length)
  current_vec[cur_sub$season_week] <- cur_sub$value
  current_vec[!is.na(current_vec) & current_vec <= 0] <- NA  # log benötigt > 0

  age_grouped_list[[ag]] <- list(
    train   = build_matrix(ag),
    current = current_vec
  )
}

# Jags String

inferno_jags_model <- "
model{
  for(t in 1:weeks){
  y_log[t] ~ dnorm(theta[t], tau)

  # Posteriore Praediktivverteilung
  ypred_log[t] ~ dnorm(theta[t], tau)

  # Ruecktransformation
  ypred[t] <- exp(ypred_log[t])

  # Link
  theta[t] <- gamma[t] + delta[t]
  }

  # GP-Kovarianzsstruktur, unveraendert
  delta[1:weeks] <- mu_s * ones[1:weeks] + invCholUpper %*% Z[1:weeks]

  for(t in 1:weeks){
  Z[t] ~ dnorm(0,1)
  }

  # Prior fuer Saisonmittel
  mu_s ~ dnorm(0, pow(sigma_mu, -2))

  }
"

# Step 1: Schätzung von theta_{s,t} - Benötigt reine Matrix einer Altersgruppe
# Zeilen = Saisons ; Spalten = Wochen

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
  tau_hat <- colMeans(y_log_matrix - beta_hat) # tau_hat: [T]

  # Geschätzte wahre Log-Counts
  theta_hat <- sweep(beta_hat, 2, tau_hat, "+") # theta_hat: [S x T]

  list(beta_hat = beta_hat, # beta_hat: [S x T]
       tau_hat = tau_hat, # tau_hat: [T]
       theta_hat = theta_hat) # theta_hat: [S x T]
}

# Step 2: Sigma^2 schätzen (Rauschparameter)
step2_estimate_sigma <- function(y_log, theta_hat){
  residuals <- y_log - theta_hat
  sigma2 <- sum(residuals^2) / (length(residuals) - 1)
  sqrt(sigma2) # Standardabweichung, nicht Varianz
}

# Step 3: gamma_t schätzen (typisches saisonales Log-Profil)
step3_estimate_gamma <- function(theta_hat){
  colMeans(theta_hat) # gamma_hat: [T]
}

# Step 4: Sigma^2_mu schätzen (Saison-zu-Saison-Variabilität)
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
    control = list(maxit = 5000, reltol = 1e-8)
  )

  # Parameter zurücktransformieren
  lambda_hat <- exp(opt$par[1])
  phi_hat <- plogis(opt$par[2])

  # Kovarianzmatrix bauen
  Sigma_hat <- build_Sigma(sigma2_S, lambda_hat, phi_hat)
  Sigma_hat <- Sigma_hat + diag(1e-8, weeks) # Numerische Stabilität

  list(sigma2_Sigma = sigma2_S,
       lambda_hat = lambda_hat,
       phi_hat = phi_hat,
       Sigma_hat = Sigma_hat)
}

# Inverse der oberen Cholesky-Zerlegung von Sigma^{-1} - JAGS input
compute_invCholUpper <- function(Sigma_hat){
  Sigma_inv <- solve(Sigma_hat) # Inverse von Sigma_hat
  chol_upper <- chol(Sigma_inv) # Obere Cholesky-Zerlegung von Sigma^{-1}
  invCholUpper <- solve(chol_upper) # Inverse der oberen Cholesky-Zerlegung
  invCholUpper
}

# Kovarianzmatrix auf weeks_total (Saison + Horizonte) erweitern
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
# y_obs: aktueller Saisonvektor der Länge 30, unbeobachtete Wochen = NA

step6_sample_forecasts <- function(y_obs,
                                   params,
                                   n_chains = 1,
                                   n_iter = 25000,
                                   n_burnin = 12500,
                                   n_thin = 2,
                                   n_ahead = 4){
  weeks_obs <- length(y_obs)
  weeks_total <- weeks_obs + n_ahead

  y_log_ext <- c(log(y_obs), rep(NA, n_ahead))

  gamma_ext <- c(params$gamma_hat,
                 rep(tail(params$gamma_hat, 1), n_ahead))

  Sigma_ext <- extend_Sigma(params$Sigma_hat,
                            params$lambda_hat,
                            params$phi_hat,
                            params$sigma2_Sigma,
                            weeks_total)

  invCholUpper <- compute_invCholUpper(Sigma_ext)

  jags_data <- list(weeks = weeks_total,
                    y_log = y_log_ext,
                    tau = 1 / (params$sigma_hat^2),
                    gamma = gamma_ext,
                    invCholUpper = invCholUpper,
                    sigma_mu = params$sigma_mu,
                    ones = rep(1, weeks_total))

  # Modell kompilieren
  model <- jags.model(textConnection(inferno_jags_model),
                      data = jags_data,
                      n.chains = n_chains,
                      quiet = TRUE)

  # Burn-in
  update(model, n.iter = n_burnin, progress.bar = "none")

  # Sampling
  samples <- coda.samples(model = model,
                          variable.names =
                            c("ypred", "ypred_log", "theta", "mu_s"),
                          n.iter = n_iter - n_burnin,
                          thin = n_thin,
                          progress.bar = "none")
  samples
}

# Forecasts für mehrere Horizonte
# last_obs = Anzahl beobachteter Saisonwochen (0 ist zu Saisonbeginn möglich)

get_horizon_forecasts <- function(samples, last_obs, horizons = 1:4){
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

run_inferno <- function(data_wide,
                        y_current,
                        last_obs,
                        n_ahead = 4,
                        n_chains = 1,
                        n_iter = 25000,
                        n_burnin = 12500,
                        n_thin = 2){
  # Log-Transformation der Trainingsdaten
  y_log <- log(data_wide)

  s1 <- step1_estimate_theta(y_log)
  sigma_hat <- step2_estimate_sigma(y_log, s1$theta_hat)
  gamma_hat <- step3_estimate_gamma(s1$theta_hat)
  s4 <- step4_estimate_sigma_mu(s1$theta_hat, gamma_hat)
  s5 <- step5_estimate_covariance(s4$delta_hat, s4$mu_hat)

  # Zusammenfassung der Parameter
  params <- list(sigma_hat = sigma_hat,
                 gamma_hat = gamma_hat,
                 sigma_mu = s4$sigma_mu,
                 delta_hat = s4$delta_hat,
                 mu_hat = s4$mu_hat,
                 sigma2_Sigma = s5$sigma2_Sigma,
                 lambda_hat = s5$lambda_hat,
                 phi_hat = s5$phi_hat,
                 Sigma_hat = s5$Sigma_hat,
                 tau_hat = s1$tau_hat)

  samples <- step6_sample_forecasts(y_obs = y_current,
                                    params = params,
                                    n_chains = n_chains,
                                    n_iter = n_iter,
                                    n_burnin = n_burnin,
                                    n_thin = n_thin,
                                    n_ahead = n_ahead)

  horizon_fc <- get_horizon_forecasts(samples = samples,
                                      last_obs = last_obs,
                                      horizons = 1:n_ahead)

  list(params = params,
       horizon_forecasts = horizon_fc)
}

# Parallel über die Altersgruppen (quasi wie die Regionen im Paper von Osthus)

run_inferno_parallel <- function(grouped_list,
                                 last_obs,
                                 n_cores = 3,
                                 n_ahead = 4,
                                 n_iter = 25000,
                                 n_burnin = 12500,
                                 n_thin = 2){
  cl <- makeCluster(n_cores)
  on.exit(stopCluster(cl), add = TRUE)

  # Funktionen und Pakete an worker übergeben
  clusterEvalQ(cl, library(rjags))
  clusterExport(cl, varlist = c("run_inferno", "step1_estimate_theta",
                                "step2_estimate_sigma",
                                "step3_estimate_gamma",
                                "step4_estimate_sigma_mu",
                                "step5_estimate_covariance",
                                "compute_invCholUpper",
                                "extend_Sigma",
                                "step6_sample_forecasts",
                                "get_horizon_forecasts",
                                "inferno_jags_model",
                                "grouped_list", "last_obs",
                                "n_ahead", "n_iter", "n_burnin", "n_thin"),
                envir = environment())

  results <- parLapply(cl = cl, X = names(grouped_list),
                       fun = function(ag){
                         age <- grouped_list[[ag]]
                         tryCatch(
                           run_inferno(
                             data_wide = age$train,
                             y_current = age$current,
                             last_obs = last_obs,
                             n_ahead = n_ahead,
                             n_chains = 1,
                             n_iter = n_iter,
                             n_burnin = n_burnin,
                             n_thin = n_thin
                           ),
                           error = function(e){
                             warning(sprintf("Fehler bei Altersgruppe %s: %s",
                                             ag, e$message))
                             NULL
                           }
                         )
                       })

  names(results) <- names(grouped_list)
  results
}

# Ausführen: ein Forecast-Durchlauf für den aktuellen Datenstand

results <- run_inferno_parallel(age_grouped_list,
                                last_obs = last_obs,
                                n_cores = config$n_cores,
                                n_ahead = config$n_ahead,
                                n_iter = config$n_iter,
                                n_burnin = config$n_burnin,
                                n_thin = config$n_thin)

# Output im Submission-Format 
# forecast_date   = Donnerstag der Woche, in der der Forecast erstellt wird  (= letzter Daten-Sonntag + 4 Tage)
# target_end_date = Sonntag der Zielwoche

forecast_date <- last_date + 4

quantile_map <- c(lower_95 = 0.025, lower_80 = 0.1, lower_50 = 0.25,
                  median = 0.5, upper_50 = 0.75, upper_80 = 0.9,
                  upper_95 = 0.975)

make_long <- function(dfh, ag){
  rows <- lapply(seq_len(nrow(dfh)), function(i){
    h <- dfh$horizon[i]
    base <- data.frame(
      location = config$location,
      age_group = ag,
      forecast_date = forecast_date,
      target_end_date = last_date + 7 * h,
      horizon = h,
      stringsAsFactors = FALSE
    )
    mean_row <- cbind(base, data.frame(type = "mean", quantile = NA_real_,
                                       value = dfh$forecast[i]))
    quant_rows <- do.call(rbind, lapply(names(quantile_map), function(m){
      cbind(base, data.frame(type = "quantile",
                             quantile = unname(quantile_map[m]),
                             value = round(dfh[[m]][i])))
    }))
    rbind(mean_row, quant_rows)
  })
  do.call(rbind, rows)
}

submission <- do.call(rbind, lapply(names(results), function(ag){
  if (is.null(results[[ag]])) return(NULL)
  make_long(results[[ag]]$horizon_forecasts, ag)
}))

if (is.null(submission) || nrow(submission) == 0) {
  stop("Keine Forecasts erzeugt - alle Altersgruppen fehlgeschlagen.")
}

submission <- submission[order(submission$age_group, submission$horizon,
                               submission$type, submission$quantile), ]

# quote = c(1, 2, 6): nur location, age_group und type in Anführungszeichen,

dir.create(config$output_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(config$output_dir,
                      sprintf("%s-agi-are-%s.csv",
                              format(forecast_date), config$model_name))

write.table(submission, out_file, sep = ",", quote = c(1, 2, 6),
            row.names = FALSE, na = "NA")

message(sprintf("Forecast geschrieben: %s (%d Zeilen)", out_file, nrow(submission)))
