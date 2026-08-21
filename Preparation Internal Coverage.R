rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)
library(purrr)


url <- "https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/refs/heads/main/data/agi/are/latest_data-agi-are.csv"

obs <- read.csv(url) %>%
  filter(location == "DE") %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(date, age_group, observed = value)


model_files <- c(
  "Inferno Log-Normal"        = "Inferno_ln_score_help_26.csv")

age_groups <- c("00+", "00-04", "05-14", "15-34", "35-59", "60+")

intervals <- tibble::tribble(
  ~nominal, ~lower, ~upper,
  "50%",     0.25,   0.75,
  "80%",     0.10,   0.90,
  "95%",     0.025,  0.975
)

coverage_one <- function(file, model_name) {
  if (!file.exists(file)) {
    return(NULL)
  }
  
  fc <- read.csv(file) %>%
    mutate(target_end_date = as.Date(target_end_date)) %>%
    dplyr::select(age_group, target_end_date, horizon, quantile, value)
  
  purrr::pmap_dfr(intervals, function(nominal, lower, upper) {
    fc %>%
      filter(quantile %in% c(lower, upper)) %>%
      mutate(bound = ifelse(quantile == lower, "lo", "hi")) %>%
      dplyr::select(-quantile) %>%
      pivot_wider(names_from = bound, values_from = value) %>%
      inner_join(obs, by = c("target_end_date" = "date", "age_group")) %>%
      filter(!is.na(lo), !is.na(hi), !is.na(observed)) %>%
      mutate(covered = observed >= lo & observed <= hi) %>%
      group_by(age_group, horizon) %>%           # <-- Altersgruppe bleibt erhalten
      summarise(
        n        = n(),
        coverage = 100 * mean(covered),
        .groups  = "drop"
      ) %>%
      mutate(nominal = nominal)
  }) %>%
    mutate(model = model_name)
}

coverage_long <- purrr::imap_dfr(model_files, ~ coverage_one(.x, .y))

coverage_table <- coverage_long %>%
  mutate(
    model     = factor(model, levels = names(model_files)),
    age_group = factor(age_group, levels = age_groups)
  ) %>%
  dplyr::select(model, age_group, horizon, nominal, coverage) %>%
  pivot_wider(names_from = nominal, values_from = coverage) %>%
  complete(model, age_group, horizon = 1:4) %>%   
  arrange(age_group, model, horizon)

fmt <- function(x) ifelse(is.na(x), "--", sprintf("%.1f", x))

coverage_fmt <- coverage_table %>%
  mutate(across(all_of(intervals$nominal), fmt))

write.csv(coverage_fmt, "coverage_table_by_agegroup.csv", row.names = FALSE)



help1 <- function(csv, obs, model_name, ag){
  forecasts <- csv %>% 
    mutate(forecast_date = as.Date(forecast_date),
           target_end_date = as.Date(target_end_date),
           model = model_name)
  
  forecasts <- forecasts %>%
    group_by(target_end_date, horizon, age_group, quantile) %>%
    filter(forecast_date == max(forecast_date)) %>%
    ungroup()
  
  scoring_df <- forecasts %>% 
    left_join(obs, by = c("target_end_date" = "date", "age_group")) %>%
    rename(predicted = value, quantile_level = quantile) %>% 
    filter(!is.na(observed)) 
  
  scoring_df <- scoring_df %>% 
    rename(true_value = observed, category = age_group, prediction = predicted)
  
  scoring_df <- scoring_df %>%
    dplyr::select(model, category, horizon, target_end_date, quantile_level, prediction, true_value) %>%
    filter(category == ag)
  
  return(scoring_df)
}
setwd("C:/Users/runeg/OneDrive/Dokumente/Karlsruher Institut für Technologie - Studium/Bachelorarbeit/Code")


inferno_score_help <- read.csv("Inferno_score_help.csv")
inferno_nb_score_help <- read.csv("Inferno_nb_score_help.csv")
moa_lognormal_score_help <- read.csv("MoA_lognormal_score_help.csv")
moa_nb_score_help <- read.csv("MoA_score_help.csv")
hhh4_score_help <- read.csv("hhh4_score_help.csv")
hhh4_christmas_score_help <- read.csv("hhh4_christmas_score_help.csv")
LightGBM_score_help <- read.csv("LightGBM_score_help.csv")
TSMixer_score_help <- read.csv("TSMixer_score_help.csv")
EnsembleComplete_score_help <- read.csv("EnsembleComplete_score_help.csv")
MeanEnsemble_score_help <- read.csv("MeanEnsemble_score_help.csv")





inferno_ln <- help1(inferno_score_help, obs, "Inferno_LogNormal", "60+")
inferno_nb <- help1(inferno_nb_score_help, obs, "Inferno_NegBin", "60+")
moa_ln <- help1(moa_lognormal_score_help, obs, "MoA_LogNormal", "60+")
moa_nb <- help1(moa_nb_score_help, obs, "MoA_NegBin", "60+")
hhh4 <- help1(hhh4_score_help, obs, "hhh4", "60+")
hhh4_christmas <- help1(hhh4_christmas_score_help, obs, "hhh4_christmas", "00+") # Nur für 00+
lightgbm <- help1(LightGBM_score_help, obs, "LightGBM", "60+")
tsmixer <- help1(TSMixer_score_help, obs, "TSMixer", "60+")
ecomp <- help1(EnsembleComplete_score_help, obs, "EnsembleComplete", "00+") # Nur für 00+
mens <- help1(MeanEnsemble_score_help, obs, "MeanEnsemble", "60+")

quant_list <- list(inferno_ln, inferno_nb, moa_ln, moa_nb, hhh4, lightgbm,
                   tsmixer, mens)
quantile_forecasts <- bind_rows(quant_list)
setDT(quantile_forecasts)
models_of_interest <- c("Inferno_LogNormal", "Inferno_NegBin", 
                        "MoA_LogNormal", "MoA_NegBin",
                        "hhh4", "LightGBM", "TSMixer", "MeanEnsemble")


interval_levels <- list(
  "50" = c(lower = 0.25, upper = 0.75),
  "80" = c(lower = 0.10, upper = 0.90),
  "95" = c(lower = 0.025, upper = 0.975)
)


compute_coverage <- function(qf, level_name, bounds) {
  d <- qf[quantile_level %in% bounds, ]
  
  d_wide <- dcast(d, model + category + horizon + target_end_date + true_value ~ quantile_level,
                  value.var = "prediction")
  
  setnames(d_wide,
           old = as.character(bounds),
           new = c("q_lower", "q_upper"))
  
  d_wide[, covered := true_value >= q_lower & true_value <= q_upper]
  
  agg <- d_wide[, .(
    n_total   = .N,
    n_covered = sum(covered)
  ), by = .(model, category)]
  
  wilson <- mapply(function(x, n) {
    if (n == 0) return(c(lower = NA_real_, upper = NA_real_))
    res <- prop.test(x, n, correct = FALSE)$conf.int
    c(lower = res[1], upper = res[2])
  }, agg$n_covered, agg$n_total)
  
  agg[, `:=`(
    coverage  = n_covered / n_total,
    ci_lower  = wilson["lower", ],
    ci_upper  = wilson["upper", ],
    level     = level_name
  )]
  
  agg[]
}

coverage_dt <- rbindlist(lapply(names(interval_levels), function(lvl) {
  compute_coverage(quantile_forecasts, lvl, interval_levels[[lvl]])
}))

coverage_dt <- coverage_dt[model %in% models_of_interest]
coverage_dt[, model := factor(model, levels = models_of_interest)]


coverage_dt[, nominal_level := as.numeric(level) / 100]
coverage_dt[, binom_p_value := mapply(function(x, n, p0) {
  binom.test(x, n, p = p0)$p.value
}, n_covered, n_total, nominal_level)]



export_dt <- coverage_dt[, .(model, category_type = category, level, coverage,
                             ci_lower, ci_upper, n_total, binom_p_value)]

write.csv(export_dt, "Coverage_Horizon_60plus.csv")
