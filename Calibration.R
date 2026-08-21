rm(list = ls())
library(dplyr)
library(readr)
library(scoringutils)
library(data.table)
setwd("C:/Users/runeg/OneDrive/Dokumente/Karlsruher Institut für Technologie - Studium/Bachelorarbeit/Code")


coverage_stats <- function(x, p_nominal) {
  n <- length(x)
  k <- sum(x)
  test <- binom.test(k, n, p = p_nominal)
  ci <- test$conf.int
  list(
    n = n,
    coverage = k / n,
    ci_lower = ci[1],
    ci_upper = ci[2],
    p_value = test$p.value
  )
}

# help 
nominal <- c(unc_int_50 = 0.50, unc_int_80 = 0.80, unc_int_95 = 0.95)

build_stats <- function(dt, by_col) {
  dt[, {
    out <- list()
    for (col in names(nominal)) {
      s <- coverage_stats(get(col), nominal[[col]])
      out[[paste0(col, "_n")]]         <- s$n
      out[[paste0(col, "_coverage")]]  <- s$coverage
      out[[paste0(col, "_ci_lower")]]  <- s$ci_lower
      out[[paste0(col, "_ci_upper")]]  <- s$ci_upper
      out[[paste0(col, "_p_value")]]   <- s$p_value
    }
    out
  }, by = by_col]
}

# main
overlap_check <- function(df) {
  
  calibration <- as.data.table(df)[, .(
    horizon,
    age_group,
    unc_int_50 = observed >= lower_50 & observed <= upper_50,
    unc_int_80 = observed >= lower_80 & observed <= upper_80,
    unc_int_95 = observed >= lower_95 & observed <= upper_95
  )]
  

  cal_horizons     <- build_stats(copy(calibration)[, horizon := as.character(horizon)], "horizon")
  cal_horizons_all <- build_stats(copy(calibration)[, horizon := as.character(horizon)][, horizon := "total"], "horizon")
  cal_horizons     <- rbind(cal_horizons_all, cal_horizons)
  

  cal_agegroups <- build_stats(calibration, "age_group")
  
 
  cal_horizon_by_age <- calibration[, {
    out <- list()
    for (col in names(nominal)) {
      s <- coverage_stats(get(col), nominal[[col]])
      out[[paste0(col, "_n")]]         <- s$n
      out[[paste0(col, "_coverage")]]  <- s$coverage
      out[[paste0(col, "_ci_lower")]]  <- s$ci_lower
      out[[paste0(col, "_ci_upper")]]  <- s$ci_upper
      out[[paste0(col, "_p_value")]]   <- s$p_value
    }
    out
  }, by = .(age_group, horizon)]
  cal_horizon_by_age[, horizon := as.character(horizon)]
  
  # zusammenführen
  setnames(cal_horizons, "horizon", "category")
  cal_horizons[, category_type := "horizon"]
  
  setnames(cal_agegroups, "age_group", "category")
  cal_agegroups[, category_type := "age_group"]
  
  setnames(cal_horizon_by_age, "horizon", "category")
  cal_horizon_by_age[, category_type := "horizon_by_age"]
  
  cal_combined <- rbindlist(list(cal_horizons, cal_agegroups, cal_horizon_by_age), fill = TRUE)
  
  print(cal_combined)
  return(cal_combined)
}


inferno_ln_pre <- read.csv("Inferno_ln_overlap.csv")
inferno_nb_pre <- read.csv("Inferno_nb_overlap.csv")
moa_ln_pre     <- read.csv("MoA_ln_overlap.csv")
moa_nb_pre     <- read.csv("MoA_nb_overlap.csv")


inferno_ln_overlap <- overlap_check(inferno_ln_pre)
inferno_nb_overlap <- overlap_check(inferno_nb_pre)
moa_ln_overlap     <- overlap_check(moa_ln_pre)
moa_nb_overlap     <- overlap_check(moa_nb_pre)

# zusammführen
inferno_ln_overlap[, model := "Inferno_ln"]
inferno_nb_overlap[, model := "Inferno_nb"]
moa_ln_overlap[, model := "MoA_ln"]
moa_nb_overlap[, model := "MoA_nb"]

all_overlap <- rbindlist(list(inferno_ln_overlap, inferno_nb_overlap,
                              moa_ln_overlap, moa_nb_overlap))



library(ggplot2)

plot_calibration_horizon <- function(dt, age_group_filter, nominal_level, level_name) {
  d <- dt[category_type == "horizon_by_age" & age_group == age_group_filter]
  
  ggplot(d, aes(x = category, 
                y = get(paste0(level_name, "_coverage")))) +
    geom_hline(yintercept = nominal_level, linetype = "dashed", color = "grey40") +
    geom_point(size = 2, color = "darkgreen") +
    geom_errorbar(aes(ymin = get(paste0(level_name, "_ci_lower")),
                      ymax = get(paste0(level_name, "_ci_upper"))),
                  width = 0.2, color = "darkgreen") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    labs(x = "Forecast Horizon", y = "Empirical Coverage") +
    theme_minimal()
}

# Coverage pro horizont
plot_calibration_horizon(all_overlap[model == "Inferno_ln"], 
                         "00+", 0.50, "unc_int_50")

# Alle modelle
library(tidyr)


prep_coverage_long <- function(dt) {
  d <- dt[category_type == "age_group"]
  
  long <- rbindlist(list(
    d[, .(category, model, level = "50%", nominal = 0.50,
          coverage = unc_int_50_coverage,
          ci_lower = unc_int_50_ci_lower,
          ci_upper = unc_int_50_ci_upper)],
    d[, .(category, model, level = "80%", nominal = 0.80,
          coverage = unc_int_80_coverage,
          ci_lower = unc_int_80_ci_lower,
          ci_upper = unc_int_80_ci_upper)],
    d[, .(category, model, level = "95%", nominal = 0.95,
          coverage = unc_int_95_coverage,
          ci_lower = unc_int_95_ci_lower,
          ci_upper = unc_int_95_ci_upper)]
  ))
  

  long[, level := factor(level, levels = c("50%", "80%", "95%"))]
  age_order <- c("00+", setdiff(sort(unique(long$category)), "00+"))
  long[, category := factor(category, levels = age_order)]
  
  long
}


plot_coverage_stacked <- function(long_dt) {
  ggplot(long_dt, aes(x = category, y = coverage, color = model)) +
    geom_hline(aes(yintercept = nominal), linetype = "dashed", color = "grey40") +
    geom_point(position = position_dodge(width = 0.4), size = 2) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  position = position_dodge(width = 0.4), width = 0.2) +
    facet_wrap(~ level, ncol = 1) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    labs(x = "Age Group", y = "Empirical Coverage", color = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


all_long <- prep_coverage_long(all_overlap)


plot_coverage_stacked(all_long[model %in% c("MoA_ln", "MoA_nb")])


plot_coverage_stacked(all_long[model %in% c("Inferno_ln", "Inferno_nb")])


