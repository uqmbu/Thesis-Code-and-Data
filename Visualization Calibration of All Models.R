rm(list = ls())
# Debugging mit Claude Sonnet 5

library(tidyverse)
library(ggplot2)
library(data.table)



coverage_files <- list.files(
  setwd("C:/Users/runeg/OneDrive/Dokumente/Karlsruher Institut für Technologie - Studium/Bachelorarbeit/Code"),
  pattern = "^Coverage_Horizon_.*\\.csv$",
  full.names = TRUE
)


coverage_all <- rbindlist(
  lapply(coverage_files, function(f) {
    dt <- fread(f)
    dt[, V1 := NULL]
    dt
  })
)


setnames(coverage_all, "category_type", "category")
coverage_all[, nominal := as.numeric(level) / 100]


coverage_all[, category := factor(
  category,
  levels = c( "00+", "00-04", "05-14", "15-34", "35-59", "60+")
)]


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


# Grafiken, die pro Altersgruppe die empirische Coverage zeigen
plot_coverage_stacked(coverage_all[model %in% c("MoA_LogNormal", "MoA_NegBin")])
plot_coverage_stacked(coverage_all[model %in% c("Inferno_LogNormal", 
                                            "Inferno_NegBin")])
plot_coverage_stacked(coverage_all[model
                                   %in% c("Inferno_LogNormal", "Inferno_NegBin",
                                          "MoA_LogNormal", "MoA_NegBin",
                                          "EnsembleComplete", "MeanEnsemble", 
                                          "LightGBM", "TSMixer", 
                                          "hhh4", "hhh4_christmas")])
