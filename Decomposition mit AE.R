rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/runeg/OneDrive/Dokumente/Karlsruher Institut für Technologie - Studium/Bachelorarbeit/Code")
scores <- read.csv("Scores_Horizon_00plus.csv")

model_order <- c("Inferno Log-Normal", "Inferno Negative Binomial",
                 "MoA Log-Normal", "MoA Negative Binomial",
                 "EnsembleComplete","MeanEnsemble",
                 "LightGBM", "TSMixer", "hhh4", "hhh4_christmas")

scores_long <- scores %>%
  filter(horizon %in% 1:4) %>%
  select(model, horizon, overprediction, dispersion, underprediction) %>%
  pivot_longer(cols = c(overprediction, dispersion, underprediction),
               names_to = "component", values_to = "value") %>%
  mutate(
    model = factor(model, levels = model_order),
    component = factor(component,
                       levels = c("overprediction", "dispersion",
                                  "underprediction"),
                       labels = c("Overprediction", "Spread",
                                  "Underprediction"))
  )

# separater Datensatz nur für AE (median)
scores_ae <- scores %>%
  filter(horizon %in% 1:4) %>%
  select(model, horizon, ae_median) %>%
  mutate(model = factor(model, levels = model_order))

ggplot(scores_long, aes(x = model, y = value)) +
  geom_col(aes(fill = model, alpha = component), color = "black", linewidth = 0.2) +
  geom_point(data = scores_ae,
             aes(x = model, y = ae_median, shape = "AE (point forecast)", color = model),
             inherit.aes = FALSE, size = 2.5) +
  scale_alpha_manual(values = c("Overprediction" = 0.35,
                                "Spread" = 0.65,
                                "Underprediction" = 1),
                     name = "Decomposition of WIS") +
  scale_shape_manual(name = NULL, values = c("AE (point forecast)" = 16)) +
  guides(fill = "none", color = "none") +
  facet_wrap(~ horizon, nrow = 1,
             labeller = labeller(horizon = function(x) paste0("Horizon ", x))) +
  labs(x = NULL, y = "WIS") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    strip.background = element_rect(fill = "grey", color = NA),
    strip.text = element_text(size = 12)
  )
