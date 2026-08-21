# Thesis-Code-and-Data
This repo contains the codes and csv's used to produce forecasts and visualizations for the thesis.
The branch 'code' contains the codes that were used to produce, evaluate and visualize results.
The branch 'CSV-Files' contains the csv-files that were necessary as steps in between or as outputs.
The branch 'Self-Contained-R-Script-Inferno-log-normal' contains a self-contained R-Script that should forecast ARI infections from mid-October onwards using Inferno with general practioner consultations for ARI assumed to be log-normally distributed.

Abstract: 
Every year, as temperatures drop, respiratory illnesses circulate widely, with the risk of severe courses motivating close surveillance and forecasting of these diseases. This thesis modifies and implements two forecasting models, the Method of Analogues (MoA) (Viboud et al. 2003) and Inferno (Osthus 2022), in R to retrospectively generate probabilistic forecasts of general practitioner consultations for acute respiratory infections (ARI) in Germany during the winter of 2024/25, based on RKI data, rescaled by and retrieved from the RESPINOW-Hub. Each model is implemented in a log-normal and a negative binomial variant to handle count data. The resulting forecasts are evaluated using the Weighted Interval Score and compared against multiple other probabilistic forecasting models discussed in Bracher et al. (2026). Inferno produces comparatively accurate forecasts, while MoA performs less well.
