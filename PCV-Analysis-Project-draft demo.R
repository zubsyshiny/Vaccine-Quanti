# Project: Global PCV Introduction Determinants Analysis
# Author: Xuan Zhang
# Method: Time-Dependent Cox Proportional Hazards Model
# Status: Research Proposal Stage / Draft Script

library(survival)
library(tidyverse)

# 1. DATA STRUCTURING (Logic for Time-Dependent Covariates)
# In my PCV proposal, factors like GNI per capita change over time[cite: 517].
# We use tmerge to create a (start, stop] interval dataset.
prepare_longitudinal_data <- function(baseline_df, time_varying_df) {
  # Baseline: country entry at Year 2000 [cite: 526]
  # Event: National Year of PCV Introduction [cite: 531]
  
  long_data <- tmerge(baseline_df, baseline_df, id=ISO3, 
                      introduction = event(Intro_Year_Clean, status))
  
  # Merging dynamic covariates like annual GNI or DTP3 coverage [cite: 523, 539]
  long_data <- tmerge(long_data, time_varying_df, id=ISO3,
                      gni_per_capita = tdc(year, gni_value))
  
  return(long_data)
}

# 2. MODEL SPECIFICATION
# Evaluating the 'hazard' (likelihood) of vaccine introduction [cite: 516]
run_pcv_cox_model <- function(data) {
  fit <- coxph(Surv(tstart, tstop, introduction) ~ 
                 gni_per_capita + gavi_status + dtp3_coverage + u5mr, 
               data = data)
  
  # Extract Hazard Ratios for socioeconomic determinants [cite: 545]
  return(summary(fit))
}

# 3. DIAGNOSTICS
# Testing Proportional Hazards Assumption via Schoenfeld residuals [cite: 547]
check_assumptions <- function(fit) {
  ph_test <- cox.zph(fit)
  print(ph_test)
  plot(ph_test, main = "Schoenfeld Residuals: PH Assumption Check")
}

# 4.visualization of Results
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. VISUALIZING BY GAVI STATUS [cite: 535, 543]
plot_gavi_trend <- function(gavi_intro_data) {
  plot_intro_data <- gavi_intro_data %>%
    group_by(intro_year, `Gavi Status`) %>%
    summarise(n = n(), .groups = "drop") %>%
    complete(intro_year = 2000:2025, `Gavi Status`, fill = list(n = 0)) %>%
    group_by(`Gavi Status`) %>%
    mutate(cumulative_n = cumsum(n))
  
  ggplot(plot_intro_data, aes(x = intro_year, y = cumulative_n, fill = `Gavi Status`)) +
    geom_area(alpha = 0.8) +
    scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Cumulative PCV Introduction by Gavi Status",
         subtitle = "Evaluating Gavi's impact on neutralizing financial barriers [cite: 543]",
         x = "Year of Introduction", y = "Number of Countries") +
    theme_minimal()
}

# 2. VISUALIZING BY INCOME GROUP [cite: 534, 535]
plot_income_group_trend <- function(eco_class_data) {
  # Logic: Calculating cumulative count per World Bank Income Category
  ggplot(eco_class_data, aes(x = Intro_Year_Clean, y = cumulative_countries, 
                             color = income_2024_original)) +
    geom_line(size = 1.2) +
    geom_point(alpha = 0.5) +
    scale_x_continuous(limits = c(2000, 2025), breaks = seq(2000, 2025, 5)) +
    theme_minimal() +
    labs(title = "Global PCV Introduction: Cumulative Count by Income Group",
         color = "WB Income Group (2024)",
         y = "Cumulative Countries", x = "Year")
}

