library(DoubleML)
library(mlr3verse)
library(stargazer)
library(tidyverse)

source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# For every dataset do the following
# Calculate MSE
# Calculate rates for learners and for 
run_analysis <- function(
  mcs_obj, 
  name,
  file = "Motivation",
  parameter_names = "theta", 
  N = NULL, Samples = NULL, by = NULL, na.rm = TRUE, digits = 4, scale = 1){
  
  df_estimates <- mcs_obj$Estimates
  df_measures <- mcs_obj$Measures
  
  # Calculate MSE
  df_mse <- mse(df_estimates, parameter_names, N, Samples, na.rm, by)
  
  list_rates_theta <- estimate_rate(df_mse, TRUE, na.rm)
  
  # Round Data for Output
  list_rates_theta$rate <- list_rates_theta$rate %>% 
    mutate(across(is.numeric, ~ round(.x, digits))) %>% 
    select(-parameter_names)
  list_rates_theta$rate_desc <- list_rates_theta$rate_desc %>% 
    mutate(across(is.numeric, ~ round(.x, digits)))
  
  stargazer(list_rates_theta$rate, type = "latex", summary = FALSE,
            out = paste0("Results/Tables/", file, "/", name, "_mse_rate.tex"))
  stargazer(list_rates_theta$rate_desc, type = "latex", summary = FALSE,
            out = paste0("Results/Tables/", file, "/", name, "_rate.tex"))
  
  ggsave(
    filename = paste0("Results/Plots/", file, "/", name, "_rate_theta.png"), 
    plot = list_rates_theta$plot,
    scale = scale
  )
  
  # Get approximation to nuisance function
  df_ns_dsc <- desc_nuis(df_measures, by = by) %>% 
    mutate(Fun = case_when(
      Fun == "ml_g" ~ "E[Y| X]",
      Fun == "ml_m" ~ "E[D| X]",
      TRUE ~ NA_character_
    ))
  list_rates_ns <- estimate_rate(df_ns_dsc)
  
  list_rates_ns$rate <- list_rates_ns$rate %>% 
    rename(
      "Pred. Error In" = "Mean_msr_in",
      "Pred. Error Val." = "Mean_msr_val",
      "Nuisance Function" = "Fun"
    )
  
  list_rates_ns$rate_desc <- list_rates_ns$rate_desc %>% 
    rename("Nuisance Function" = "Fun")
  
  list_rates_ns$rate <- list_rates_ns$rate %>% 
    ungroup() %>% 
    mutate(
      Bias = case_when(
        str_detect(as.character(Bias), "e-..$") ~ 
        str_remove(as.character(Bias), "(?<=\\..{2}).*(?=e-..)"),
        TRUE ~ as.character(round(Bias, digits))
      ),
      across(is.numeric, ~ round(.x, digits))
    )
  
  list_rates_ns$rate_desc <- list_rates_ns$rate_desc %>% 
    mutate(across(is.numeric, ~ round(.x, digits)))
  
  stargazer(list_rates_ns$rate, type = "latex", summary = FALSE,
            out = paste0("Results/Tables/", file, "/", name, "_mse_rate_nuisance.tex"))
  
  stargazer(list_rates_ns$rate_desc, type = "latex", summary = FALSE,
            out = paste0("Results/Tables/", file, "/", name, "_rate_nuisance.tex"))
  
  ggsave(
    filename = paste0("Results/Plots/", file, "/", name, "_rate_nuisance.png"), 
    plot = list_rates_ns$plot,
    scale = scale
  )
  
  # Get distribution of estimates
  gg_dist <- distribution(
    mcs_obj = df_estimates,
    parameter_names, N, Samples, na.rm = na.rm, by = by
  )
  
  ggsave(
    filename = paste0("Results/Plots/", file, "/", name, "_dist.png"), 
    plot = gg_dist,
    scale = scale
  )
  
  # Consistency of estimates
  list_cons <- consistency(
    mcs_obj = df_estimates,
    parameter_names = parameter_names,
    N = N, Samples = Samples, na.rm = na.rm, by = by
  )
  
  ggsave(
    filename = paste0("Results/Plots/", file, "/", name, "_consistency.png"), 
    plot = list_cons$plot,
    scale = scale
  )
  
  # Normality
  list_as_nor <- asymptotic_normal(
    mcs_obj = df_estimates,
    parameter_names = parameter_names,
    N = N, Samples = Samples, na.rm = na.rm, by = by
  )
  
  ggsave(
    filename = paste0("Results/Plots/", file, "/", name, "_asymp_normal.png"), 
    plot = list_as_nor$plot,
    scale = scale
  )
  
  # Coverage Probability
  list_cov_prob <- cov_prob(
    mcs_obj = df_estimates,
    parameter_names = parameter_names,
    N = N, Samples = Samples, na.rm = na.rm, by = by
  )
  
  ggsave(
    filename = paste0("Results/Plots/", file, "/", name, "_cov_prob.png"), 
    plot = list_cov_prob$plot,
    scale = scale
  )
  
}
