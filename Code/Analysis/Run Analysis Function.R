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
  mcs_obj, parameter_names = "theta", N = NULL, Samples = NULL, by = NULL,
  na.rm = TRUE, scale = 1)

str_name <- "eval(expr_name)"
str_path <- "Results/"
str_file <- paste0(str_path, "Data/", str_name, ".RData")
expr_name <- parse(text = str_name)

# Load RData
load(str_file)

# Calculate MSE
df_mse <- mse(eval(expr_name), parameter_names, N, Samples, by, na.rm)

stargazer(df_mse, type = "latex", 
          out = paste0("Results/Tables/", str_name, "_mse.RData"))

list_rates_theta <- estimate_rate(df_mse)
ggsave(
  filename = paste0("Results/Plots/", str_name, "_rate_theta.png"), 
  plot = list_rates_theta$plot,
  scale = scale
  )

# Get approximation to nuisance function
df_ns_dsc <- desc_nuis(eval(expr_name))
list_rates_ns <- estimate_rate(df_ns_dsc)
ggsave(
  filename = paste0("Results/Plots/", str_name, "_rate_nuisance.png"), 
  plot = list_rates_ns$plot,
  scale = scale
)

# Get distribution of estimates
list_dist <- distribution(eval(expr_name))
ggsave(
  filename = paste0("Results/Plots/", str_name, "_dist.png"), 
  plot = list_dist$plot,
  scale = scale
)

# Consistency of estimates
list_cons <- consistency(eval(expr_name))

# Normality
list_as_nor <- asymptotic_normal(eval(expr_name))

