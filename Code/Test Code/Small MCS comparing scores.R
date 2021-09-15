library(DoubleML)
library(mlr3verse)
library(tidyverse)

source("Code/DGP class.R")
source("Code/DGP functions.R")
source("Code/Definition Parameter Space.R")
source("Code/Estimator Functions.R")
source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# Simple example to compare scores
load("Data/Sine.RData")

# Keep only N = 100
sine <- sine %>% subset(N = 100)

list_tune_settings <- list(
  terminator = trm("combo", 
                   list(
                     trm("evals", n_evals = 30), 
                     trm("stagnation", iters = 5)
                   )
  ),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("cv", folds = 5),
  measure = list(ml_g = msr("regr.mse"), ml_m = msr("classif.logloss"))
)

# Check whether estimator works
dataset <- sine$datasets[[1]]$data
list_globals = list(
  list_design_points = list_design_points,
  dml_mean = dml_mean,
  calc_err_approx = calc_err_approx,
  msr_validation_set = msr_validation_set
)

dml_ranger_no_tuning <- dml_estimator(
  dataset, x_cols = paste0("X.", 1:30), y_col = "Y", d_cols = "D", 
  ml_g = "regr.ranger", ml_m = "classif.ranger",
  list_globals = list_globals, tune = FALSE, 
  par_grids = list(ranger = list_ranger)
  )

mcs_small <- mcs(dml_estimator, sine)

mcs_small_rob <- run_simulation.mcs(
    mcs_obj = mcs_small,
    seed = 2, parallel = TRUE, workers = 4, globals = TRUE, 
    x_cols = paste0("X.", 1:30), y_col = "Y", d_cols = "D",
    ml_g = "regr.ranger",
    ml_m = "classif.ranger",
    draw_sample_splitting = FALSE,
    tune = TRUE,
    tune_settings = list_tune_settings,
    par_grids = list(ranger = list_ranger),
    list_globals = list_globals
  )

mcs_small_iv <- mcs_small %>% 
  run_simulation(
    seed = 2, parallel = FALSE,
    x_cols = c("X.1", "X.2"), y_col = "Y", d_cols = "D",
    ml_g = "regr.ranger",
    ml_m = "classif.ranger",
    score = "IV-type",
    draw_sample_splitting = FALSE,
    tune = FALSE,
    tune_settings = list_tune_settings,
    par_grids = list(ranger = list_ranger), 
  )
