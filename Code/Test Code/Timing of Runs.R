library(DoubleML)
library(mlr3verse)
library(mlr3extralearners)
library(tidyverse)

source("Code/Estimator Functions.R")
source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")
source("Code/Definition Parameter Space.R")

load("Data/Neural.RData")

# Define inputs
vec_ml_g <- c("regr.glmnet", "regr.kknn", "regr.nnet", "regr.ranger", 
              "regr.rpart", "regr.xgboost")

vec_ml_m <- c("classif.glmnet", "classif.kknn", "classif.nnet", "classif.ranger", 
              "classif.rpart", "classif.xgboost")

vec_X_cols <- paste0("X.", 1:30)
vec_D_col <- "D"
vec_Y_col <- "Y"

list_tune_settings <- # Example Estimator evaluation
  list_tune_settings <- list(
    terminator = trm("combo", 
                     list(
                       trm("evals", n_evals = 100), 
                       trm("stagnation", iters = 5)
                     )
    ),
    algorithm = tnr("random_search"),
    rsmp_tune = rsmp("cv", folds = 5),
    measure = list(ml_g = msr("regr.mse"), ml_m = msr("classif.logloss"))
  )

# Check whether estimator works
list_globals = list(
  list_design_points = list_design_points,
  dml_mean = dml_mean,
  calc_err_approx = calc_err_approx,
  msr_validation_set = msr_validation_set
)

# Keep only one dataset for every N
neural <- neural %>% filter(N = NULL, Samples = 1)

mcs_timing <- mcs(dml_estimator, neural)

# Check if dml_estimator works at all
dml_estimator(
  dataset = mcs_timing$dgp$datasets$`Sample = 1 with N = 50`$data, 
  x_cols = vec_X_cols,
  y_col = vec_Y_col,
  d_cols = vec_D_col,
  draw_sample_splitting = FALSE,
  ml_g = vec_ml_g,
  ml_m = vec_ml_m, 
  tune = FALSE,
  par_grids = list_parameterspace,
  list_globals = list_globals
)

dml_estimator(
  dataset = mcs_timing$dgp$datasets$`Sample = 1 with N = 50`$data, 
  x_cols = vec_X_cols,
  y_col = vec_Y_col,
  d_cols = vec_D_col,
  draw_sample_splitting = FALSE,
  ml_g = vec_ml_g,
  ml_m = vec_ml_m, 
  tune = TRUE,
  tune_settings = list_tune_settings,
  par_grids = list_parameterspace,
  list_globals = list_globals
)

system.time(mcs_timing %>% 
              run_simulation(
                seed = 10, 
                parallel = TRUE, workers = 2,
                x_cols = vec_X_cols,
                d_cols = vec_D_col,
                y_col = vec_Y_col,
                ml_g = vec_ml_g,
                ml_m = vec_ml_m,
                tune = FALSE,
                rsmp_key = "repeated_cv",
                rsmp_args = list(folds = 5, repeats = 3),
                par_grids = list_parameterspace,
                list_globals = list_globals
              ))

system.time(mcs_timing %>% 
              run_simulation(
                seed = 10, 
                parallel = FALSE, workers = 3,
                x_cols = vec_X_cols,
                d_cols = vec_D_col,
                y_col = vec_Y_col,
                ml_g = vec_ml_g,
                ml_m = vec_ml_m,
                tune = TRUE,
                rsmp_key = "cv",
                rsmp_args = list(folds = 5),
                par_grids = list_parameterspace, 
                tune_settings = list_tune_settings,
                list_globals = list_globals
              ))
