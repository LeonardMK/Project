library(DoubleML)
library(future)
library(mlr3verse)
library(mlr3extralearners)
library(tidyverse)

source("Code/Definition Parameter Space.R")
source("Code/Estimator Functions.R")
source("Code/Utils.R")

load("Data/Neural.RData")
df_neural <- neural$datasets$`Sample = 1 with N = 400`$data
rm(neural)

# Setting up Parameters
list_tune_settings_rcv <- list(
  terminator = trm("combo", 
                   list(
                     trm("evals", n_evals = 10), 
                     trm("stagnation", iters = 5)
                   )
  ),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("repeated_cv", folds = 5, repeats = 3),
  measure = list(ml_g = msr("regr.mse"), ml_m = msr("classif.logloss"))
)

vec_ml_g <- c("regr.glmnet", "regr.xgboost", "regr.ranger", "regr.rpart", 
              "regr.kknn", "regr.nnet")
vec_ml_m <- c("classif.glmnet", "classif.xgboost", "classif.ranger", 
              "classif.rpart", "classif.kknn", "classif.nnet")

list_globals = list(
  list_design_points = list_design_points,
  dml_mean = dml_mean,
  calc_err_approx = calc_err_approx,
  msr_validation_set = msr_validation_set
)

plan(multisession, workers = parallel::detectCores())

dml_no_cf_no_tune <- dml_estimator(
  dataset = df_neural, x_cols = paste0("X.", 1:30), y_col = "Y", d_cols = "D", 
  ml_g = vec_ml_g, 
  ml_m = vec_ml_m, 
  tune = FALSE, 
  tune_settings = list_tune_settings_rcv,
  par_grids = list_parameterspace, 
  rsmp_key = "no_cf",
  list_globals = list_globals
)

dml_no_cf_tune <- dml_estimator(
  dataset = df_neural, x_cols = paste0("X.", 1:30), y_col = "Y", d_cols = "D", 
  ml_g = vec_ml_g, 
  ml_m = vec_ml_m, 
  tune = TRUE, 
  tune_settings = list_tune_settings_rcv,
  par_grids = list_parameterspace, 
  rsmp_key = "no_cf",
  list_globals = list_globals
)

dml_no_cf_tune$Estimates
dml_no_cf_tune$Measures

dml_no_cf_tune_non_orth <- dml_estimator(
  dataset = df_neural, x_cols = paste0("X.", 1:30), y_col = "Y", d_cols = "D", 
  ml_g = vec_ml_g, 
  ml_m = vec_ml_m, 
  tune = TRUE, 
  tune_settings = list_tune_settings_rcv,
  par_grids = list_parameterspace, 
  rsmp_key = "no_cf",
  list_globals = list_globals,
  score = non_orth_score
)

dml_no_cf_tune_non_orth$Estimates
dml_no_cf_tune_non_orth$Measures

# Normal splitting
dml_cf_cv_tune <- dml_estimator(
  dataset = df_neural, x_cols = paste0("X.", 1:30), y_col = "Y", d_cols = "D", 
  ml_g = vec_ml_g, 
  ml_m = vec_ml_m, 
  tune = TRUE, 
  tune_settings = list_tune_settings_rcv,
  par_grids = list_parameterspace, 
  rsmp_key = "cv", rsmp_args = list(folds = 5),
  list_globals = list_globals
)

dml_cf_cv_tune$Estimates
dml_cf_cv_tune$Measures

# Repeated Cross Fitting
dml_cf_rcv_tune <- dml_estimator(
  dataset = df_neural, x_cols = paste0("X.", 1:30), y_col = "Y", d_cols = "D", 
  ml_g = vec_ml_g, 
  ml_m = vec_ml_m, 
  tune = TRUE, 
  tune_settings = list_tune_settings_rcv,
  par_grids = list_parameterspace, 
  rsmp_key = "repeated_cv", rsmp_args = list(folds = 5, repeats = 5),
  list_globals = list_globals
)

dml_cf_rcv_tune$Estimates
dml_cf_rcv_tune$Measures

# Bootstrapping as a tuning strategy
dml_cf_tune_bt <- dml_estimator(
  dataset = df_neural, x_cols = paste0("X.", 1:30), y_col = "Y", d_cols = "D", 
  ml_g = vec_ml_g, 
  ml_m = vec_ml_m, 
  tune = TRUE, 
  tune_settings = list_tune_settings_bt,
  par_grids = list_parameterspace, 
  rsmp_key = "cv", rsmp_args = list(folds = 5),
  list_globals = list_globals
)

dml_cf_tune_bt$Estimates
dml_cf_tune_bt$Measures
