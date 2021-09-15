library(DoubleML)
library(mlr3verse)
library(tidyverse)

source("Code/Estimator Functions.R")
source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")

# Specify setup
vec_ml_g <- c("regr.xgboost")

vec_ml_m <- c("classif.xgboost")

vec_X_cols <- paste0("X.", 1:30)
vec_D_col <- "D"
vec_Y_col <- "Y"

list_tune_settings_cv <- list(
  terminator = trm("stagnation"),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("cv", folds = 5),
  measure = list(ml_g = msr("regr.mse"), ml_m = msr("classif.logloss"))
)

list_tune_settings_rcv <- list(
  terminator = trm("stagnation"),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("repeated_cv", folds = 5, repeats = 3),
  measure = list(ml_g = msr("regr.mse"), ml_m = msr("classif.logloss"))
)

list_tune_settings_bt <- list(
  terminator = trm("stagnation"),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("bootstrap", repeats = 30, ),
  measure = list(ml_g = msr("regr.mse"), ml_m = msr("classif.logloss"))
)

# Check whether estimator works
list_globals = list(
  list_design_points = list_design_points,
  dml_mean = dml_mean,
  calc_err_approx = calc_err_approx,
  msr_validation_set = msr_validation_set
)

# For every DGP function differing model selection schemes are used
# 5 Fold CV
# 5 CV with 3 repeats
# Bootstrapping +.632

# Sparse ------------------------------------------------------------------

#
load("Data/Sparse.RData")

mcs_sparse <- mcs(dml_estimator, sparse)

# Remove sparse to keep working memory ready
rm(sparse)

mcs_sparse_dml <- mcs_sparse %>% 
  run_simulation(
    seed = 10, 
    parallel = TRUE,
    workers = 2,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = FALSE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )


# Sparse Analysis ---------------------------------------------------------


# Sine --------------------------------------------------------------------

load("Data/Sine.RData")

mcs_sine <- mcs(dml_estimator, sine)

# Remove sine to keep working memory ready
rm(sine)

mcs_sine_dml <- mcs_sine %>% 
  run_simulation(
    seed = 10, 
    parallel = TRUE,
    workers = 2,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = FALSE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )


# Sine Analysis -----------------------------------------------------------



# Polynomial Interaction --------------------------------------------------

load("Data/Inter.RData")

mcs_inter <- mcs(dml_estimator, inter)

# Remove inter to keep working memory ready
rm(inter)

mcs_inter_dml <- mcs_inter %>% 
  run_simulation(
    seed = 10, 
    parallel = TRUE,
    workers = 2,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = FALSE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )


# Polynomial Interaction Analysis -----------------------------------------


# Neural Network ----------------------------------------------------------

load("Data/Neural.RData")

mcs_neural <- mcs(dml_estimator, neural)

# Remove neural to keep working memory ready
rm(neural)

mcs_neural_dml <- mcs_neural %>% 
  run_simulation(
    seed = 10, 
    parallel = TRUE,
    workers = 2,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = FALSE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )

# Neural Network Analysis -------------------------------------------------


