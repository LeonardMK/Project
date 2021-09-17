library(DoubleML)
library(mlr3verse)
library(tidyverse)

source("Code/Estimator Functions.R")
source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# Specify setup
vec_ml_g <- c("regr.xgboost")

vec_ml_m <- c("classif.xgboost")

vec_X_cols <- paste0("X.", 1:30)
vec_D_col <- "D"
vec_Y_col <- "Y"

trm_combo <- trm("combo",
                 list(
                   trm("evals", n_evals = 100),
                   trm("stagnation")
                 )
)

# Use the method that performed best on the tuning task
list_tune_settings <- list(
  terminator = trm_combo,
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("cv", folds = 5),
  measure = list(ml_g = msr("regr.mse"), ml_m = msr("classif.logloss"))
)

list_globals = list(
  list_design_points = list_design_points,
  dml_mean = dml_mean,
  calc_err_approx = calc_err_approx,
  msr_validation_set = msr_validation_set
)

# For every DGP function 
# Select a small sample and a big.
# Perform cross fitting.
# Vary between 2, 5 and 10 folds.
# Vary repeats between 1 and 5
# Use only one learner.

# Sparse ------------------------------------------------------------------

#
load("Data/Sparse.RData")

sparse <- sparse %>% subset(N = c(50, 800), Samples = 1:200)
mcs_sparse <- mcs(dml_estimator, sparse)

# Remove sparse to keep working memory ready
rm(sparse)

mcs_sparse_5fold <- mcs_sparse %>% 
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
    rsmp_args = list(folds = 2),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )

mcs_sparse_5fold <- mcs_sparse %>% 
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

mcs_sparse_10fold <- mcs_sparse %>% 
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
    rsmp_args = list(folds = 10),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )


# Sparse Analysis ---------------------------------------------------------


# Sine --------------------------------------------------------------------

load("Data/Sine.RData")

sine <- sine %>% subset(N = c(50, 800), Samples = 1:200)

mcs_sine <- mcs(dml_estimator, sine)

# Remove sine to keep working memory ready
rm(sine)

mcs_sine_5fold <- mcs_sine %>% 
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
    rsmp_args = list(folds = 2),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )

mcs_sine_5fold <- mcs_sine %>% 
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

mcs_sine_10fold <- mcs_sine %>% 
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
    rsmp_args = list(folds = 10),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )

# Sine Analysis -----------------------------------------------------------



# Polynomial Interaction --------------------------------------------------

load("Data/Inter.RData")

inter <- inter %>% subset(N = c(50, 800), Samples = 1:200)

mcs_inter <- mcs(dml_estimator, inter)

# Remove inter to keep working memory ready
rm(inter)

mcs_inter_5fold <- mcs_inter %>% 
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
    rsmp_args = list(folds = 2),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )

mcs_inter_5fold <- mcs_inter %>% 
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

mcs_inter_10fold <- mcs_inter %>% 
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
    rsmp_args = list(folds = 10),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )

# Polynomial Interaction Analysis -----------------------------------------


# Neural Network ----------------------------------------------------------

load("Data/Neural.RData")

neural <- neural %>% samples(N = c(50, 800), Samples = 1:200)

mcs_neural <- mcs(dml_estimator, neural)

# Remove neural to keep working memory ready
rm(neural)

mcs_neural_5fold <- mcs_neural %>% 
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
    rsmp_args = list(folds = 2),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )

mcs_neural_5fold <- mcs_neural %>% 
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

mcs_neural_10fold <- mcs_neural %>% 
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
    rsmp_args = list(folds = 10),
    par_grids = list_parameterspace,
    list_globals = list_globals,
  )

# Neural Network Analysis -------------------------------------------------


