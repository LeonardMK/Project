library(DoubleML)
library(mlr3verse)
library(mlr3extralearners)
library(tidyverse)

source("Code/Definition Parameter Space.R")
source("Code/Estimator Functions.R")
source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# Detect number of cores
int_cores <- parallel::detectCores()
str_path <- "Results/Data/"

# Specify setup
vec_ml_g <- c("regr.xgboost")

vec_ml_m <- c("classif.xgboost")

vec_X_cols <- paste0("X.", 1:30)
vec_D_col <- "D"
vec_Y_col <- "Y"

list_tune_settings <- list(
  terminator = trm("combo", 
                   list(
                     trm("evals", n_evals = 50), 
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

# Sparse ------------------------------------------------------------------

# No sample splitting
load("Data/Sparse.RData")

sparse <- sparse %>% subset(N = c(50, 100, 400), Samples = 1:200)

mcs_sparse <- mcs(dml_estimator, sparse)

# Remove sparse to keep working memory ready
rm(sparse)

mcs_sparse_naive <- mcs_sparse %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "no_cf",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    score = non_orth_score,
    apply_cross_fitting = FALSE
  )

mcs_sparse_naive$dgp$datasets <- NULL
save(mcs_sparse_naive, file = paste0(str_path, "/mcs_sparse_naive_small.RData"))
rm(mcs_sparse_naive)

mcs_sparse_non_orth <- mcs_sparse %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    score = non_orth_score,
  )

mcs_sparse_non_orth$dgp$datasets <- NULL
save(mcs_sparse_non_orth, file = paste0(str_path, "/mcs_sparse_non_orth_small.RData"))
rm(mcs_sparse_non_orth)

mcs_sparse_non_cf <- mcs_sparse %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "no_cf",
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    apply_cross_fitting = FALSE
  )

mcs_sparse_non_cf$dgp$datasets <- NULL
save(mcs_sparse_non_cf, file = paste0(str_path, "/mcs_sparse_non_cf_small.RData"))
rm(mcs_sparse_non_cf)

mcs_sparse_dml <- mcs_sparse %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
  )

mcs_sparse_dml$dgp$datasets <- NULL
save(mcs_sparse_dml, file = paste0(str_path, "/mcs_sparse_dml_small.RData"))
rm(mcs_sparse_dml)

rm(mcs_sparse)

# Sparse Analysis ---------------------------------------------------------


# Sine --------------------------------------------------------------------

load("Data/Sine.RData")

sine <- sine %>% subset(N = c(50, 100, 400), Samples = 1:200)

mcs_sine <- mcs(dml_estimator, sine)

# Remove sine to keep working memory ready
rm(sine)

mcs_sine_naive <- mcs_sine %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "no_cf",
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    score = non_orth_score,
    apply_cross_fitting = FALSE
  )

mcs_sine_naive$dgp$datasets <- NULL
save(mcs_sine_naive, file = paste0(str_path, "/mcs_sine_naive_small.RData"))
rm(mcs_sine_naive)

mcs_sine_non_orth <- mcs_sine %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    score = non_orth_score,
  )

mcs_sine_non_orth$dgp$datasets <- NULL
save(mcs_sine_non_orth, file = paste0(str_path, "/mcs_sine_non_orth_small.RData"))
rm(mcs_sine_non_orth)

mcs_sine_non_cf <- mcs_sine %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "no_cf",
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    apply_cross_fitting = FALSE
  )

mcs_sine_non_cf$dgp$datasets <- NULL
save(mcs_sine_non_cf, file = paste0(str_path, "/mcs_sine_non_cf_small.RData"))
rm(mcs_sine_non_cf)

mcs_sine_dml <- mcs_sine %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
  )

mcs_sine_dml$dgp$datasets <- NULL
save(mcs_sine_dml, file = paste0(str_path, "/mcs_sine_dml_small.RData"))
rm(mcs_sine_dml)

rm(mcs_sine)


# Sine Analysis -----------------------------------------------------------



# Polynomial Interaction --------------------------------------------------

load("Data/Inter.RData")

inter <- inter %>% subset(N = c(50, 100, 400), Samples = 1:200)

mcs_inter <- mcs(dml_estimator, inter)

# Remove inter to keep working memory ready
rm(inter)

mcs_inter_naive <- mcs_inter %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "no_cf",
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    score = non_orth_score,
    apply_cross_fitting = FALSE
  )

mcs_inter_naive$dgp$datasets <- NULL
save(mcs_inter_naive, file = paste0(str_path, "/mcs_inter_naive_small.RData"))
rm(mcs_inter_naive)

mcs_inter_non_orth <- mcs_inter %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    score = non_orth_score,
  )

mcs_inter_non_orth$dgp$datasets <- NULL
save(mcs_inter_non_orth, file = paste0(str_path, "/mcs_inter_non_orth_small.RData"))
rm(mcs_inter_non_orth)

mcs_inter_non_cf <- mcs_inter %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "no_cf",
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    apply_cross_fitting = FALSE
  )

mcs_inter_non_cf$dgp$datasets <- NULL
save(mcs_inter_non_cf, file = paste0(str_path, "/mcs_inter_non_cf_small.RData"))
rm(mcs_inter_non_cf)

mcs_inter_dml <- mcs_inter %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
  )

mcs_inter_dml$dgp$datasets <- NULL
save(mcs_inter_dml, file = paste0(str_path, "/mcs_inter_dml_small.RData"))
rm(mcs_inter_dml)

rm(mcs_inter)


# Polynomial Interaction Analysis -----------------------------------------


# Neural Network ----------------------------------------------------------

load("Data/Neural.RData")

neural <- neural %>% subset(N = c(50, 100, 400), Samples = 1:200)

mcs_neural <- mcs(dml_estimator, neural)

# Remove neural to keep working memory ready
rm(neural)

mcs_neural_naive <- mcs_neural %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "no_cf",
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    score = non_orth_score,
    apply_cross_fitting = FALSE
  )

mcs_neural_naive$dgp$datasets <- NULL
save(mcs_neural_naive, file = paste0(str_path, "/mcs_neural_naive_small.RData"))
rm(mcs_neural_naive)

mcs_neural_non_orth <- mcs_neural %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    score = non_orth_score,
  )

mcs_neural_non_orth$dgp$datasets <- NULL
save(mcs_neural_non_orth, file = paste0(str_path, "/mcs_neural_non_orth_small.RData"))
rm(mcs_neural_non_orth)

mcs_neural_non_cf <- mcs_neural %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "no_cf",
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
    apply_cross_fitting = FALSE
  )

mcs_neural_non_cf$dgp$datasets <- NULL
save(mcs_neural_non_cf, file = paste0(str_path, "/mcs_neural_non_cf_small.RData"))
rm(mcs_neural_non_cf)

mcs_neural_dml <- mcs_neural %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
    x_cols = vec_X_cols,
    d_cols = vec_D_col,
    y_col = vec_Y_col,
    ml_g = vec_ml_g,
    ml_m = vec_ml_m,
    tune = TRUE,
    rsmp_key = "cv",
    rsmp_args = list(folds = 5),
    par_grids = list_parameterspace, tune_settings = list_tune_settings,
    list_globals = list_globals,
  )

mcs_neural_dml$dgp$datasets <- NULL
save(mcs_neural_dml, file = paste0(str_path, "/mcs_neural_dml_small.RData"))
rm(mcs_neural_dml)

rm(mcs_neural)

# Neural Network Analysis -------------------------------------------------


