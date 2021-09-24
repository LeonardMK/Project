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
vec_ml_g <- c("regr.glmnet", "regr.xgboost", "regr.ranger", "regr.rpart", 
              "regr.kknn", "regr.nnet")
vec_ml_m <- c("classif.glmnet", "classif.xgboost", "classif.ranger", 
              "classif.rpart", "classif.kknn", "classif.nnet")

vec_X_cols <- paste0("X.", 1:30)
vec_D_col <- "D"
vec_Y_col <- "Y"

trm_combo <- trm("combo",
                 list(
                   trm("evals", n_evals = 50),
                   trm("stagnation")
                 )
)

list_tune_settings_rcv <- list(
  terminator = trm_combo,
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("repeated_cv", folds = 5, repeats = 3),
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

load("Data/Sparse.RData")

sparse <- sparse %>% subset(N = c(50, 100, 400), Samples = 1:200)
mcs_sparse <- mcs(dml_estimator, sparse)

# Remove sparse to keep working memory ready
rm(sparse)

mcs_sparse_not <- mcs_sparse %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
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

mcs_sparse_not$dgp$datasets <- NULL
save(mcs_sparse_not, file = paste0(str_path, "/mcs_sparse_not_small.RData"))
rm(mcs_sparse_not)

mcs_sparse_rcv <- mcs_sparse %>% 
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
    par_grids = list_parameterspace,
    tune_settings = list_tune_settings_rcv,
    list_globals = list_globals,
  )

mcs_sparse_rcv$dgp$datasets <- NULL
save(mcs_sparse_rcv, file = paste0(str_path, "/mcs_sparse_rcv_small.RData"))
rm(mcs_sparse_rcv)

rm(mcs_sparse)

# Sparse Analysis ---------------------------------------------------------


# Sine --------------------------------------------------------------------

load("Data/Sine.RData")

sine <- sine %>% subset(N = c(50, 100, 400), Samples = 1:200)

mcs_sine <- mcs(dml_estimator, sine)

# Remove sine to keep working memory ready
rm(sine)

mcs_sine_not <- mcs_sine %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
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

mcs_sine_not$dgp$datasets <- NULL
save(mcs_sine_not, file = paste0(str_path, "/mcs_sine_not_small.RData"))
rm(mcs_sine_not)

mcs_sine_rcv <- mcs_sine %>% 
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
    par_grids = list_parameterspace,
    tune_settings = list_tune_settings_rcv,
    list_globals = list_globals,
  )

mcs_sine_rcv$dgp$datasets <- NULL
save(mcs_sine_rcv, file = paste0(str_path, "/mcs_sine_rcv_small.RData"))
rm(mcs_sine_rcv)

rm(mcs_sine)

# Sine Analysis -----------------------------------------------------------



# Polynomial Interaction --------------------------------------------------

load("Data/Inter.RData")

inter <- inter %>% subset(N = c(50, 100, 400), Samples = 1:200)

mcs_inter <- mcs(dml_estimator, inter)

# Remove inter to keep working memory ready
rm(inter)

mcs_inter_not <- mcs_inter %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
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

mcs_inter_not$dgp$datasets <- NULL
save(mcs_inter_not, file = paste0(str_path, "/mcs_inter_not_small.RData"))
rm(mcs_inter_not)

mcs_inter_rcv <- mcs_inter %>% 
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
    par_grids = list_parameterspace,
    tune_settings = list_tune_settings_rcv,
    list_globals = list_globals,
  )

mcs_inter_rcv$dgp$datasets <- NULL
save(mcs_inter_rcv, file = paste0(str_path, "/mcs_inter_rcv_small.RData"))
rm(mcs_inter_rcv)

rm(mcs_inter)

# Polynomial Interaction Analysis -----------------------------------------


# Neural Network ----------------------------------------------------------

load("Data/Neural.RData")

neural <- neural %>% samples(N = c(50, 100, 400), Samples = 1:200)

mcs_neural <- mcs(dml_estimator, neural)

# Remove neural to keep working memory ready
rm(neural)

mcs_neural_not <- mcs_neural %>% 
  run_simulation(
    seed = 10, 
    workers = int_cores,
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

mcs_neural_not$dgp$datasets <- NULL
save(mcs_neural_not, file = paste0(str_path, "/mcs_neural_not_small.RData"))
rm(mcs_neural_not)

mcs_neural_rcv <- mcs_neural %>% 
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
    par_grids = list_parameterspace,
    tune_settings = list_tune_settings_rcv,
    list_globals = list_globals,
  )

mcs_neural_rcv$dgp$datasets <- NULL
save(mcs_neural_rcv, file = paste0(str_path, "/mcs_neural_rcv_small.RData"))
rm(mcs_neural_rcv)

rm(mcs_neural)

# Neural Network Analysis -------------------------------------------------


