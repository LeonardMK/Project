library(DoubleML)
library(mlr3verse)
library(paradox)
library(tidyverse)

setwd("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/6. Semester/Masterarbeit/Project")
source("Code/DGP class.R")
source("Code/Estimator Functions.R")

# Coefficients that are supposed to remain constant have to be set beforehand or must be provided
vec_beta <- rnorm(2, sd = 10)
vec_mu <- runif(2, min = -10, max = 10)
mat_Sigma <- diag(runif(2, min = 1, max = 10))

test <- dgp() %>% 
  add_level(
    formulas = Y ~ beta_0 + theta * D + g(X, beta) + E, 
    beta_0 = 1.5,
    theta = 0.6,
    g = function(X, beta){
      X <- as.matrix(X)
      Y_X <- (
        sin(X %*% beta) * 
          sqrt(
            abs(1 / sum(rep(c(0, 1), length.out = ncol(X))))
          )
      )
      
      as.vector(Y_X)
    },
    X = MASS::mvrnorm(N, mu = vec_mu, Sigma = mat_Sigma),
    beta = vec_beta,
    E = rnorm(N, 0, 2)
  ) %>% 
  add_level(
    formulas = D ~ m(X, gamma, U),
    m = function(X, gamma, U){
      X <- as.matrix(X)
      pi <- sigmoid::logistic(X %*% gamma + U)
      d <- rbinom(nrow(X), 1, pi)
      cbind(D = d, Prob_D = as.vector(pi))
    },
    U = rnorm(N, 0, 0.5),
    gamma = c(0.2, -0.2)
  )

test %>% get_formulas()
test %>% get_arguments()
# test <- test %>% set_arguments(gamma = runif(10, -2, 2))

test <- test %>% run_simulation(N = c(100, 250), seed = 2, samples = 100)

# Set some external parameters
x_cols <-  c("X.1", "X.2")
y_col <- "Y"
d_cols <-  "D"

str_rsmpl_key <- "repeated_cv"
int_folds <- 5
int_repeats <- 3

str_lrn_m <- "classif.xgboost"
str_lrn_g <- "regr.xgboost"

dataset <- test$datasets$`Sample = 1 with N = 100`$data
dml_dataset <- DoubleMLData$new(
  data = dataset,
  x_cols,
  y_col,
  d_cols
)

plr_obj <- DoubleMLPLR$new(
  data = dml_dataset, 
  ml_g = "regr.xgboost",
  ml_m = "classif.xgboost", 
  draw_sample_splitting = FALSE
  )

# MLR takes care of everything for us.
# Define a new classification task with treatment as the dependent variable.
# Define the dependent variable as a strata
# Then extract the sampling ids
# Overwrite the samplings from the DML task
new_task <- Task$new(id = "help task", task_type = "classif")
new_task$set_col_roles(cols = "D", roles = "target")
new_task$set_col_roles(cols = "D", roles = "stratum")
new_rsmp <- rsmp(str_rsmpl_key, folds = int_fols)
new_rsmp$instantiate(new_task)
list_train <- map(1:new_rsmp$iters, ~ new_rsmp$train_set(.x))
list_test <- map(1:new_rsmp$iters, ~ new_rsmp$test_set(.x))

list_samples <- list(list(train_ids = list_train, test_ids = list_test))
plr_obj$set_sample_splitting(list_samples)
plr_obj$fit()
plr_obj$summary()

# Now implement tuninng.

plr_obj$tune(param_set = par_grids, tune_settings = tune_settings, tune_on_folds = TRUE)
plr_obj$params
plr_obj$fit()
plr_obj$summary()

# Not possible to stratify at this stage. 
# Set params on every prespecified fold.
# Now the task is the same as for every split

# Create search_space
ss_m <- ps(
  
)

ss_g

# Measure

# Terminator

#

map(list_train, function(index_train){
  
  data_train <- dataset[index_train]
  
  # Create a classification task with D as the stratum variable
  tsk_m <- Task$new(id = "m_X", task_type = "classif", backend = data_train)
  tsk_g <- Task$new(id = "g_X", task_type = "regr", backend = data_train)
  
  # Set col roles
  tsk_m$set_col_roles(cols = d_cols, "target")
  tsk_m$set_col_roles(cols = d_cols, "stratum")
  tsk_m$set_col_roles(cols = x_cols, "feature")

  tsk_g$set_col_roles(cols = y_col, "target")
  tsk_g$set_col_roles(cols = d_cols, "stratum")
  tsk_g$set_col_roles(cols = x_cols, "feature")
  
  # Now get a sample split
  rsmp_obj <- rsmp(str_rsmpl_key, int_folds, int_repeats)
  rsmp_obj$instantiate(tsk_m)
  
  # Tune the object
  
})

# The folds should be prespecified since this would allow for more comparability
get_sample_splits <- function(mcs_obj, N = NULL, Samples = NULL, rsmp_key = "cv", ...){
  
  # For every 
  
}