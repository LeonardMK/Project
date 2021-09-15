library(DoubleML)
library(mlr3verse)
library(tidyverse)

source("Code/DGP class.R")
source("Code/Definition Parameter Space.R")
source("Code/Estimator Functions.R")
source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# Example dgp object
set.seed(4)

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
    gamma = c(-0.8, -1)
  )

test %>% get_formulas()
test %>% get_arguments()
# test <- test %>% set_arguments(gamma = runif(10, -2, 2))

test <- test %>% run_simulation(N = c(50, 100), seed = 2, samples = 30)

# Example Estimator evaluation
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

vec_ml_g <- c("regr.glmnet", "regr.xgboost", "regr.ranger", "regr.rpart", "regr.kknn")
vec_ml_m <- c("classif.glmnet", "classif.xgboost", "classif.ranger", "classif.rpart", "classif.kknn")

test_dataset <- test$datasets$`Sample = 10 with N = 100`$data
x <- dml_estimator(dataset = test_dataset, x_cols = paste0("X.", 1:2), y_col = "Y", d_cols = "D", 
              ml_g = vec_ml_g, 
              ml_m = vec_ml_m, 
              draw_sample_splitting = FALSE, tune = FALSE, 
              tune_settings = list_tune_settings,
              par_grids = list_parameterspace
)

# Create a mcs object
mcs_test <- mcs(dml_estimator, test)

print(mcs_test)

# Run on a single core first
mcs_test <- run_simulation(
  mcs_test, seed = 2, parallel = TRUE, workers = 2,
  x_cols = paste0("X.", 1:2), y_col = "Y", d_cols = "D", 
  ml_g = vec_ml_g, 
  ml_m = vec_ml_m, 
  draw_sample_splitting = FALSE, tune = FALSE, 
  tune_settings = list_tune_settings,
  par_grids = list_parameterspace, 
  globals = TRUE
)

df_estimates <- get_estimates(mcs_test)

df_mse <- mse(mcs_test, by = "algorithms")
mse(mcs_test, N = 100, Samples = 1:10, na.rm = TRUE, by = "algorithms")

consistency(mcs_test, by = "algorithms")
asymptotic_normal(mcs_test, by = "algorithms")
distribution(mcs_test, by = "algorithms")
cov_prob(mcs_test, by = "algorithms")
