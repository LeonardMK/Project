library(DoubleML)
library(mlr3verse)
library(tidyverse)

setwd("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/6. Semester/Masterarbeit/Project/")

source("Code/DGP class.R")
source("Code/DPG functions.R")
source("Code/Definition Parameter Space.R")
source("Code/Estimator Functions.R")
source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# Create DGP objects
set.seed(4)
int_dim <- 20
vec_N <- 10000
int_samples <- 1
vec_beta <- rnorm(int_dim, sd = 10)
vec_beta_sparse <- vec_beta
vec_beta_sparse[sample(x = c(TRUE, FALSE), size = int_dim, replace = TRUE, prob = c(0.9, 0.1))] <- 0
vec_gamma <- rnorm(int_dim, sd = 9)
vec_gamma_sparse <- vec_gamma
vec_gamma_sparse[sample(x = c(TRUE, FALSE), size = int_dim, replace = TRUE, prob = c(0.9, 0.1))] <- 0
vec_mu <- runif(int_dim, min = -10, max = 10)
mat_A <- matrix(runif(int_dim ^ 2) * 2 -1, ncol = int_dim)
mat_Sigma <- t(mat_A) %*% mat_A

# Investigating Signal to Noise Ratio
df_X <- data.frame(MASS::mvrnorm(1000, vec_mu, mat_Sigma))
vec_E <- rnorm(1000, 0, 2)

# SD of E equals 2
# Signal to noise ratio is supposed to be 4
sd_E <- 2
var_E <- 4
g_sparse <- dgp_sparse(df_X, vec_E, vec_beta_sparse, "regression") - vec_E
dbl_factor_sparse <- sqrt(var(g_sparse) / (4 * var_E))
var(g_sparse) / (var_E * dbl_factor_sparse ^ 2)
sd_E_sparse <- sd_E * dbl_factor_sparse

g_sine <- dgp_sine(df_X, vec_E, vec_beta, "regression") - vec_E
dbl_factor_sine <- sqrt(var(g_sine) / (4 * var_E))
var(g_sine) / (var_E * dbl_factor_sine ^ 2)
sd_E_sine <- sd_E * dbl_factor_sine

g_poly <- dgp_poly(df_X, vec_E, 3, type = "regression") - vec_E
dbl_factor_poly <- sqrt(var(g_poly) / (4 * var_E))
var(g_poly) / (var_E * dbl_factor_poly ^ 2)
sd_E_poly <- sd_E * dbl_factor_poly

g_nnet <- dgp_nnet(X = df_X, E = vec_E, hidden_units = 5, type = "regression") - vec_E
dbl_factor_nnet <- sqrt(var(g_nnet) / (4 * var_E))
var(g_nnet) / (var_E * dbl_factor_nnet ^ 2)
sd_E_nnet <- sd_E * dbl_factor_nnet

# Repeat for classification task
# Also want a 50% share of treated
# Shift the mean vector so that the share is nearly 50%
vec_U <- rnorm(1000, sd = 0.1)
var_U <- 0.1 ^ 2
sd_U <- 0.1

# Sparse
m_sparse <- dgp_sparse(df_X, vec_U, vec_gamma_sparse, "classification") - vec_U
dbl_factor_sparse <- sqrt(var(m_sparse) / (4 * var_U))
var(m_sparse) / (var_U * dbl_factor_sparse ^ 2)
sd_U_sparse <- sd_U * dbl_factor_sparse

rbinom(1000, 1, dgp_sparse(5.5 + df_X, sd_U_sparse * vec_U, vec_gamma_sparse, "classification")) %>% table()
dbl_mu_sparse <- 5.5

# Sine
m_sine <- dgp_sine(df_X, vec_U, vec_gamma, "classification") - vec_U
dbl_factor_sine <- sqrt(var(m_sine) / (4 * var_U))
var(m_sine) / (var_U * dbl_factor_sine ^ 2)
sd_U_sine <- sd_U * dbl_factor_sine

rbinom(1000, 1, dgp_sine(df_X, sd_U_sine * vec_U, vec_gamma, "classification")) %>% table()

# Polynomial
m_poly <- dgp_poly(df_X, vec_U, 3, type = "classification") - vec_U
dbl_factor_poly <- sqrt(var(m_poly) / (4 * var_U))
var(m_poly) / (var_U * dbl_factor_poly ^ 2)
sd_U_poly <- sd_U * dbl_factor_poly

rbinom(1000, 1, dgp_poly(1.2 + df_X, sd_U_poly * vec_U, 3, type = "classification")) %>% table()
dbl_mu_poly <- 1.2

# Neural Net
int_hidden_units <- 5
vec_alpha_0 <- rep(0, int_hidden_units)
mat_alpha <- matrix(rnorm(int_dim * int_hidden_units), nrow = int_dim)
dbl_beta_0_nnet <- 0.25
vec_beta_Z <- rnorm(int_hidden_units)

m_nnet <- dgp_nnet(X = df_X, E = vec_U, hidden_units = 5, type = "classification", 
                   alpha_0 = vec_alpha_0, alpha = mat_alpha,
                   beta = vec_beta_Z) - vec_U
dbl_factor_nnet <- sqrt(var(m_nnet) / (4 * var_U))
var(m_nnet) / (var_U * dbl_factor_nnet ^ 2)
sd_U_nnet <- sd_U * dbl_factor_nnet

rbinom(1000, 1, dgp_nnet(df_X, sd_U_nnet * vec_U, 5, type = "classification",
                         alpha_0 = vec_alpha_0, alpha = mat_alpha,
                         beta_0 = 0.25, beta = vec_beta_Z)) %>% table()

# Sparse Matrix
sparse <- dgp() %>% 
  add_level(
    formulas = Y ~ beta_0 + theta * D + g(X, E, beta, "regression"), 
    beta_0 = 1.5,
    theta = 0.6,
    g = dgp_sparse,
    X = MASS::mvrnorm(N, mu = vec_mu + dbl_mu_sparse, Sigma = mat_Sigma),
    beta = vec_beta_sparse,
    E = rnorm(N, 0, sd_E_sparse)
  ) %>% 
  add_level(
    formulas = D ~ m(X, U, gamma),
    m = function(X, U, gamma){
      Prob_D <- dgp_sparse(X, U, gamma, "classification")
      D <- rbinom(nrow(X), 1, Prob_D)
      cbind(D = D, Prob_D = as.vector(Prob_D))
    },
    U = rnorm(N, 0, sd_U_sparse),
    gamma = vec_gamma_sparse
  )

sparse <- sparse %>% 
  run_simulation(N = vec_N, seed = 2, samples = int_samples)

# Sine Function
sine <- dgp() %>% 
  add_level(
    formulas = Y ~ beta_0 + theta * D + g(X, E, beta, "regression"), 
    beta_0 = 1.5,
    theta = 0.6,
    g = dgp_sine,
    X = MASS::mvrnorm(N, mu = vec_mu, Sigma = mat_Sigma),
    beta = vec_beta,
    E = rnorm(N, 0, sd_E_sine)
  ) %>% 
  add_level(
    formulas = D ~ m(X, U, gamma),
    m = function(X, U, gamma){
      Prob_D <- dgp_sine(X, U, gamma, "classification")
      D <- rbinom(nrow(X), 1, Prob_D)
      cbind(D = D, Prob_D = as.vector(Prob_D))
    },
    U = rnorm(N, 0, sd_U_sine),
    gamma = vec_gamma
  )

sine <- sine %>% 
  run_simulation(N = vec_N, seed = 2, samples = int_samples)

# Interaction Function
inter <- dgp() %>% 
  add_level(
    formulas = Y ~ beta_0 + theta * D + g(X, E, beta, "regression"), 
    beta_0 = 1.5,
    theta = 0.6,
    g = function(X, E) dgp_poly(X, E, order = 3, type = "regression"),
    X = MASS::mvrnorm(N, mu = vec_mu + dbl_mu_poly, Sigma = mat_Sigma),
    E = rnorm(N, 0, sd_E_poly)
  ) %>% 
  add_level(
    formulas = D ~ m(X, U),
    m = function(X, U){
      Prob_D <- dgp_poly(X, U, order = 3, type = "classification")
      D <- rbinom(nrow(X), 1, Prob_D)
      cbind(D = D, Prob_D = as.vector(Prob_D))
    },
    U = rnorm(N, 0, sd_U_poly),
  )

inter <-  inter %>% 
  run_simulation(N = vec_N, seed = 2, samples = int_samples)

# Neural Network
neural <- dgp() %>% 
  add_level(
    formulas = Y ~ beta_0 + theta * D + g(X, E, beta, "regression"), 
    beta_0 = 1.5,
    theta = 0.6,
    g = function(X, E) dgp_nnet(X, E, hidden_units = 5, type = "regression"),
    X = MASS::mvrnorm(N, mu = vec_mu, Sigma = mat_Sigma),
    E = rnorm(N, 0, sd_E_nnet)
  ) %>% 
  add_level(
    formulas = D ~ m(X, U, gamma),
    m = function(X, U){
      Prob_D <- dgp_nnet(
        X, U, hidden_units = 5, type = "classification",
        alpha_0 = vec_alpha_0, alpha = mat_alpha,
        beta_0 = dbl_beta_0_nnet, beta = vec_beta_Z)
      D <- rbinom(nrow(X), 1, Prob_D)
      cbind(D = D, Prob_D = as.vector(Prob_D))
    },
    U = rnorm(N, 0, sd_U_nnet),
  )

neural <- neural %>% 
  run_simulation(N = vec_N, seed = 2, samples = int_samples)

save()