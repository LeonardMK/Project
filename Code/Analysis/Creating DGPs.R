library(DoubleML)
library(magrittr)
library(mlr3verse)
library(tidyverse)

setwd("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/6. Semester/Masterarbeit/Project/")

source("Code/DGP class.R")
source("Code/DGP functions.R")
source("Code/Definition Parameter Space.R")
source("Code/Estimator Functions.R")
source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# Create DGP objects
set.seed(4)
int_dim <- 30
int_N <- 10000
int_samples <- 1000
vec_beta <- runif(int_dim, -1, 1)
vec_beta_sparse <- vec_beta
vec_beta_sparse[sample(x = c(TRUE, FALSE), size = int_dim, replace = TRUE, prob = c(0.9, 0.1))] <- 0
vec_gamma <- runif(int_dim, -1, 1)
vec_gamma_sparse <- vec_gamma
vec_gamma_sparse[sample(x = c(TRUE, FALSE), size = int_dim, replace = TRUE, prob = c(0.9, 0.1))] <- 0
vec_mu <- rep(0, int_dim)
mat_A <- matrix(runif(int_dim ^ 2, -1, 1), ncol = int_dim)
mat_Sigma <- cov2cor(t(mat_A) %*% mat_A)
solve(mat_Sigma) # Is invertible

# Investigating Signal to Noise Ratio
df_X <- data.frame(MASS::mvrnorm(int_N, vec_mu, mat_Sigma))
cov(df_X)

# Set SD of E equal 1
sd_E <- 1
var_E <- 1
vec_E <- rnorm(int_N, 0, sd_E)

# Signal to noise ratio is supposed to be 4
g_sparse <- dgp_sparse(df_X, vec_E, vec_beta_sparse, "regression", 1.56) - vec_E
var(g_sparse) / var_E
fac_g_sparse <- 1.56

g_sine <- dgp_sine(df_X, vec_E, vec_beta, "regression", 2.85) - vec_E
var(g_sine) / var_E
fac_g_sine <- 2.85

g_poly <- dgp_poly(df_X, vec_E, 2, type = "regression", factor_signal = 0.048) - 
  vec_E
var(g_poly) / var_E
fac_g_poly <- 0.048

g_nnet <- dgp_nnet(X = df_X, E = vec_E, hidden_units = 5, type = "regression", 
                   factor_signal = 0.96) - vec_E
var(g_nnet) / var_E
fac_g_nnet <- 0.96

# Repeat for classification task
# Also want a 50% share of treated
# Shift the mean vector so that the share is nearly 50%
sd_U <- 1
var_U <- 1
vec_U <- rnorm(int_N, sd = sd_U)

# Sparse
m_sparse <- dgp_sparse(df_X, vec_U, vec_gamma_sparse, "classification", 2.7) %>% 
  sigmoid(inverse = TRUE) %>% 
  subtract(vec_U)
var(m_sparse) / var_U

fac_m_sparse <- 2.7
rbinom(int_N, 1, sigmoid(m_sparse + vec_U)) %>% table()

# Sine
m_sine <- dgp_sine(df_X, vec_U, vec_gamma, "classification", 2.9) %>% 
  sigmoid(inverse = TRUE) %>% 
  subtract(vec_U)
var(m_sine) / var_U

fac_m_sine <- 2.9
rbinom(int_N, 1, sigmoid(m_sine + vec_U)) %>% table()

# Polynomial
int_order <- 2
m_poly <- dgp_poly(df_X, vec_U, order = int_order, 
                   type = "classification", factor_signal = 0.048) %>% 
  sigmoid(inverse = TRUE) %>% 
  subtract(vec_U)
var(m_poly) / var_U

fac_m_poly <- 0.048
rbinom(int_N, 1, sigmoid(m_poly + vec_U - 1.1)) %>% table()
scale_m_poly <- -1.1

# Neural Net
int_hidden_units <- 5
dbl_beta_0 <- -3

m_nnet <- dgp_nnet(df_X, vec_U, type = "classification", 
                   beta_0 = dbl_beta_0, factor_signal = 0.96) %>% 
  sigmoid(inverse = TRUE) %>% 
  subtract(vec_U)
var(m_nnet) / var_U

fac_m_nnet <- 0.96
rbinom(int_N, 1, sigmoid(m_nnet + vec_U)) %>% table()

# Define sample sizes
vec_N <- c(50, 100, 400, 800, 1600)

# Sparse Matrix
sparse <- dgp() %>% 
  add_level(
    formulas = Y ~ theta * D + g(X, E, beta), 
    theta = 0.6,
    g = function(X, E, beta) dgp_sparse(X, E, beta, "regression", fac_g_sparse),
    X = MASS::mvrnorm(N, mu = vec_mu, Sigma = mat_Sigma),
    beta = vec_beta_sparse,
    E = rnorm(N, 0, sd_E)
  ) %>% 
  add_level(
    formulas = D ~ m(X, U, gamma),
    m = function(X, U, gamma){
      Prob_D <- dgp_sparse(X, U, gamma, "classification", fac_m_sparse)
      D <- rbinom(nrow(X), 1, Prob_D)
      cbind(D = D, Prob_D = as.vector(Prob_D))
    },
    U = rnorm(N, 0, sd_U),
    gamma = vec_gamma_sparse
  )

# Check whether properties hold
sparse <- sparse %>% 
  run_simulation(N = 10000, seed = 2, samples = 1)

data_sparse <- sparse$datasets$`Sample = 1 with N = 10000`$data
var(data_sparse$Y - data_sparse$E) / var(data_sparse$E)
var(data_sparse$Prob_D %>% sigmoid(inverse = TRUE) - data_sparse$U) / var(data_sparse$U)
data_sparse$D %>% table()

# Create whole dgp set
sparse <- sparse %>% 
  run_simulation(N = vec_N, seed = 2, samples = int_samples)

save(sparse, file = "Data/Sparse.RData")
rm(sparse)

# Sine Function
sine <- dgp() %>% 
  add_level(
    formulas = Y ~ theta * D + g(X, E, beta), 
    theta = 0.6,
    g = function(X, E, beta) dgp_sine(X, E, beta, "regression", fac_g_sine),
    X = MASS::mvrnorm(N, mu = vec_mu, Sigma = mat_Sigma),
    beta = vec_beta,
    E = rnorm(N, 0, sd_E)
  ) %>% 
  add_level(
    formulas = D ~ m(X, U, gamma),
    m = function(X, U, gamma){
      Prob_D <- dgp_sine(X, U, gamma, "classification", fac_m_sine)
      D <- rbinom(nrow(X), 1, Prob_D)
      cbind(D = D, Prob_D = as.vector(Prob_D))
    },
    U = rnorm(N, 0, sd_U),
    gamma = vec_gamma
  )

# Check whether properties hold
sine <- sine %>% 
  run_simulation(N = 10000, seed = 2, samples = 1)

data_sine <- sine$datasets$`Sample = 1 with N = 10000`$data
var(data_sine$Y - data_sine$E) / var(data_sine$E)
var(data_sine$Prob_D %>% sigmoid(inverse = TRUE) - data_sine$U) / var(data_sine$U)
data_sine$D %>% table()

# Create whole dgp set
sine <- sine %>% 
  run_simulation(N = vec_N, seed = 2, samples = int_samples)

save(sine, file = "Data/Sine.RData")
rm(sine)

# Interaction Function
inter <- dgp() %>% 
  add_level(
    formulas = Y ~ theta * D + g(X, E), 
    beta_0 = 1.5,
    theta = 0.6,
    g = function(X, E) {
      dgp_poly(X, E, order = 2, type = "regression", factor_signal = fac_g_poly)
      },
    X = MASS::mvrnorm(N, mu = vec_mu, Sigma = mat_Sigma),
    E = rnorm(N, 0, sd_E)
  ) %>% 
  add_level(
    formulas = D ~ m(X, U),
    m = function(X, U){
      # Need to sclae untransformed values s.t. classes are balanced
      Prob_D <- dgp_poly(X, U, order = 2, type = "regression", 
                         factor_signal = fac_m_poly) %>% 
        subtract(1.1) %>% 
        sigmoid()
      D <- rbinom(nrow(X), 1, Prob_D)
      cbind(D = D, Prob_D = as.vector(Prob_D))
    },
    U = rnorm(N, 0, sd_U),
  )

# Check whether properties hold
inter <- inter %>% 
  run_simulation(N = 10000, seed = 2, samples = 1)

data_inter <- inter$datasets$`Sample = 1 with N = 10000`$data
var(data_inter$Y - data_inter$E) / var(data_inter$E)
var(data_inter$Prob_D %>% sigmoid(inverse = TRUE) - data_inter$U) / var(data_inter$U)
data_inter$D %>% table()

# Create whole dgp set
inter <- inter %>% 
  run_simulation(N = vec_N, seed = 2, samples = int_samples)

save(inter, file = "Data/Inter.RData")
rm(inter)

# Neural Network
neural <- dgp() %>% 
  add_level(
    formulas = Y ~ theta * D + g(X, E), 
    theta = 0.6,
    g = function(X, E){ 
      dgp_nnet(X, E, hidden_units = 5, type = "regression", 
               factor_signal = fac_g_nnet)},
    X = MASS::mvrnorm(N, mu = vec_mu, Sigma = mat_Sigma),
    E = rnorm(N, 0, sd_E)
  ) %>% 
  add_level(
    formulas = D ~ m(X, U),
    m = function(X, U){
      Prob_D <- dgp_nnet(
        X, U, hidden_units = 5, type = "classification",
        beta_0 = dbl_beta_0, factor_signal = fac_m_nnet)
      D <- rbinom(nrow(X), 1, Prob_D)
      cbind(D = D, Prob_D = as.vector(Prob_D))
    },
    U = rnorm(N, 0, sd_U),
  )

# Check whether properties hold
neural <- neural %>% 
  run_simulation(N = 10000, seed = 2, samples = 1)

data_neural <- neural$datasets$`Sample = 1 with N = 10000`$data
var(data_neural$Y - data_neural$E) / var(data_neural$E)
var(data_neural$Prob_D %>% sigmoid(inverse = TRUE) - data_neural$U) / var(data_neural$U)
data_neural$D %>% table()

# Create whole dgp set
neural <- neural %>% 
  run_simulation(N = vec_N, seed = 2, samples = int_samples)

save(neural, file = "Data/neural.RData")
rm(neural)