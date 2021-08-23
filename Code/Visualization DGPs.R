library(plotly)
library(tidyverse)

setwd("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/6. Semester/Masterarbeit/Project/")
source("Code/DPG functions.R")
source("Code/Utils.R")

# Simulate data
int_N <- 1000
int_p <- 2
vec_beta <- rnorm(int_p, sd = 10)
grid_X <- expand.grid(X1 = seq(-3, 3, by = 0.1), X2 = seq(-3, 3, by = 0.1))

# Sparse
regr_Y_sparse <- dgp_sparse(grid_X, beta = vec_beta, type = "regression")
classif_Y_sparse <- dgp_sparse(grid_X, beta = vec_beta, type = "classification")

mat_regr_Y_sparse <- matrix(regr_Y_sparse, nrow = 61, ncol = 61)
mat_classif_Y_sparse <- matrix(classif_Y_sparse, nrow = 61, ncol = 61)

plot_ly(z = ~mat_regr_Y_sparse) %>% 
  add_surface()

plot_ly(z = ~mat_classif_Y_sparse) %>% 
  add_surface()

# Sine function
regr_Y_sine <- dgp_sine(grid_X, beta = vec_beta, type = "regression")
classif_Y_sine <- dgp_sine(grid_X, beta = vec_beta, type = "classification")

mat_regr_Y_sine <- matrix(regr_Y_sine, nrow = 61, ncol = 61)
mat_classif_Y_sine <- matrix(classif_Y_sine, nrow = 61, ncol = 61)

plot_ly(z = ~mat_regr_Y_sine) %>% 
  add_surface()

plot_ly(z = ~mat_classif_Y_sine) %>% 
  add_surface()

# High order interactions
set.seed(2)
vec_beta_poly <- rnorm(sum(ncol(grid_X) ^ seq(1, 4)))

regr_Y_poly <- dgp_poly(grid_X,
                        order = 4,
                        beta = vec_beta_poly,
                        type = "regression")

classif_Y_poly <- dgp_poly(grid_X,
                           order = 4,
                           beta = vec_beta_poly,
                           type = "classification")

mat_regr_Y_poly <- matrix(regr_Y_poly, nrow = 61, ncol = 61)
mat_classif_Y_poly <- matrix(classif_Y_poly, nrow = 61, ncol = 61)

plot_ly(z = ~mat_regr_Y_poly) %>% 
  add_surface()

plot_ly(z = ~mat_classif_Y_poly) %>% 
  add_surface()


# Neural Net
regr_Y_nnet <- dgp_nnet(grid_X,
                        alpha_0 = runif(5),
                        alpha = matrix(rnorm(2 * 5), 2, 5),
                        beta_0 = 0.2,
                        beta = rnorm(5),
                        type = "regression")

classif_Y_nnet <- dgp_nnet(grid_X,
                           alpha_0 = runif(5),
                           alpha = matrix(rnorm(2 * 5), 2, 5),
                           beta_0 = 0.2,
                           beta = rnorm(5),
                           type = "classification")

mat_regr_Y_nnet <- matrix(regr_Y_nnet, nrow = 61, ncol = 61)
mat_classif_Y_nnet <- matrix(classif_Y_nnet, nrow = 61, ncol = 61)

plot_ly(z = ~mat_regr_Y_nnet) %>% 
  add_surface()

plot_ly(z = ~mat_classif_Y_nnet) %>% 
  add_surface()