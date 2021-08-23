library(sigmoid)
library(tidyverse)

setwd("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/6. Semester/Masterarbeit/Project/")
source("Code/Utils.R")

# Sparse dgp function
dgp_sparse <- function(X, E = rnorm(nrow(X)), beta = rep(1, ncol(X)), type = c("regression", "classification")){
  
  if (!is.matrix(X)) X <- as.matrix(X)
  
  Y <- X %*% beta + E
  
  if (type == "regression") {
    as.vector(Y)
  } else {
    sigmoid(as.vector(Y))
  }
  
}

# Sine function
dgp_sine <- function(X, E = rnorm(nrow(X)), beta = rep(1, ncol(X)), type = c("regression", "classification")){
  
  if (!is.matrix(X)) X <- as.matrix(X)
  
  Y <- sin(X %*% beta) + E
  
  if (type == "regression") {
    as.vector(Y)
  } else {
    sigmoid(as.vector(Y))
  }
}

# Function with high order moments using kernels
dgp_poly <- function(X, E = rnorm(nrow(X)), 
                     order = 2, 
                     beta = rep(1, (sum(ncol(X) ^ seq(1, order)))),
                     type = c("regression", "classification")){
  
  if (!is.matrix(X)) X <- as.matrix(X)
  
  # Calculate products feature products
  str_formula <- create_interactions(X, order = order)
  
  mat_X <- model.frame(as.formula(paste0("~ ", str_formula)), data = as.data.frame(X))
  Y <- as.matrix(mat_X) %*% beta + E
  
  if (type == "regression") {
    as.vector(Y)
  } else {
    sigmoid(as.vector(Y))
  }
}

# Data generated from a single layer neural network
dgp_nnet <- function(X, E = rnorm(nrow(X)), hidden_units = 5, 
                     alpha_0 = rep(1, hidden_units), 
                     alpha = matrix(rep(1, ncol(X) * hidden_units), nrow = ncol(X), ncol = hidden_units), 
                     beta_0 = 1, beta = rep(1, hidden_units), 
                     type = c("regression", "classification")){
  
  if (!is.matrix(X)) X <- as.matrix(X)
  
  # Calculate features
  mat_Z <- sigmoid(alpha_0 + X %*% alpha)
  Y <- beta_0 + mat_Z %*% beta + E
  
  if (type == "regression") {
    as.vector(Y)
  } else {
    sigmoid(as.vector(Y))
  }
}
