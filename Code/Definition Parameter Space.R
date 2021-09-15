library(data.table)
library(mlr3verse)
library(paradox)
library(tidyverse)

# The grid has to be a list of lists
# At the top level it differs between the algorithms
# Then between ml_g and ml_m

# glmnet has lambda as a hyperparameter
list_glmnet <- list(
  ml_g = ps(alpha = p_dbl(lower = 0, upper = 1)),
  ml_m = ps(alpha = p_dbl(lower = 0, upper = 1))
)

# xgboost has the hyperparameters
# nu: The learning rate 0, 1
# subsample: Subsampling 0, 1
# max_depth: 0, inf Integer
list_xgboost <- list(
  ml_g = ps(
    eta = p_dbl(lower = 0, upper = 0.1),
    subsample = p_dbl(lower = 0.1, upper = 1),
    max_depth = p_int(lower = 1, upper = 7),
    nrounds = p_int(lower = 100, upper = 3000)
  ),
  ml_m = ps(
    eta = p_dbl(lower = 0, upper = 0.1),
    subsample = p_dbl(lower = 0.1, upper = 1),
    max_depth = p_int(lower = 1, upper = 7),
    nrounds = p_int(lower = 100, upper = 3000)
  )
)

list_ranger <- list(
  ml_g = ps(
    mtry = p_int(lower = 1, upper = Inf),
    num.trees = p_int(lower = 1000, upper = 3000)
  ),
  ml_m = ps(
    mtry = p_int(lower = 1, upper = Inf),
    num.trees = p_int(lower = 1000, upper = 3000)
  )
)

# Regression Trees
list_rpart <- list(
  ml_g = ps(
    cp = p_dbl(lower = 1, upper = exp(0.1), logscale = TRUE)
  ),
  ml_m = ps(
    cp = p_dbl(lower = 1, upper = exp(0.1), logscale = TRUE)
  )
)

# Add also kknn as a the most flexible variant
list_kknn <- list(
  ml_g = ps(
    k = p_int(lower = 1, upper = 20),
    distance = p_int(lower = 1, upper = 4),
    kernel = p_fct(levels = c("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal"))
  ),
  ml_m = ps(
    k = p_int(lower = 1, upper = 20),
    distance = p_int(lower = 1, upper = 4),
    kernel = p_fct(levels = c("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal"))
  )
)

# SIngle layer Neural Network
list_nnet <- list(
  ml_g = ps(
    size = p_int(lower = 1, upper = 20),
    decay = p_dbl(lower = 1, upper = exp(0.1), logscale = TRUE),
    maxit = p_int(lower = 500, upper = 10000)
  ),
  ml_m =  ps(
    size = p_int(lower = 1, upper = 20),
    decay = p_dbl(lower = 1, upper = exp(0.1), logscale = TRUE),
    maxit = p_int(lower = 500, upper = 10000)
  )
)

# Create a single list
list_parameterspace <- list(
  glmnet = list_glmnet,
  xgboost = list_xgboost,
  ranger = list_ranger,
  rpart = list_rpart,
  # svm = list_svm,
  kknn = list_kknn,
  nnet = list_nnet
)

# In case no tuning is performed and only default values are used, design points are needed
list_design_points <- list(
  glmnet = data.table(alpha = 1),
  xgboost = data.table(eta = 0.3, max_depth = 6, subsample = 1),
  ranger = data.table(mtry = 1, num.trees = 500),
  rpart = data.table(cp = 0.01),
  # svm = data.table(kernel = "radial", epsilon = 0.1),
  kknn = data.table(k = 7, distance = 2, kernel = "optimal"),
  nnet = data.table(size = 3, decay = 0, maxit = 1000)
)
