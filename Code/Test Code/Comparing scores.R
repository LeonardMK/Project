library(DoubleML)
library(MASS)
library(mlr3verse)
library(plotly)
library(tidyverse)

setwd("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/6. Semester/Masterarbeit/Project/")

source("Code/DGP class.R")
source("Code/DGP functions.R")
source("Code/Definition Parameter Space.R")
source("Code/Estimator Functions.R")
source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# Simple example to compare scores
set.seed(3141)
int_N <- 10000
int_p <- 4
theta <- 0.6
vec_mean <- runif(int_p, -1, 1)
mat_A <- matrix(runif(int_p ^ 2, -1, 1), nrow = int_p, ncol = int_p)
mat_Sigma <- cov2cor(t(mat_A) %*% mat_A)
X <- MASS::mvrnorm(int_N, vec_mean, mat_Sigma)
colnames(X) <- paste0("X", 1:int_p)
U <- rnorm(int_N)
V <- rnorm(int_N)

prob_D <- dgp_nnet(X, V, beta_0 = -2, type = "classification")
vec_D <- rbinom(n = int_N, size = 1, prob = prob_D)
g_0 <- dgp_nnet(X, U, type = "regression")
vec_Y <- theta * vec_D + g_0

df_example <- cbind.data.frame(X, D = vec_D, Prob_D = prob_D, Y = vec_Y, U = U, V = V)

# Plot Data
plot_ly(df_example, x = ~ X1, y = ~X2, z = ~Y, color = ~D) %>% 
  add_markers()

# Create DML object
ml_g = lrn("regr.xgboost")
ml_m = lrn("classif.xgboost")
obj_dml_data = DoubleMLData$new(
  df_example, 
  y_col = "Y", 
  d_cols = "D", 
  x_cols = colnames(X)
)

n_rep = 1
n_folds = 5
dml_plr_rob_obj = DoubleMLPLR$new(
  data = obj_dml_data, 
  ml_g = ml_g, 
  ml_m = ml_m, 
  n_rep = n_rep, 
  n_folds = n_folds, 
  dml_procedure = "dml2", 
  score = "partialling out"
)

dml_plr_iv_obj = DoubleMLPLR$new(
  data = obj_dml_data, 
  ml_g = ml_g, 
  ml_m = ml_m, 
  n_rep = n_rep, 
  n_folds = n_folds, 
  dml_procedure = "dml2",
  score = "IV-type"
)

# Set the same resampling for both
dml_plr_iv_obj$set_sample_splitting(dml_plr_rob_obj$smpls)

# Tune models
tune_settings <- list(
  terminator = trm("stagnation"),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("cv", folds = 5),
  measure = list(
    "ml_g" = msr("regr.mse"),
    "ml_m" = msr("classif.logloss")
  )
)
dml_plr_rob_obj$tune(
  param_set = list_xgboost,
  tune_settings = tune_settings,
  tune_on_folds = TRUE
)

dml_plr_iv_obj$tune(
  param_set = list_glmnet,
  tune_settings = tune_settings,
  tune_on_folds = TRUE
)

# Fit using Robinson
dml_plr_rob_obj$fit(store_predictions = TRUE)
dml_plr_rob_obj$summary()

# Fit using IV-type
dml_plr_iv_obj$fit(store_predictions = TRUE)
dml_plr_iv_obj$summary()

g_hat_rob <- dml_plr_rob_obj$predictions$ml_g %>% head()
g_hat_iv <- dml_plr_iv_obj$predictions$ml_g %>% head()

# The predicted values are identical
all(g_hat_iv == g_hat_rob)

# If IV-score is fit without refitting the model
# I can recalculate it from the robinson model
v_hat_rob <- df_example$D - dml_plr_rob_obj$predictions$ml_m
u_hat_rob <- df_example$Y - dml_plr_rob_obj$predictions$ml_g
v_hat_iv <- df_example$D - dml_plr_iv_obj$predictions$ml_m
u_hat_iv <- df_example$Y - dml_plr_iv_obj$predictions$ml_g
v_hatd_rob = v_hat_rob * df_example$D
v_hatd_iv = v_hat_iv * df_example$D
psi_a_rob <- -v_hatd_rob
psi_b_rob <- v_hat_rob * u_hat_rob
psi_a_iv <- -v_hatd_rob
psi_b_iv <- v_hat_iv * u_hat_iv

# Get into dataframe form
df_rob_iv <- cbind.data.frame(
  u_hat_rob,
  u_hat_iv,
  v_hat_rob,
  v_hat_iv,
  v_hatd_rob,
  v_hatd_iv
)

# Estimate Theta from Robinson
-mean(psi_b_rob) / mean(psi_a_rob)
-mean(psi_b_iv) / mean(psi_a_iv)
# Estimated parameters are identical

# Creating a new plr model with IV score will give the correct result
df_example$nuis_Y <- df_example$Y - dml_plr_rob_obj$coef * df_example$D
obj_dml_data_plug_in <- DoubleMLData$new(
  data = df_example, 
  x_cols = paste0("X", 1:2), 
  y_col = "nuis_Y",
  d_cols = "D"
)
dml_plr_iv_plug_in_obj <- DoubleMLPLR$new(
  data = obj_dml_data_plug_in, 
  ml_g = ml_g, 
  ml_m = ml_m, 
  n_rep = n_rep, 
  n_folds = n_folds, 
  dml_procedure = "dml2",
  score = "IV-type" 
)

dml_plr_iv_plug_in_obj$tune(
  param_set = list_xgboost,
  tune_settings = tune_settings,
  tune_on_folds = TRUE
)
dml_plr_iv_plug_in_obj$fit(store_predictions = TRUE)
dml_plr_iv_plug_in_obj$summary()
