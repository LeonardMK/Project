library(data.table)
library(mlr3)
library(paradox)
library(tidyverse)

setwd("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/6. Semester/Masterarbeit/Project/")

source("Code/Definition Parameter Space.R")
source("Code/DGP functions.R")

# Generate Data
set.seed(3141)

int_N <- 200
int_p <- 5
theta <- 0.6
vec_mean <- rnorm(int_p)
mat_A <- matrix(runif(int_p ^ 2) - 1, nrow = int_p, ncol = int_p)
mat_Sigma <- t(mat_A) %*% mat_A
X <- MASS::mvrnorm(int_N, vec_mean, mat_Sigma)
colnames(X) <- paste0("X", 1:int_p)
U <- rnorm(int_N)
V <- rnorm(int_N)

prob_D <- dgp_nnet(X, V, type = "classification", beta_0 = -2, alpha_0 = -0.5)
vec_D <- rbinom(n = int_N, size = 1, prob = prob_D)
g_0 <- dgp_nnet(X, U, type = "regression")
vec_Y <- theta * vec_D + g_0

df_example <- cbind.data.frame(X, D = vec_D, Prob_D = prob_D, Y = vec_Y, U = U, V = V)
df_example$D <- as.factor(df_example$D)

vec_train_ids <- 1:160
vec_test_ids <- 161:200

# Set feature roles
tsk_classif <- TaskClassif$new(
  id = "classif", 
  backend = df_example, 
  target = "D"
)
tsk_classif$select(c(paste0("X", 1:5)))

# Setting row roles
tsk_classif$set_row_roles(rows = vec_train_ids, roles = "use")
tsk_classif$set_row_roles(rows = vec_test_ids, roles = "validation")

tsk_regr <- TaskRegr$new(
  id = "regression", 
  backend = df_example, 
  target = "Y"
)

tsk_regression$select(c(paste0("X", 1:5)))

# Setting row roles
tsk_regr$set_row_roles(rows = vec_train_ids, roles = "use")
tsk_regr$set_row_roles(rows = vec_test_ids, roles = "validation")

# Creating learners
lrn_classif_nnet <- lrn("classif.nnet")
lrn_regr_nnet <- lrn("regr.nnet")

terminator = trm("stagnation")
tt <- tnr("random_search")

instance_classif = TuningInstanceSingleCrit$new(
  task = tsk_classif,
  learner = lrn_classif_nnet,
  resampling = rsmp("cv"),
  measure = msr("classif.ce"),
  search_space = list_nnet$ml_m,
  terminator = terminator
)

tt$optimize(instance_classif)

instance_regr = TuningInstanceSingleCrit$new(
  task = tsk_regr,
  learner = lrn_regr_nnet,
  resampling = rsmp("cv"),
  measure = msr("regr.mse"),
  search_space = list_nnet$ml_g,
  terminator = terminator
)
tt$optimize(instance_regr)
instance_regr

lrn_classif_nnet$param_set$values <- instance_classif$result_learner_param_vals
lrn_regr_nnet$param_set$values <- instance_regr$result_learner_param_vals

lrn_classif_nnet$train(tsk_classif)
lrn_classif_nnet$predict(tsk_classif, row_ids = vec_test_ids)
lrn_regr_nnet$train(tsk_regr, row_ids = vec_train_ids)
lrn_regr_nnet$predict(tsk_regr, row_ids = vec_test_ids)


# Running Double ML
ml_g = lrn("regr.nnet")
ml_m = lrn("classif.nnet")
obj_dml_data = DoubleMLData$new(
  df_example, 
  y_col = "Y", 
  d_cols = "D", 
  x_cols = colnames(X)
)

n_rep = 1
n_folds = 5
dml_plr_obj = DoubleMLPLR$new(
  data = obj_dml_data, 
  ml_g = ml_g, 
  ml_m = ml_m, 
  n_rep = n_rep, 
  n_folds = n_folds, 
  dml_procedure = "dml2"
)

tune_settings = list(
  terminator = trm("stagnation"),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("repeated_cv", folds = 5, repeats = 3),
  measure = list(
    "ml_g" = msr("regr.mse"),
    "ml_m" = msr("classif.logloss")
  )
)

dml_plr_obj$tune(
  param_set = list_nnet, 
  tune_settings = tune_settings,
  tune_on_folds = TRUE
)

dml_plr_obj$fit(store_predictions = TRUE)

dml_plr_obj$summary()
dml_mean(dml_plr_obj)