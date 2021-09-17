library(DoubleML)
library(mlr3)
library(mlr3extralearners)
library(data.table)

source("Code/DGP functions.R")
source("Code/Definition Parameter Space.R")

# Generate Data
set.seed(3141)

int_N <- 100
int_p <- 5
theta <- 0.6
vec_mean <- rnorm(int_p)
mat_A <- matrix(runif(int_p ^ 2) - 1, nrow = int_p, ncol = int_p)
mat_Sigma <- t(mat_A) %*% mat_A
X <- MASS::mvrnorm(int_N, vec_mean, mat_Sigma)
colnames(X) <- paste0("X", 1:int_p)
U <- rnorm(int_N)
V <- rnorm(int_N)

prob_D <- dgp_poly(X, V, type = "classification")
vec_D <- rbinom(n = int_N, size = 1, prob = prob_D)
g_0 <- dgp_poly(X, U, type = "regression")
vec_Y <- theta * vec_D + g_0

df_example <- cbind.data.frame(X, D = vec_D, Prob_D = prob_D, Y = vec_Y, U = U, V = V)
# df_example$D <- as.factor(df_example$D)

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
  rsmp_tune = rsmp("cv", folds = 5),
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

# The deviation from the theoretical function is
msr_validation_set(msr("regr.mse"), df_example$Y, dml_plr_obj$predictions$ml_g)
msr_validation_set(msr("classif.logloss"), as.factor(df_example$D), dml_plr_obj$predictions$ml_m)

# Calculating accuracy to true function
calc_err_approx(df_example$Y - df_example$U, dml_plr_obj$predictions$ml_g)
calc_err_approx(df_example$Prob_D, dml_plr_obj$predictions$ml_m)
