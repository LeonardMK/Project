library(DoubleML)
library(furrr)
library(magrittr)
library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(mlr3tuning)
library(tidyverse)

set.seed(1)


# Replicate a simple PLR model
int_dim <- 50
int_n <- 500
int_rep <- 1000
dbl_alpha <- 0.05

list_data <- map(seq_len(int_rep), ~ {
  make_plr_CCDDHNR2018(
    n_obs = int_n,
    dim_x = int_dim, 
    alpha = 0.75,
    return_type = "DoubleMLData"
  )
})

# Find a suitable learner
lrn_rf_m <- lrn("regr.rpart")
lrn_rf_g <- lrn("regr.rpart")

# Tune learner
list_grid <- list(
  ml_g = ParamSet$new(
    list()
  )
)

# Implement non-orthogonal scores
source("Code/Utils.R")

plan(multisession, workers = 3)

list_theta <- seq_len(int_rep) %>% 
  future_map(~ {
    # browser()
    # Create DML objects
    dml_non_orth <- DoubleMLPLR$new(
      data = list_data[[.x]], 
      ml_g = lrn_rf_g,
      ml_m = lrn_rf_m,
      n_folds = 2,
      score = non_orth_score,
      apply_cross_fitting = FALSE
    )
    dml_orth <- DoubleMLPLR$new(
      data = list_data[[.x]], 
      ml_g = lrn_rf_g,
      ml_m = lrn_rf_m,
      n_folds = 5,
    )
    dml_orth_no_cf <- DoubleMLPLR$new(
      data = list_data[[.x]], 
      ml_g = lrn_rf_g,
      ml_m = lrn_rf_m,
      n_folds = 1,
      apply_cross_fitting = FALSE
    )
    
  # Fit DML objects
  dml_non_orth$fit()
  dml_orth$fit()
  dml_orth_no_cf$fit()
  
  vec_theta <- c(
    "non_orthogonal" = dml_non_orth$coef,
    "orthogonal" = dml_orth$coef,
    "orthogonal_no_cf" = dml_orth_no_cf$coef
  )
  
  return(vec_theta)
    
  },
  .options = furrr_options(seed = 123),
  .progress = TRUE)

df_theta <- do.call(cbind.data.frame, list_theta) %>% 
  t() %>% 
  set_rownames(seq_len(int_rep)) %>% 
  set_colnames(c("Non Orthogonal", "Orthogonal", "No Sample Splitting")) %>% 
  as.data.frame() %>% 
  pivot_longer(
    cols = 1:3,
    names_to = "Estimator",
    values_to = "Theta hat"
    )

df_theta %>% 
  ggplot(aes(x = `Theta hat`, fill = Estimator)) + 
  geom_histogram(position = "identity", alpha = 0.5) + 
  geom_vline(xintercept = 0.75) + 
  xlab(label = "Estimate of Theta Hat") + 
  ylab("Count") + 
  theme_bw()
