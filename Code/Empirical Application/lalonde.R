library(DoubleML)
library(furrr)
library(Matching)
library(mlr3verse)
library(mlr3extralearners)
library(tidyverse)

source("Code/Utils.R")

# Get data
data("lalonde")

summary(lalonde)

# Define column roles
vec_X_cols <- c("age", "educ", "black", "hisp", "married", "nodegr", "re74", 
                "re75", "u74", "u75")
vec_D_cols <- c("treat")
vec_Y_col <- c("re78")

# Create DML dataset
dml_lalonde <- DoubleMLData$new(
  data = lalonde,
  vec_X_cols,
  vec_Y_col,
  vec_D_cols
)

# Setup learners
vec_ml_g <- c("regr.glmnet", "regr.xgboost", "regr.ranger", "regr.rpart", "regr.kknn")
vec_ml_m <- c("classif.glmnet", "classif.xgboost", "classif.ranger", "classif.rpart", "classif.kknn")
df_grid_lrns <- cbind.data.frame(ml_g = vec_ml_g, ml_m = vec_ml_m)

# Setup Tuning specs
list_tune_settings <- list(
  terminator = trm("stagnation"),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("cv", folds = 5),
  measure = list(ml_g = msr("regr.mse"), ml_m = msr("classif.logloss"))
)

# Load Parameterspaces
source("Code/Definition Parameter Space.R")

# Adapt mtry of random forests
list_parameterspace$ranger$ml_g$params$mtry$upper <- length(vec_X_cols)
list_parameterspace$ranger$ml_m$params$mtry$upper <- length(vec_X_cols)

# Want to get the same partitioning for all estimators.
int_folds <- 5
int_repeats <- 3
lalonde <- lalonde %>% mutate(treat = as.factor(treat))

# Create a new task
set.seed(4)
new_task <- TaskRegr$new(id = "help task", backend = lalonde, target = vec_Y_col)
new_task$set_col_roles(cols = vec_D_cols, roles = "stratum")
new_rsmp <- exec(rsmp, .key = "repeated_cv", folds = int_folds, 
                 repeats = int_repeats)
new_rsmp$instantiate(new_task)

list_samples <- map(1:int_repeats, function(repeats){
  
  vec_folds <- seq((int_folds) * (repeats - 1) + 1, int_folds * repeats)
  
  list_train <- map(vec_folds, ~ new_rsmp$train_set(.x))
  list_test <- map(vec_folds, ~ new_rsmp$test_set(.x))
  
  list(train_ids = list_train, test_ids = list_test)
  
})

# Partially Linear Model
plan("multisession", workers = 3)

list_plr <- df_grid_lrns %>% 
  future_pmap(function(ml_g, ml_m){
    
    lrn_g <- lrn(ml_g)
    lrn_m <- lrn(ml_m)
    
    # Get paramset in list form
    str_ml_g <- str_remove(ml_g, "^.*\\.")
    str_ml_m <- str_remove(ml_m, "^.*\\.")
    
    list_param_set <- list(
      ml_g = list_parameterspace[[str_ml_g]]$ml_g,
      ml_m = list_parameterspace[[str_ml_m]]$ml_m
    )
    
    dml_plr <- DoubleMLPLR$new(
      data = dml_lalonde,
      ml_g = lrn_g,
      ml_m = lrn_m,
      draw_sample_splitting = FALSE
    )
    
    dml_plr$set_sample_splitting(list_samples)
    
    dml_plr$tune(
      param_set = list_param_set,
      tune_settings = list_tune_settings,
      tune_on_folds = TRUE
    )
    
    dml_plr$fit(store_predictions = TRUE)
    
    # Extract coefficient
    df_results <- cbind.data.frame(dml_plr$coef, dml_plr$se, DF = NA, dml_plr$pval)
    colnames(df_results) <- c("parameter_est", "sd", "df", "p_value")
    
    if (dml_plr$n_rep == 1) {
      df_results$type_rep_plr <- "No Repeats"
    } else if (dml_plr$n_rep > 1) {
      vec_results_mean <- dml_mean(dml_plr, na.rm = TRUE)
      df_results <- rbind(df_results, vec_results_mean)
      df_results$type_rep_est <- c("median", "mean")
    }
    
    dbl_ml_g_acc <- msr_validation_set(
      msr = list_tune_settings$measure$ml_g,
      truth = lalonde[, vec_Y_col],
      response = dml_plr$predictions$ml_g
      )
    
    dbl_ml_m_acc <- msr_validation_set(
      msr = list_tune_settings$measure$ml_m,
      truth = lalonde[, vec_D_cols],
      response = dml_plr$predictions$ml_m
      )
    
    list(
      Estimate = df_results,
      Accuracy = c(ml_g = dbl_ml_g_acc, ml_m = dbl_ml_m_acc)
    )
    
  }, 
  .options = furrr_options(
    globals = TRUE,
    packages = sessionInfo() %>% pluck("otherPkgs") %>% names(), 
    seed = 2
  ),
  .progress = TRUE)

list_irm <- df_grid_lrns %>% 
  future_pmap(function(ml_g, ml_m){
    
    lrn_g <- lrn(ml_g)
    lrn_m <- lrn(ml_m)
    
    # Get paramset in list form
    str_ml_g <- str_remove(ml_g, "^.*\\.")
    str_ml_m <- str_remove(ml_m, "^.*\\.")
    
    list_param_set <- list(
      ml_g = list_parameterspace[[str_ml_g]]$ml_g,
      ml_m = list_parameterspace[[str_ml_m]]$ml_m
    )
    
    dml_irm <- DoubleMLIRM$new(
      data = dml_lalonde,
      ml_g = lrn_g,
      ml_m = lrn_m,
      draw_sample_splitting = FALSE
    )
    
    dml_irm$set_sample_splitting(list_samples)
    
    dml_irm$tune(
      param_set = list_param_set,
      tune_settings = list_tune_settings,
      tune_on_folds = TRUE
    )
    
    dml_irm$fit(store_predictions = TRUE)
    
    # Extract coefficient
    df_results <- cbind.data.frame(dml_irm$coef, dml_irm$se, DF = NA, dml_irm$pval)
    colnames(df_results) <- c("parameter_est", "sd", "df", "p_value")
    
    if (dml_irm$n_rep == 1) {
      df_results$type_rep_irm <- "No Repeats"
    } else if (dml_irm$n_rep > 1) {
      vec_results_mean <- dml_mean(dml_irm, na.rm = TRUE)
      df_results <- rbind(df_results, vec_results_mean)
      df_results$type_rep_est <- c("median", "mean")
    }
    
    dbl_ml_g_acc <- msr_validation_set(
      msr = list_tune_settings$measure$ml_g,
      truth = lalonde[, vec_Y_col],
      response = dml_irm$predictions$ml_g
    )
    
    dbl_ml_m_acc <- msr_validation_set(
      msr = list_tune_settings$measure$ml_m,
      truth = lalonde[, vec_D_cols],
      response = dml_irm$predictions$ml_m
    )
    
    list(
      Estimate = df_results,
      Accuracy = c(ml_g = dbl_ml_g_acc, ml_m = dbl_ml_m_acc)
    )
    
  }, 
  .options = furrr_options(
    globals = TRUE,
    packages = sessionInfo() %>% pluck("otherPkgs") %>% names(), 
    seed = 2
  ),
  .progress = TRUE)

# Get both models into form
