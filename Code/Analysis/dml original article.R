library(DoubleML)
library(future)
library(haven)
library(mlr3extralearners)
library(mlr3verse)
library(tidyverse)

source("Code/Definition Parameter Space.R")
source("Code/Utils.R")

str_y <- "net_tfa"
str_d <- "e401"
vec_x <- c("age", "inc", "educ", "fsize", "marr", "twoearn", "db", "pira", "hown")

df_e401k <- read_dta("DMLonGitHub/sipp1991.dta") %>% as.data.frame()
dml_df <- DoubleMLData$new(
  data = df_e401k,
  x_cols = vec_x,
  y_col = str_y,
  d_cols = str_d
)

vec_ml_g <- c("regr.xgboost", "regr.ranger", "regr.nnet")
vec_ml_m <- c("classif.xgboost", "classif.ranger", "classif.nnet")

# Need to limit number of parameters selected by random forest
list_parameterspace$ranger$ml_g$params$mtry$upper <- length(vec_x)
list_parameterspace$ranger$ml_m$params$mtry$upper <- length(vec_x)

# Same sample splitting for every learner
int_dml_folds <- 5
int_dml_repeats <- 3

new_task <- TaskRegr$new(id = "help task", backend = df_e401k, target = str_y)
new_task$set_col_roles(cols = str_d, roles = "stratum")
new_rsmp <- rsmp("repeated_cv", folds = int_dml_folds, repeats = int_dml_repeats)
new_rsmp$instantiate(new_task)

vec_repeats <- seq(1, int_dml_repeats)

list_samples <- map(vec_repeats, function(repeats){
  
  vec_folds <- seq((int_dml_folds) * (repeats - 1) + 1, 
                   int_dml_folds * repeats)
  
  list_train <- map(vec_folds, ~ new_rsmp$train_set(.x))
  list_test <- map(vec_folds, ~ new_rsmp$test_set(.x))
  
  list(train_ids = list_train, test_ids = list_test)
  
})

# Setup for tuning
int_tune_folds <- 5
int_tune_repeats <- 5

list_tune_settings <- list(
  terminator = trm("combo", 
                   list(
                     trm("evals", n_evals = 50), 
                     trm("stagnation", iters = 5)
                   )
  ),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp(
    .key = "repeated_cv", 
    folds = int_tune_folds, 
    repeats = int_tune_repeats
  ),
  measure = list(
    ml_g = msr("regr.mse"), 
    ml_m = msr("classif.logloss"))
)

# Start model fitting
df_grid_lrns <- cbind.data.frame(ml_g = vec_ml_g, ml_m = vec_ml_m)

int_cores <- parallel::detectCores()
plan(multisession, workers = int_cores)

pbar <- progress::progress_bar$new(
  format = "Calculating [:bar] :percent eta: :eta",
  total = nrow(df_grid_lrns),
  clear = FALSE,
)

list_results <- pmap(df_grid_lrns, function(ml_g, ml_m){
  
  print(paste0("Currently at ml_g: ", ml_g, " and ml_m: ", ml_m, "."))
  
  lrn_g <- lrn(ml_g)
  lrn_m <- lrn(ml_m)
  
  ind_g <- str_which(names(list_parameterspace), str_remove(ml_g, "^.*\\."))
  ind_m <- str_which(names(list_parameterspace), str_remove(ml_m, "^.*\\."))
  
  list_par_grids <- list(
    ml_g = list_parameterspace[[ind_g]]$ml_g, 
    ml_m = list_parameterspace[[ind_m]]$ml_m
  )
  
  dml_plr <- DoubleMLPLR$new(
    data = dml_df,
    ml_g = lrn_g,
    ml_m = lrn_m,
    draw_sample_splitting = FALSE
  )
  
  dml_plr$set_sample_splitting(list_samples)
  
  ddpcr::quiet({
    dml_plr$tune(
      param_set = list_par_grids,
      tune_settings = list_tune_settings,
      tune_on_folds = TRUE
    )
    
    dml_plr$fit(store_predictions = TRUE)
  })
  
  dml_irm <- DoubleMLIRM$new(
    data = dml_df,
    ml_g = ml_g,
    ml_m = ml_m,
    draw_sample_splitting = FALSE
  )
  
  dml_irm$set_sample_splitting(list_samples)
  
  ddpcr::quiet({
    dml_irm$tune(
      param_set = list_par_grids,
      tune_settings = list_tune_settings,
      tune_on_folds = TRUE
    )
    
    dml_irm$fit(store_predictions = TRUE)
  })
  
  msr_g_val_set_plr <- msr_validation_set(
    msr = list_tune_settings$measure$ml_g,
    truth = df_e401k[, str_y],
    response = dml_plr$predictions$ml_g
  )
  
  msr_m_val_set_plr <- msr_validation_set(
    msr = list_tune_settings$measure$ml_m,
    truth = as.factor(df_e401k[, str_d]),
    response = dml_plr$predictions$ml_m
  )
  
  arr_prediction_0 <- dml_irm$predictions$ml_g0
  arr_prediction_1 <- dml_irm$predictions$ml_g1
  for(i in 1:int_dml_repeats){
    arr_prediction_0[df_e401k[str_d] == 1, i, ] <- arr_prediction_1[df_e401k[str_d] == 1, i, ]
  }
  
  msr_g_val_set_irm <- msr_validation_set(
    msr = list_tune_settings$measure$ml_g,
    truth = df_e401k[, str_y],
    response = arr_prediction_0
  )
  
  msr_m_val_set_irm <- msr_validation_set(
    msr = list_tune_settings$measure$ml_m,
    truth = as.factor(df_e401k[, str_d]),
    response = dml_irm$predictions$ml_m
  )
  
  # Extract Results
  df_results <- rbind.data.frame(
    c(dml_plr$coef, dml_plr$se, dml_plr$pval, msr_g_val_set_plr, 
      msr_m_val_set_plr, "plr", ml_g, ml_m),
    c(dml_irm$coef, dml_irm$se, dml_irm$pval, msr_g_val_set_irm, 
      msr_m_val_set_irm, "irm", ml_g, ml_m)
  )
  
  print(df_results)
  
  pbar$tick()
  
  df_results
  
})

# Aggregate results
df_results <- list_results %>% 
  map_df(~ {
    colnames(.x) <- c("coef", "se", "pval", "msr_g_val", "msr_m_val", "model", 
                      "ml_g", "ml_m")
    .x
  })

# Refit with best performing Models.
lrn_plr_g <- df_results %>% 
  filter(model == "plr") %>% 
  filter(msr_g_val == min(msr_g_val)) %>% 
  pull(ml_g) %>% 
  str_remove("^regr\\.")

lrn_plr_m <- df_results %>% 
  filter(model == "plr") %>% 
  filter(msr_m_val == min(msr_m_val)) %>% 
  pull(ml_m) %>% 
  str_remove("^classif\\.")

lrn_irm_g <- df_results %>% 
  filter(model == "irm") %>% 
  filter(msr_g_val == min(msr_g_val)) %>% 
  pull(ml_g) %>% 
  str_remove("^regr\\.")

lrn_irm_m <- df_results %>% 
  filter(model == "irm") %>% 
  filter(msr_m_val == min(msr_m_val)) %>% 
  pull(ml_m) %>% 
  str_remove("^classif\\.")

list_par_grids_plr <- list(
  ml_g = list_parameterspace[[lrn_plr_g]]$ml_g, 
  ml_m = list_parameterspace[[lrn_plr_m]]$ml_m
)

list_par_grids_irm <- list(
  ml_g = list_parameterspace[[lrn_irm_g]]$ml_g, 
  ml_m = list_parameterspace[[lrn_irm_m]]$ml_m
)

# Refit models
dml_plr <- DoubleMLPLR$new(
  data = dml_df,
  ml_g = lrn(paste0("regr.", lrn_plr_g)),
  ml_m = lrn(paste0("classif.", lrn_plr_m)),
  draw_sample_splitting = FALSE
)

dml_plr$set_sample_splitting(list_samples)

plan(multisession, workers = int_cores)

ddpcr::quiet({
  dml_plr$tune(
    param_set = list_par_grids_plr,
    tune_settings = list_tune_settings,
    tune_on_folds = TRUE
  )
  
  dml_plr$fit(store_predictions = TRUE)
})

dml_irm <- DoubleMLIRM$new(
  data = dml_df,
  ml_g = lrn(paste0("regr.", lrn_irm_g)),
  ml_m = lrn(paste0("classif.", lrn_irm_m)),
  draw_sample_splitting = FALSE
)

dml_irm$set_sample_splitting(list_samples)

plan(multisession, workers = int_cores)

ddpcr::quiet({
  dml_irm$tune(
    param_set = list_par_grids_irm,
    tune_settings = list_tune_settings,
    tune_on_folds = TRUE
  )
  
  dml_irm$fit(store_predictions = TRUE)
})

df_results_final <- df_results %>% 
  rbind(
    c(dml_plr$coef, dml_plr$se, dml_plr$pval, NA, NA, "plr", 
      dml_plr$learner$ml_g$id, dml_plr$learner$ml_m$id),
    c(dml_irm$coef, dml_irm$se, dml_irm$pval, NA, NA, "irm", 
      dml_irm$learner$ml_g$id, dml_irm$learner$ml_m$id)
  )

save(df_results_final, file = "Results/Data/e401k.RData")

vec_mle_replace <- c("SGBM", "RF", "N-Net")
names(vec_mle_replace) <- c("xgboost", "ranger", "nnet")

df_results_final %>% 
  mutate(
    across(1:5, as.numeric),
    ml_g = str_remove(ml_g, "^regr\\."),
    ml_m = str_remove(ml_m, "^classif\\."),
    Algorithms = case_when(
      ml_g == ml_m ~ ml_g,
      TRUE ~ paste0("$ l_0(X) $: ", ml_g, " $ m_0(X) $ : ", ml_m)
    ),
    Algorithms = str_replace_all(Algorithms, vec_mle_replace)
  ) %>% 
  arrange(desc(model)) %>% 
  select(-ml_g, -ml_m) %>% 
  transform_scientific(2) %>% 
  set_names(c("$ \\hat{\\theta}_0 $", "SE", "p-value", 
              "Prediction Error $ \\hat{l}_0(X) $", 
              "Prediction Error $ \\hat{m}_0(X) $",
              "Model")) %>% 
  stargazer(
    summary = FALSE,
    out = "Results/Tables/e401k.tex",
    title = "Estimates of $ \theta_0 $ for the Partial Linear Regression and
    Interactive Model",
    label = "tab_example_e401k"
  )

df_results_final$pval <- format(df_results_final$pval, digits = 2, scientific = TRUE)