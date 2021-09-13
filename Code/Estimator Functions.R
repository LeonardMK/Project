library(data.table)
library(DoubleML)
library(mlr3verse)
library(tidyverse)

source("Code/Utils.R")

# General structure of an estimator function
# Inputs are data, formulas and other specifications
# Output is supposed to be a list
# Parameter of interest
# Standard errors for Parameter of Interest
# Degrees of Freedom
# Confidence Bands if needed
# tune can be FALSE, then default values are used,
# TRUE, then a named list, called par_grid, has to be specified, controlling the search space.
# Further tune_settings can be specified.
# Since a limited number of folds can be specified, which can lead to repetitions
# in small samples a function is needed, controlling the number of repeated cv.


dml_estimator <- function(
  dataset, 
  x_cols = NULL, y_col = NULL, d_cols = NULL, z_cols = NULL, 
  draw_sample_splitting = TRUE, dml_class = DoubleMLPLR, ml_g, ml_m,
  tune = FALSE, rsmp_key = "repeated_cv", rsmp_args = list(folds = 5, repeats = 3),
  par_grids = NULL, tune_settings = NULL, tune_on_fold = TRUE,
  na.rm = TRUE, list_globals, ...){
  
  if (!is.null(list_globals)) list2env(list_globals, environment())
  
  # Check that every estimator in ml_g, ml_m has a par_grids value
  if (
    (!(str_remove(ml_g, "^.*\\.") %in% names(par_grids) %>% all()) |
     !(str_remove(ml_m, "^.*\\.") %in% names(par_grids) %>% all())) &
    tune
  ) {
    stop(
      "You need to supply a parameter space for every machine learning estimator.", 
      call. = FALSE)
  }
  
  # Check that all MLE perform either regression or classification
  if (str_detect(ml_g, "^regr\\.*") %>% all()) {
    str_type_y <- "regr"
  } else if (str_detect(ml_g, "^classif\\.*") %>% all()) {
    str_type_y <- "classif"
  } else {
    stop("All elements in 'ml_g' must be either of type regression or classification.")
  }
  
  if (str_detect(ml_m, "^regr\\.*") %>% all()) {
    str_type_d <- "regr"
  } else if (str_detect(ml_m, "^classif\\.*") %>% all()) {
    str_type_d <- "classif"
  } else {
    stop("All elements in 'ml_m' must be either of type regression or classification.")
  }
  
  data_dml <- DoubleMLData$new(
    dataset, 
    x_cols,
    y_col, 
    d_cols,
    z_cols, 
  )
  
  # Calculate E[Y | X]
  dataset$E_Y_X <- dataset[, y_col] - dataset[, "E"]
  
  # Set sample splits manually if ml_m is classification
  if (str_type_d == "classif") {
    
    draw_sample_splitting <- FALSE
    
    # In case d cols is numeric
    dataset <- dataset %>% mutate(across(d_cols, as.factor))
    
    # Create a new task
    new_task <- TaskRegr$new(id = "help task", backend = dataset, target = y_col)
    new_task$set_col_roles(cols = d_cols, roles = "stratum")
    new_rsmp <- exec(rsmp, .key = rsmp_key, !!!rsmp_args)
    new_rsmp$instantiate(new_task)
    
    if (rsmp_key == "cv") {
      
      list_train <- map(1:new_rsmp$iters, ~ new_rsmp$train_set(.x))
      list_test <- map(1:new_rsmp$iters, ~ new_rsmp$test_set(.x))
      
      list_samples <- list(list(train_ids = list_train, test_ids = list_test))
      
    } else if (rsmp_key == "repeated_cv") {
      
      int_folds <- new_rsmp$param_set$values$folds
      int_repeats <- new_rsmp$param_set$values$repeats
      
      vec_repeats <- seq(1, int_repeats)
      
      list_samples <- map(vec_repeats, function(repeats){
        
        vec_folds <- seq((int_folds) * (repeats - 1) + 1, int_folds * repeats)
        
        list_train <- map(vec_folds, ~ new_rsmp$train_set(.x))
        list_test <- map(vec_folds, ~ new_rsmp$test_set(.x))
        
        list(train_ids = list_train, test_ids = list_test)
        
      })
      
    } else {
      
      stop("Only 'cv' and 'repeated_cv' are currently supported", call. = FALSE)
      
    }
    
  }
  
  # Choose only the learners with the best metric
  # No need to create a cross product of all learners
  grid_lrns <- list(str_g = ml_g, str_m = ml_m)
  
  if (!tune) {
    
    if (is.null(par_grids)) {
      
      if ((str_remove(ml_g, "^.*\\.") != str_remove(ml_m, "^.*\\.")) %>% any()) {
        stop("When no tuning is performed design points are used. Currently the DML
               package allows only for supplying one design point. Machine learning
               estimators need to have the same parameterspace.", call. = FALSE)
      }
      
      source("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/6. Semester/Masterarbeit/Project/Code/Definition Parameter Space.R")
      
      par_grids <- list_parameterspace
      
    }
    
  } else {
    
    # Make a clone of par_grids to circumvent overwriting
    par_grids <- par_grids %>% map(~ {
      list(
        ml_g = .x$ml_g$clone(),
        ml_m = .x$ml_m$clone()
      )
    })
    
  }
    
  if (is.null(tune_settings)) {
    
    str_msr_g <- if_else(str_type_y == "regr", "regr.mse", "classif.logloss")
    str_msr_m <- if_else(str_type_d == "regr", "regr.mse", "classif.logloss")
  
    tune_settings <- list(
      terminator = trm("evals", n_evals = 100),
      rsmp_tune = rsmp("cv", folds = 5),
      measure = list(
        ml_g = msr(str_msr_g),
        ml_m = msr(str_msr_m)
      )
    )
    
  } else {
    
    str_msr_g <- tune_settings$measure$ml_g$id
    str_msr_m <- tune_settings$measure$ml_m$id
    
  }
  
  # Random Forests can have for mtry at most length(x_cols)
  if (str_detect(ml_g, "\\.ranger$") %>% any()) par_grids$ranger$ml_g$params$mtry$upper <- length(x_cols)
  if (str_detect(ml_m, "\\.ranger$") %>% any()) par_grids$ranger$ml_m$params$mtry$upper <- length(x_cols)
  
  # SVM if no tuning performed is a hot mess. Need to trim the parameterspace
  if ((str_detect(ml_g, "\\.svm$") %>% any()) & !tune) par_grids$svm$ml_g$subset("kernel")
  if ((str_detect(ml_m, "\\.svm$") %>% any()) & !tune) par_grids$svm$ml_m$subset("kernel")
  
  # Now map over all useful combinations
  list_tuning <- pmap(grid_lrns, function(str_g, str_m){
    
    # SVM need an explicit mention what kind of classif/regr will be performed
    if(str_detect(str_g, "\\.svm$")) {
      lrn_g = exec(lrn, !!!str_g, type = if_else(str_type_y == "regr", "eps-regression", "C-classification"))
    } else {
      lrn_g = exec(lrn, !!!str_g)
    }
    if(str_detect(str_m, "\\.svm$")) {
      lrn_m = exec(lrn, !!!str_m, type = if_else(str_type_d == "regr", "eps-regression", "C-classification"))
    } else {
      lrn_m = exec(lrn, !!!str_m)
    }
    
    dml_est <- dml_class$new(
      data_dml,
      lrn_g, 
      lrn_m,
      ...,
      draw_sample_splitting = draw_sample_splitting
    )
    
    if (!draw_sample_splitting) dml_est$set_sample_splitting(list_samples)
    
    if (!tune) {
      
      # Get design point and overwrite algorithm in tune_settings
      dt_design_point <- list_design_points[[str_remove(str_g, "^.*\\.")]]
      
      # Mtry parameter of random forests depends on n.
      if (str_detect(str_g, "\\.ranger$")) dt_design_point$mtry <- floor(sqrt(length(x_cols)))
      
      tune_settings$algorithm <- tnr("design_points", design = dt_design_point)
      
    }
    
    ind_g <- str_which(names(par_grids), str_remove(str_g, "^.*\\."))
    ind_m <- str_which(names(par_grids), str_remove(str_m, "^.*\\."))
    
    list_par_grids <- list(ml_g = par_grids[[ind_g]]$ml_g, ml_m = par_grids[[ind_m]]$ml_m)
    
    start_time <- Sys.time()
    ddpcr::quiet(
      dml_est$tune(param_set = list_par_grids, tune_settings = tune_settings, tune_on_folds = tune_on_fold)
      )
    stop_time <- Sys.time()
    dbl_time_taken <- stop_time - start_time
    
    # It is possible to have multiple treatment variables. For now just assume one
    df_grid <- expand.grid(rep = 1:int_repeats, fold = 1:int_folds, fun = c("ml_g", "ml_m"))
    dt_tuning_results <- df_grid %>% pmap(function(rep, fold, fun){
      
      vec_tuning_res <- dml_est$tuning_res$D[[rep]][[fun]][[1]]$tuning_result[[fold]]$tuning_result
      cbind.data.frame(
        vec_tuning_res, 
        fun = fun, 
        rep = rep, 
        fold = fold, 
        mle = ifelse(fun == "ml_g", str_g, ifelse(fun == "ml_m", str_m, NA)))
      
    }) %>% map_dfr(~ .x)
    
    # Function returns a list. First entry holds the results.
    # The second the specifications
    ddpcr::quiet(dml_est$fit(store_predictions = TRUE))
    
    df_results <- cbind.data.frame(dml_est$coef, dml_est$se, DF = NA, dml_est$pval)
    colnames(df_results) <- c("parameter_est", "sd", "df", "p_value")
    
    if (dml_est$n_rep == 1) {
      df_results$type_rep_est <- "No Repeats"
    } else if (dml_est$n_rep > 1) {
      vec_results_mean <- dml_mean(dml_est, na.rm = na.rm)
      df_results <- rbind(df_results, vec_results_mean)
      df_results$type_rep_est <- c("median", "mean")
    }
    
    list_settings <- list(
      `DML algorithm` = dml_est$dml_procedure,
      `N Folds` = dml_est$n_folds,
      `N Rep` = dml_est$n_rep,
      `Learner` = dml_est$learner,
    )
    
    list_predictions <- dml_est$predictions
    names(list_predictions) <- str_remove(names(list_predictions), "ml_")
    
    # Calculate the prediction error on the holdout sets
    # G estimates E[Y | X]
    msr_g_val_set <- msr_validation_set(
        msr = tune_settings$measure$ml_g,
        truth = dataset[, y_col],
        response = list_predictions$g
      )
    
    # M estimates the probabilities of D
    msr_m_val_set <- msr_validation_set(
      msr = tune_settings$measure$ml_m,
      truth = dataset[, d_cols],
      response = list_predictions$m
    )
    
    # Calculate the closeness to the true function
    acc_g_0 <- calc_err_approx(dataset$E_Y_X, list_predictions$g, na.rm)
    acc_m_0 <- calc_err_approx(dataset$Prob_D, list_predictions$m, na.rm)
    
    df_val_set <- data.frame(
      fun = c("ml_g", "ml_m"),
      mean_msr_val = c(msr_g_val_set, msr_m_val_set), 
      key_msr = c(tune_settings$measure$ml_g$id, tune_settings$measure$ml_m$id),
      mle = c(str_g, str_m)
    ) %>% 
      cbind(rbind(acc_g_0, acc_m_0)) %>% 
      set_rownames(NULL)
    
    list(
      Estimates = df_results, 
      Settings = list_settings, 
      Tuning_Results = dt_tuning_results,
      Accuracy_Validation = df_val_set,
      Time = dbl_time_taken
    )    
  })
  
  vec_mle_names_g <- str_remove(ml_g, "^.*\\.")
  vec_mle_names_m <- str_remove(ml_m, "^.*\\.")
  vec_names_list_tuning <- if_else(
    vec_mle_names_g == vec_mle_names_m, 
    vec_mle_names_g, 
    paste0("g: ", vec_mle_names_g, " m: ", vec_mle_names_m))
  names(list_tuning) <- vec_names_list_tuning
  
  # Find best learner
  dt_tune_result_in <- list_tuning %>%
    map_dfr(~ {
      .x$Tuning_Results %>%
        dplyr::select(
          str_msr_g,
          str_msr_m,
          fun,
          rep,
          fold,
          mle,
          learner_param_vals
        )
    })

  df_tune_result_out <- list_tuning %>% 
    map(~ .x$Accuracy_Validation) %>% 
    map_df(~ .x)
  
  # Want to get validation set performance
  df_msrs <- dt_tune_result_in %>% 
    mutate(
      msr = case_when(
        fun == "ml_g" ~ !!sym(str_msr_g),
        fun == "ml_m" ~ !!sym(str_msr_m)
      )
    ) %>% 
    group_by(fun, mle) %>% 
    summarise(
      mean_msr_in = mean(msr, na.rm = na.rm)
    ) %>% 
    left_join(df_tune_result_out, by = c("fun", "mle")) %>% 
    mutate(
      min_msr_in = case_when(
        fun == "ml_g" & mean_msr_in == min(mean_msr_in) ~ TRUE,
        fun == "ml_m" & mean_msr_in == min(mean_msr_in) ~ TRUE,
        TRUE ~ FALSE
      ),
      min_msr_val = case_when(
        fun == "ml_g" & mean_msr_val == min(mean_msr_val) ~ TRUE,
        fun == "ml_m" & mean_msr_val == min(mean_msr_val) ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  # One dataframe containing results
  df_time <- map(list_tuning, ~ c(time_tuning = .x$Time)) %>% 
    map_df(~ .x)
  
  list_estimates <- list_tuning %>% 
    map(~{
      .x$Estimates 
    })
  
  df_estimates <- do.call(rbind.data.frame, list_estimates) %>% 
    set_rownames(NULL) %>% 
    mutate(
      ml_g = rep(ml_g, each = 2),
      ml_m = rep(ml_m, each = 2),
    ) %>% 
    cbind(time_tuning = rep(df_time$time_tuning, each = 2))
  
  # A list containing lists with specifications and predictions
  list_settings_all <- list_tuning %>% 
    map(~ .x %>% pluck("Settings")) %>% 
    set_names(
      c(
        paste0("g: ", ml_g, " m: ", ml_m)
      )
    )
  
  # Find the best ml estimators
  str_g <- df_msrs %>% filter(fun == "ml_g", min_msr_val) %>% pull(mle)
  str_m <- df_msrs %>% filter(fun == "ml_m", min_msr_val) %>% pull(mle)
  
  str_name_best <- paste0("g best: ", str_g, " m best: ", str_m, collapse = "")
  
  if (which(grid_lrns$str_g == str_g) != which(grid_lrns$str_m == str_m)) {
    # Create a new dml object with said algorithms and the given specifications
    # SVM need an explicit mention what kind of classif/regr will be performed
    if(str_detect(str_g, "\\.svm$")) {
      lrn_g = exec(lrn, !!!str_g, type = if_else(str_type_y == "regr", "eps-regression", "C-classification"))
    } else {
      lrn_g = exec(lrn, !!!str_g)
    }
    if(str_detect(str_m, "\\.svm$")) {
      lrn_m = exec(lrn, !!!str_m, type = if_else(str_type_d == "regr", "eps-regression", "C-classification"))
    } else {
      lrn_m = exec(lrn, !!!str_m)
    }
    
    dml_est <- dml_class$new(
      data_dml,
      lrn_g, 
      lrn_m,
      ...,
      draw_sample_splitting = draw_sample_splitting
    )
    
    if (!draw_sample_splitting) dml_est$set_sample_splitting(list_samples)
    
    # Get optimal parameters
    list_params_ml_g <- list_tuning %>% 
      pluck(str_remove(str_g, "^.*\\.")) %>% 
      pluck("Settings") %>% 
      pluck("Tuning Result") %>% 
      pluck(d_cols) %>% 
      map(~ .x$ml_g$params)
    
    list_params_ml_m <- list_tuning %>% 
      pluck(str_remove(str_m, "^.*\\.")) %>% 
      pluck("Settings") %>% 
      pluck("Tuning Result") %>% 
      pluck(d_cols) %>% 
      map(~ .x$ml_m$params)
    
    dml_est$set_ml_nuisance_params("ml_g", d_cols, list_params_ml_g, set_fold_specific = TRUE)
    dml_est$set_ml_nuisance_params("ml_m", d_cols, list_params_ml_m, set_fold_specific = TRUE)
    ddpcr::quiet(dml_est$fit(store_predictions = TRUE))
    vec_mean <- dml_mean(dml_est, na.rm)
    df_estimates_best <- cbind.data.frame(
      parameter_est = c(dml_est$coef, vec_mean["parameter_est"]),
      sd = c(dml_est$se, vec_mean["sd"]),
      df = NA, 
      p_value = c(dml_est$pval, vec_mean["p_value"]),
      ml_g = dml_est$learner$ml_g$id, 
      ml_m = dml_est$learner$ml_m$id,
      time_tuning = NA
      )
    
    list_settings <- list(
      `DML algorithm` = dml_est$dml_procedure,
      `N Folds` = dml_est$n_folds,
      `N Rep` = dml_est$n_rep,
      `Learner` = dml_est$learner,
    )
    
    # Now append the results from the best model
    df_estimates <- df_estimates %>%  
      rbind(df_estimates_best)
    
    list_settings_all <- list_settings_all %>% 
      append(
        list(
          list_settings
        )
      )
    
    names(list_settings_all)[length(list_settings_all)] <- str_name_best
    
  } else {
    
    # Just Rename the elements that are best
    str_best_pattern <- paste0("g: ", str_g, " m: ", str_m, "$")
    
    names(list_settings_all)[str_detect(names(list_settings_all), str_best_pattern)] <- str_name_best
    
  }
  
  # Mark for estimates wihich has lowest estimated prediction error
  df_estimates <- df_estimates %>% 
    mutate(
      algorithms = case_when(
        ml_g == str_g & ml_m == str_m ~ "Best",
        TRUE ~ paste0("G: ", str_remove(ml_g, "^.*\\."), " M: ", str_remove(ml_m, "^.*\\."))
      )
    )
  
  # Return a list
  list(
    Estimates = df_estimates,
    Settings = list_settings_all,
    Measures = df_msrs
  )
  
}
