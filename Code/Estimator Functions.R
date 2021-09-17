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
  x_cols, y_col, d_cols, z_cols = NULL, 
  dml_class = DoubleMLPLR, ml_g, ml_m,
  rsmp_key = c("cv", "repeated_cv", "no_cf"), 
  rsmp_args = list(folds = 5, repeats = 3), tune = FALSE, 
  par_grids = NULL, tune_settings = NULL, tune_on_fold = TRUE,
  na.rm = TRUE, list_globals, ...){
  
  
  # Setup DML ---------------------------------------------------------------
  
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
  
  # Get extra arguments into list form
  list_dml_args <- list(...)
  
  
  # Setting Sample Splits ---------------------------------------------------
  
  if (str_type_d == "classif") {
    
    # In case d cols is numeric
    dataset <- dataset %>% mutate(across(d_cols, as.factor))
    
    if (rsmp_key %in% c("cv", "repeated_cv")) {
      
      list_dml_args$apply_cross_fitting <- TRUE
      list_dml_args$draw_sample_splitting <- FALSE
      
      # Create a new task
      new_task <- TaskRegr$new(id = "help task", backend = dataset, target = y_col)
      new_task$set_col_roles(cols = d_cols, roles = "stratum")
      new_rsmp <- exec(rsmp, .key = rsmp_key, !!!rsmp_args)
      new_rsmp$instantiate(new_task)
      
    }
    
    if (rsmp_key == "cv") {
      
      list_dml_args$n_folds <- new_rsmp$param_set$values$folds
      list_dml_args$n_rep <- 1
      list_train <- map(1:new_rsmp$iters, ~ new_rsmp$train_set(.x))
      list_test <- map(1:new_rsmp$iters, ~ new_rsmp$test_set(.x))
      
      list_samples <- list(list(train_ids = list_train, test_ids = list_test))
      
    } else if (rsmp_key == "repeated_cv") {
      
      list_dml_args$n_folds <- new_rsmp$param_set$values$folds
      list_dml_args$n_rep <- new_rsmp$param_set$values$repeats
      
      vec_repeats <- seq(1, list_dml_args$n_rep)
      
      list_samples <- map(vec_repeats, function(repeats){
        
        vec_folds <- seq((list_dml_args$n_folds) * (repeats - 1) + 1, 
                         list_dml_args$n_folds * repeats)
        
        list_train <- map(vec_folds, ~ new_rsmp$train_set(.x))
        list_test <- map(vec_folds, ~ new_rsmp$test_set(.x))
        
        list(train_ids = list_train, test_ids = list_test)
        
      })
      
    } else if (rsmp_key == "no_cf") {
      
      list_dml_args$n_folds <- 1
      list_dml_args$n_rep <- 1
      list_dml_args$apply_cross_fitting <- FALSE
      list_dml_args$draw_sample_splitting <- TRUE
      
    } else {
      
      stop("Only 'cv', 'repeated_cv' and 'no_cf' are currently supported", 
           call. = FALSE)
      
    }
    
  } else {
    
    if (is_empty(list_dml_args$n_folds)) {
      list_dml_args$n_folds <- 5
    }
    
    if (is_empty(list_dml_args$n_rep)) {
      list_dml_args$n_rep <- 1
    }
    
  }
  
  
  # Prepare Learners --------------------------------------------------------
  
  # No need to create a cross product of all learners
  grid_lrns <- list(str_g = ml_g, str_m = ml_m)
  
  if (!tune) {
    
    if (is.null(par_grids)) {
      
      if ((str_remove(ml_g, "^.*\\.") != str_remove(ml_m, "^.*\\.")) %>% any()) {
        stop("When no tuning is performed design points are used. Currently the DML
               package allows only for supplying one design point. Machine learning
               estimators need to have the same parameterspace.", call. = FALSE)
      }
      
      source("/Code/Definition Parameter Space.R")
      
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
  
  # Tune Settings -----------------------------------------------------------
  
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
  
  int_folds <- list_dml_args$n_folds
  int_repeats <- list_dml_args$n_rep
  
  # Random Forests can have for mtry at most length(x_cols)
  if (str_detect(ml_g, "\\.ranger$") %>% any()) par_grids$ranger$ml_g$params$mtry$upper <- length(x_cols)
  if (str_detect(ml_m, "\\.ranger$") %>% any()) par_grids$ranger$ml_m$params$mtry$upper <- length(x_cols)
  
  # SVM if no tuning performed is a hot mess. Need to trim the parameterspace
  if ((str_detect(ml_g, "\\.svm$") %>% any()) & !tune) par_grids$svm$ml_g$subset("kernel")
  if ((str_detect(ml_m, "\\.svm$") %>% any()) & !tune) par_grids$svm$ml_m$subset("kernel")
  
  
  # DML Fitting -------------------------------------------------------------
  
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
    
    dml_est <- exec(
      dml_class$new,
      data = data_dml,
      ml_g = lrn_g, 
      ml_m = lrn_m,
      !!!list_dml_args,
    )
    
    if (!list_dml_args$draw_sample_splitting) {
      dml_est$set_sample_splitting(list_samples)
    }
    
    
    # Tuning DML Object -------------------------------------------------------
    
    if (!tune) {
      
      # Get design point and overwrite algorithm in tune_settings
      dt_design_point <- list_design_points[[str_remove(str_g, "^.*\\.")]]
      
      # Mtry parameter of random forests depends on n.
      if (str_detect(str_g, "\\.ranger$")) {
        dt_design_point$mtry <- floor(sqrt(length(x_cols)))
      }
      
      tune_settings$algorithm <- tnr("design_points", design = dt_design_point)
      
    }
    
    ind_g <- str_which(names(par_grids), str_remove(str_g, "^.*\\."))
    ind_m <- str_which(names(par_grids), str_remove(str_m, "^.*\\."))
    
    list_par_grids <- list(
      ml_g = par_grids[[ind_g]]$ml_g, 
      ml_m = par_grids[[ind_m]]$ml_m
    )
    
    
    # Tuning with Cross Fitting -----------------------------------------------
    
    if (list_dml_args$apply_cross_fitting) {
      
      start_time <- Sys.time()
      ddpcr::quiet(
        dml_est$tune(
          param_set = list_par_grids, 
          tune_settings = tune_settings,
          tune_on_folds = tune_on_fold)
      )
      stop_time <- Sys.time()
      dbl_time_taken <- stop_time - start_time
      
      # It is possible to have multiple treatment variables. For now just assume one
      df_grid <- expand.grid(
        rep = 1:int_repeats, fold = 1:int_folds, fun = c("ml_g", "ml_m"))
      dt_tuning_results <- df_grid %>% pmap(function(rep, fold, fun){
        
        vec_tuning_res <- dml_est$tuning_res$D[[rep]][[fun]][[1]]$tuning_result[[fold]]$tuning_result
        cbind.data.frame(
          vec_tuning_res, 
          fun = fun, 
          rep = rep, 
          fold = fold, 
          mle = ifelse(fun == "ml_g", str_g, ifelse(fun == "ml_m", str_m, NA)))
        
      }) %>% map_dfr(~ .x)
      
      list_tuning_results <- dml_est$tuning_res
      
      # Tuning Without Cross Fitting ---------------------------------------------
      
    } else {
      
      # Define tasks
      if (lrn_g$task_type == "regr"){
        nuis_g <- TaskRegr$new(
          id = "nuis_g",
          backend = dataset[, c(y_col, x_cols)],
          target = y_col
        )
      } else if (lrn_g$task_type == "classif") {
        nuis_g <- TaskClassif$new(
          id = "nuis_g",
          backend = dataset[, c(y_col, x_cols)],
          target = y_col
        )
      } else {
        stop("Task 'nuis' must be either of type 'regr' or 'classif'.", 
             call. = FALSE)
      }
      if (lrn_m$task_type == "regr"){
        nuis_m <- TaskRegr$new(
          id = "nuis_m",
          backend = dataset[, c(d_cols, x_cols)],
          target = d_cols
        )
      } else if (lrn_m$task_type == "classif") {
        nuis_m <- TaskClassif$new(
          id = "nuis_m",
          backend = dataset[, c(d_cols, x_cols)],
          target = d_cols
        )
      } else {
        stop("Task 'nuis_m' must be either of type 'regr' or 'classif'.", 
             call. = FALSE)
      }
      
      # Create Tuning instances
      inst_nuis_g <- TuningInstanceSingleCrit$new(
        task = nuis_g,
        learner = lrn_g,
        resampling = tune_settings$rsmp_tune,
        measure = tune_settings$measure$ml_g,
        terminator = tune_settings$terminator,
        search_space = list_par_grids$ml_g,
        store_benchmark_result = TRUE
      )
      
      if (tune_settings$measure$ml_m$id == "classif.logloss") {
        lrn_m$predict_type <- "prob" 
      }
      
      inst_nuis_m <- TuningInstanceSingleCrit$new(
        task = nuis_m,
        learner = lrn_m,
        resampling = tune_settings$rsmp_tune,
        measure = tune_settings$measure$ml_m,
        terminator = tune_settings$terminator,
        search_space = list_par_grids$ml_m,
        store_benchmark_result = TRUE
      )
      
      # Tune
      start_time <- Sys.time()
      ddpcr::quiet({
        tune_settings$algorithm$optimize(inst_nuis_g)
        tune_settings$algorithm$optimize(inst_nuis_m)
      })
      stop_time <- Sys.time()
      dbl_time_taken <- stop_time - start_time
      
      # Get tuning results
      dt_inner_tr_g <- inst_nuis_g$result
      dt_inner_tr_m <- inst_nuis_m$result
      
      dt_tuning_results <- bind_rows(dt_inner_tr_g, dt_inner_tr_m)
      
      # Add information of dml process
      dt_tuning_results$fun <- c("ml_g", "ml_m")
      dt_tuning_results$fold <- 1
      dt_tuning_results$rep <- 1
      dt_tuning_results$mle <- c(str_g, str_m)
      
      # Extract outer results
      msr_g_val_set <- NA
      msr_m_val_set <- NA
      
      if (tune) {
        
        # Reset parameters
        lrn_g$param_set$values <- inst_nuis_g$result_learner_param_vals
        lrn_m$param_set$values <- inst_nuis_m$result_learner_param_vals
        
      }
      
      list_tuning_results <- NULL
      
    }
    
    
    # Fitting DML -------------------------------------------------------------
    
    ddpcr::quiet(dml_est$fit(store_predictions = TRUE))
    
    df_results <- cbind.data.frame(dml_est$coef, dml_est$se, DF = NA, dml_est$pval)
    colnames(df_results) <- c("parameter_est", "sd", "df", "p_value")
    
    if (int_repeats == 1) {
      df_results$type_rep_est <- "No Repeats"
    } else if (int_repeats > 1) {
      vec_results_mean <- dml_mean(dml_est, na.rm = na.rm)
      df_results <- rbind(df_results, vec_results_mean)
      df_results$type_rep_est <- c("median", "mean")
    }
    
    list_settings <- list(
      `DML algorithm` = dml_est$dml_procedure,
      `Learner` = dml_est$learner,
      `N Folds` = int_folds,
      `N Rep` = int_repeats,
      `Tuning Results` = list_tuning_results
    )
    
    list_predictions <- dml_est$predictions
    names(list_predictions) <- str_remove(names(list_predictions), "ml_")
    
    if (list_dml_args$apply_cross_fitting) {
      
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
      
    }
    
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
  
  
  # Getting Accuracy Measures -----------------------------------------------
  
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
  
  # Extracting Result -------------------------------------------------------
  
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
      ml_g = rep(ml_g, each = if_else(int_repeats == 1, 1, 2)),
      ml_m = rep(ml_m, each = if_else(int_repeats == 1, 1, 2)),
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
  # In case no cross fitting was performed choose learner with min in sample.
  if (rsmp_key == "no_cf"){
    str_g <- df_msrs %>% filter(fun == "ml_g", min_msr_in) %>% pull(mle)
    str_m <- df_msrs %>% filter(fun == "ml_m", min_msr_in) %>% pull(mle)
  } else {
    str_g <- df_msrs %>% filter(fun == "ml_g", min_msr_val) %>% pull(mle)
    str_m <- df_msrs %>% filter(fun == "ml_m", min_msr_val) %>% pull(mle)
  }
  
  str_name_best <- paste0("g best: ", str_g, " m best: ", str_m, collapse = "")
  
  # Refitting DML with Best Learners ----------------------------------------
  
  if (which(grid_lrns$str_g == str_g) != which(grid_lrns$str_m == str_m)) {
    
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
    
    dml_est <- exec(
      dml_class$new,
      data = data_dml,
      ml_g = lrn_g, 
      ml_m = lrn_m,
      !!!list_dml_args,
    )
    
    if (!list_dml_args$draw_sample_splitting) {
      dml_est$set_sample_splitting(list_samples)
    }    
    
    if (rsmp_key != "no_cf") {
      
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
      
      lgl_set_fold_specific <- TRUE
      
    } else {
      
      list_params_ml_g <- list_tuning[[str_remove(str_g, "^.*\\.")]] %>% 
        pluck("Tuning_Results") %>% 
        filter(fun == "ml_g") %>% 
        pluck("learner_param_vals") %>% 
        pluck(1)
        
      list_params_ml_m <- list_tuning[[str_remove(str_m, "^.*\\.")]] %>% 
        pluck("Tuning_Results") %>% 
        filter(fun == "ml_m") %>% 
        pluck("learner_param_vals") %>% 
        pluck(1)
        
      lgl_set_fold_specific <- FALSE
      
    }
    
    dml_est$set_ml_nuisance_params("ml_g", d_cols, list_params_ml_g, 
                                   set_fold_specific = lgl_set_fold_specific)
    dml_est$set_ml_nuisance_params("ml_m", d_cols, list_params_ml_m, 
                                   set_fold_specific = lgl_set_fold_specific)
    
    ddpcr::quiet(dml_est$fit(store_predictions = TRUE))
    
    df_estimates_best <- data.frame(
      parameter_est = dml_est$coef,
      sd = dml_est$se,
      df = NA, 
      p_value = dml_est$pval
    )
    
    if (int_repeats > 1) {
      vec_mean <- dml_mean(dml_est, na.rm)
      
      df_estimates_best <- df_estimates_best %>% 
        rbind(vec_mean)
    }
    
    df_estimates$ml_g <- dml_est$learner$ml_g$id
    df_estimates$ml_m <- dml_est$learner$ml_m$id
    df_estimates$time_tuning <- NA
    
    list_settings <- list(
      `DML algorithm` = dml_est$dml_procedure,
      `N Folds` = dml_est$n_folds,
      `N Rep` = dml_est$n_rep,
      `Learner` = dml_est$learner
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
