library(DoubleML)
library(ggpubr)
library(magrittr)
library(mlr3)
library(tidyverse)

# Non orthogonal scores
non_orth_score = function(y, d, g_hat, m_hat, smpls) {
  u_hat = y - g_hat
  psi_a = -1*d*d
  psi_b = d*u_hat
  psis = list(psi_a = psi_a, psi_b = psi_b)
  return(psis)
}

list_quos_2_env <- function(list_obj){
  
  walk(names(list_obj), ~ {
    assign(
      .x, 
      rlang::get_expr(
        list_obj[[.x]]
      ) %>% eval(),
      envir = parent.env(parent.env(environment()))
    )
  })  
  
}

is_data <- function(x, N){
  # Check whether x is of type matrix/df
  if (!is.null(dim(x))) {
    nrow(x) == N
  } else {
    length(x) == N 
  }
}

# Produces confidence interval for every estimation result in a monte carlo object.
confint.mcs <- function(mcs_obj, level = 0.05){
  
  df_results <- get_estimates(mcs_obj)
  
  if (
    str_detect(colnames(df_results), "df") && 
    df_results %>% select(matches("df")) %>% is.na() %>% not()) {
    
    crit_val <- qt(1 - level / 2, df_results %>% select(matches("df")))
    
  } else {
    
    crit_val <- qnorm(1 - level / 2)
    
  }
  
  df_results <- df_results %>% 
    mutate(
      Lower = parameter_est - crit_val * sd,
      Upper = parameter_est + crit_val * sd
    )
  
  df_results
  
}

# Function to fetch true parameters. The function returns a list
get_parameter <- function(mcs_obj, parameter_names){
  
  parameter <- mcs_obj$dgp %>% get_arguments()
  
  parameter <- parameter[parameter_names]
  
  parameter <- map(parameter, ~{
    .x %>% rlang::eval_tidy()
  })
  
  # Reduce list to a vector
  unlist(parameter) 
  
}

# Funtion to check whether a input function is p integrable
is_p_integrable <- function(fun, lower = -Inf, upper = Inf, p = 2, ...){
  
  fun_int <- function(x) abs(fun(x, ...)) ^ p
  
  tryCatch(
    expr = integrate(fun_int, lower, upper, ...)$value < Inf, 
    error = function(cond) FALSE
  )
  
}

# Rowwise map function
rmap <- function(.x, .f, ...){
  
  if (is.null(dim(.x))) stop("'dim(X)' must have a positive length.")
  
  .x <- t(.x) %>% as.data.frame()
  purrr::map(.x, .f, ...)
  
}

# Function to compute the expected difference between two functions
# Use an estimate not the integrand
calc_err_exact <- function(mcs_obj){
  
  # Get sample splits
  
  # Perform machine learning estimator for all sample splits and get a functional
  
  # Calculate then the integrated difference between the functionals
  
}

calc_err_estimate <- function(mcs_obj, function_names = c("g", "m"), categorial = c(FALSE, TRUE), na.rm = TRUE){
  
  # Get functions into the environment as a named list
  list_functions <- mcs_obj$dgp$arguments[function_names] %>% 
    map(~ {
      rlang::get_expr(.x) %>% 
        rlang::eval_tidy()
    })
  
  vec_names_fun <- list_functions %>% names()
  
  # Get arguments into vector
  list_names_args <- list_functions %>% 
    map(~ formals(.x) %>% names() %>% unique())
  
  # Also get non data parts into the environment
  names_data <- mcs_obj$dgp$datasets %>% 
    map(~ .x$data %>% colnames()) %>% 
    unlist() %>% 
    str_remove("\\..*") %>% 
    unique()
  
  list_args <- list_names_args %>% 
    map(function(arguments){
      
      # Extract relevant argument names
      names_args <- arguments[!(arguments %in% names_data)]
      names_data <- arguments[(arguments %in% names_data)]
      
      # Evaluate the relevant arguments
      mcs_obj$dgp$arguments[names_args] %>% 
        map(~{.x %>% rlang::get_expr() %>% rlang::eval_tidy()}) %>% 
        set_names(names_args) %>% 
        append(list(data = names_data))
      
    })
  
  # Get N and Sample for identification
  vec_N <- mcs_obj$dgp$datasets %>% map_dbl(~ .x$N)
  vec_Sample <- mcs_obj$dgp$datasets %>% map_dbl(~ .x$Sample)
  
  # Calculate for every dataset the difference between g(x), m(x) and the prediction
  df_true <- mcs_obj$dgp$datasets %>% 
    map(function(datasets){
      
      # Get predictions into list format
      ind_results <- mcs_obj$results %>% 
        names() %>% 
        str_which(pattern = paste0("Sample = ", datasets$Sample, " with N = ", datasets$N))
      
      map(function_names, function(fun){
        # browser()
        # Create a named list containing data and args
        list_fun_args <- list_args[[fun]]$data %>% 
          map(~{
            datasets$data %>% 
              select(
                starts_with(
                  .x
                )
              ) %>% 
              as.matrix()
          }) %>% 
          set_names(list_args[[fun]]$data) %>% 
          append(list_args[[fun]][list_args[[fun]] %>% names() %>% str_detect("data", negate = TRUE)])
        
        # Reorder list
        list_fun_args <- list_fun_args[formals(list_functions[[fun]]) %>% names()]  
        
        # Execute
        fun_0 <- exec(
          list_functions[[fun]],
          !!!list_fun_args
        ) %>% as.data.frame()
        
        if (fun == vec_names_fun[categorial]) fun_0 <- fun_0 %>% select(matches("^Prob_.*$"))
        
        # Get function estimator names
        vec_names_estimators <- mcs_obj$results[[ind_results]]$Output$Settings %>% 
          map_chr(~ {
            .x$Learner %>% pluck(paste0("ml_", fun)) %>% pluck("id")
          })
        
        vec_est_count <- table(vec_names_estimators)
        
        # Has multiple predictions for differing learners and repeated CV
        mcs_obj$results[[ind_results]]$Output$Predictions %>% 
          map(~ {
            
            # browser()
            # Get predictions
            fun_hat <- .x[[fun]] %>% as.vector()
            
            df_diff <- (fun_0 - fun_hat)
            
            # Calculate MSE, Bias and variance
            dbl_mse <- colMeans(df_diff ^ 2, na.rm = na.rm)
            dbl_bias <- colMeans(df_diff, na.rm = na.rm)
            dbl_variance <- var(fun_hat)
            
            df_msrs <- data.frame(
              MSE = dbl_mse, 
              Bias = dbl_bias, 
              Variance = dbl_variance
            )
            
          }) %>% 
          map_df(~ .x) %>% 
          mutate(
            N = vec_N[ind_results],
            Sample = vec_Sample[ind_results],
            Estimator = vec_names_estimators,
            Fun = fun,
            Best = case_when(
              MSE == min(MSE, na.rm = na.rm) ~ TRUE,
              TRUE ~ FALSE
            )
          ) %>% 
          unique()
        
      }) %>% 
        map_df(~ .x)
      
    }) %>% 
    map_df(~ .x) %>% 
    set_rownames(NULL)
  
  # Group results by N, Estimator and function.
  df_msrs_aggregate <- df_true %>% 
    group_by(N, Fun, Estimator) %>% 
    summarise(
      MSE = mean(MSE, na.rm = na.rm),
      Bias = mean(Bias, na.rm = na.rm),
      Variance = mean(Variance, na.rm = na.rm)
    )
  
  list(aggregated = df_msrs_aggregate, raw = df_true)
  
}

rep_vector <- function(x, times){
  map(1:times, ~x) %>% 
    map_df(~.x)
}

# Function to create higher order interactions
create_interactions <- function(X, order = 2){
  
  if (is.matrix(X)) X <- data.frame(X)
  
  if (is.null(colnames(X))) colnames(X) <- paste("X", 1:ncol(X))
  
  vec_colnames <- colnames(X)
  vec_class <- map_chr(X, ~class(.x))
  vec_not_numeric <- names(vec_class)[str_detect(vec_class, "numeric", negate = TRUE)]
  
  map(1:order, ~{
    
    if (.x == 1) { 
      
      paste0(vec_colnames, collapse = " + ")
      
    } else {
      
      list_names <- map(1:.x, ~vec_colnames)
      
      # Use expand.grid to create the level of interaction
      df_grid <- exec(expand.grid, list_names, stringsAsFactors = FALSE)
      
      # Remove polynomials of factors
      if (!is_empty(vec_not_numeric)) {
        
        # Detect rows where more than one entry of a non-numeric variable is present
        vec_index <- df_grid %>% 
          rmap(function(row){
            
            map_lgl(vec_not_numeric, ~{
              str_detect(row, paste0("^", .x, "$")) %>% sum() %>% is_greater_than(1)
            }) %>% 
              sum() %>% 
              is_weakly_greater_than(1)
            
          }) %>% 
          unlist()
        
        df_grid <- df_grid[!vec_index, ]
        
      }
      
      # Bind into single string
      rmap(df_grid, ~paste0("I(", paste0(.x, collapse = " * "), ")")) %>% 
        paste0(collapse = " + ")
    }
    
  }) %>% paste0(collapse = " + ")
  
}

# Function to calculate rate of convergence from MSEs and N
estimate_rate <- function(mse_data, plot = TRUE, na.rm = TRUE){
  
  # Calculate for rising N MSE_2 / MSE_1 * ()
  df_rates <- mse_data %>% 
    group_by(algorithms) %>% 
    mutate(Rate = 1 - (MSE / MSE[which(N == min(N, na.rm = na.rm))]) ^ (1 / (N / min(N, na.rm = na.rm) - 1)))
  
  # Replace rates for smallest N with NA
  df_rates[df_rates$N == min(df_rates$N, na.rm = na.rm), "Rate"] <- NA
  
  # Calculate a mean, median and se for rate
  df_rates_desc <- df_rates %>% 
    summarise(
      Mean = mean(Rate, na.rm = na.rm),
      `Standard Deviation` = sd(Rate, na.rm = na.rm),
      Minimum = min(Rate, na.rm = na.rm),
      `25% Quantile` = quantile(Rate, na.rm = na.rm, prob = 0.25),
      Median = median(Rate, na.rm = na.rm),
      `75% Quantile` = quantile(Rate, na.rm = na.rm, prob = 0.75),
      Maximum = max(Rate, na.rm = na.rm)
    )
  
  if (plot) {
    
    mse_plot <- ggplot(mse_data, aes(x = N, y = MSE, col = str_to_title(algorithms))) + 
      geom_point() + 
      geom_line() + 
      theme_bw() +
      labs(col = "Algorithms") 
    
    rates_plot <- ggplot(df_rates, aes(x = N, y = Rate, col = str_to_title(algorithms))) + 
      geom_point() + 
      geom_line() + 
      theme_bw() +
      labs(col = "Algorithms")
    
    plot_mse_rate <- ggarrange(mse_plot, rates_plot, common.legend = TRUE, legend = "right")
    
    list(rate = df_rates, rate_desc = df_rates_desc, plot = plot_mse_rate)
    
  } else {
    
    list(data = df_rates, rate_desc = df_rates_desc)
    
  }
  
}

# DML package has no mean function
dml_mean <- function(dml_obj, na.rm = TRUE){
  
  if (dml_obj$all_coef %>% is.na() %>% all()) {
    stop("Call fit method first")
  }
  
  int_S <- dml_obj$n_rep
  mat_theta_mean <- dml_obj$all_coef
  mat_sigma_mean <- dml_obj$all_se
  
  vec_theta_mean <- rowMeans(mat_theta_mean, na.rm = na.rm)
  vec_sigma_mean <- sqrt(rowMeans(mat_sigma_mean ^ 2 + (mat_theta_mean - vec_theta_mean) ^ 2, na.rm = na.rm))
  vec_t_value <- vec_theta_mean / vec_sigma_mean
  vec_p_value <- pnorm(vec_t_value, lower.tail = FALSE) * 2
  
  mat_mean <- cbind(vec_theta_mean, vec_sigma_mean, NA, vec_p_value)
  colnames(mat_mean) <- c("parameter_est", "sd", "df", "p_value")
  
  mat_mean
  
}

# Function to calculate measures on the holdout set
# Would use msr from mlr3 package but is unable to broadcast. 
# Error in case of Repeats.
msr_validation_set <- function(msr, truth, response){
  
  if (is.character(msr) && (msr %in% mlr3::msrs()$keys())) {
    msr <- msr(msr)
  } else if (!("Measure" %in% class(msr))) {
    stop("msr has to be a measure exported by mlr3 or of class Measure")
  }
  
  # Get response into vector form
  vec_response <- as.vector(response)
  
  # Now repeat truth vector
  int_times <- length(vec_response) /length(truth)
  
  # Check that it is an integer
  if (floor(int_times) != int_times) {
    stop("Response is not a multiple of truth.", call. = FALSE)
  }
  
  vec_truth_repeated <- rep(truth, int_times)
  
  if (msr$task_type == "classif") {
    
    if (!is.factor(vec_truth_repeated)) {
      stop("Truth must be of type factor.", call. = FALSE)
    }
    
    # Assume for now that only two classes are present
    mat_prob <- cbind(1 - vec_response, vec_response)
    
    if (ncol(mat_prob) != length(levels(vec_truth_repeated))) {
      stop("Function supports for now only two classes.", call. = FALSE)
    }
    
    colnames(mat_prob) <- levels(vec_truth_repeated)
    
    msr$fun(vec_truth_repeated, mat_prob)
    
  } else if(msr$task_type == "regr"){
    
    msr$fun(vec_truth_repeated, vec_response)
    
  }
  
}

# Function to calculate MSE, Bias and Variance of Estimator
calc_err_approx <- function(truth, response, na.rm = TRUE){
  
  MSE <- mean((truth - response) ^ 2, na.rm = na.rm)
  Bias <- mean((truth - response), na.rm = na.rm)
  Var <- var(response, na.rm = na.rm)
  
  c(mse = MSE, bias = Bias, variance = Var)
  
}

quick_save <- function(x, folder = "Results/Data/"){
  
  str_x_name <- deparse(quote(x))
  str_path <- paste0(folder, str_x_name, ".RData")
  save(x, file = str_path)
  
}