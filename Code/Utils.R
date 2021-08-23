library(DoubleML)
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

calc_err_estimate <- function(mcs_obj, function_names = c("g", "m"), categorial = c(FALSE, TRUE)){
  
  # Get functions into the environment as a named list
  list_functions <- mcs_obj$dgp$arguments[function_names] %>% 
    map(~ {
      rlang::get_expr(.x) %>% 
        rlang::eval_tidy()
    })
  
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
  
  # Calculate for every dataset the difference between g(x), m(x) and the prediction
  list_true <- mcs_obj$dgp$datasets %>% 
    map(function(datasets){
      
      # Get predictions into list format
      ind_results <- mcs_obj$results %>% 
        names() %>% 
        str_which(pattern = paste0("Sample = ", datasets$Sample, " with N = ", datasets$N))
      
      list_true_dataset <- map(function_names, function(fun){
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
        fun_hat <- mcs_obj$results[[ind_results]]$Output$Predictions[[fun]]
        fun_0 <- exec(
          list_functions[[fun]],
          !!!list_fun_args
        )
        
        mat_true <- cbind(fun_0, fun_hat)
        
        colnames(mat_true) <- paste0(fun, "_", colnames(mat_true))
        
        mat_true
        
      })
      
      df_true_dataset <- do.call(cbind.data.frame, list_true_dataset)
      df_true_dataset$N <- datasets$N
      df_true_dataset$Sample <- datasets$Sample
      
      df_true_dataset
      
    })
  
  df_true <- list_true %>% map_dfr(~.x)
  
  # For every function calculate the difference between predicted and true
  list_diff <- function_names %>% 
    map(function(fun){
      
      if(categorial[which(function_names == fun)]){
        
        df_fun_0 <- df_true %>% 
          select(starts_with(fun) & matches("prob"))
        df_fun_hat <- df_true %>% 
          select(starts_with(fun) & ends_with("hat"))
        abs(df_fun_0 - df_fun_hat)
        
      }else{
        
        df_fun_0 <- df_true %>% 
          select(starts_with(fun) & ends_with("0"))
        df_fun_hat <- df_true %>% 
          select(starts_with(fun) & ends_with("hat"))
        abs(df_fun_0 - df_fun_hat)
      }
      
    })
  # browser()
  df_diff <- do.call(cbind, list_diff)
  colnames(df_diff) <- paste0("diff_", colnames(df_diff))
  df_total <- cbind(df_true, df_diff)
  psych::describeBy(df_total %>% select(starts_with("diff_")), group = df_total$N, mat = TRUE) %>% 
    as.data.frame() %>% 
    select(group1, min, mean, median, max) %>% 
    rename(N = group1) %>% 
    rownames_to_column(var = "fun") %>% 
    mutate(fun = str_extract(fun, "(?<=_).*(?=_[fun|Prob])"))
  
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
est_rate <- function(mse_data){
  
  # Calculate for rising N MSE_2 / MSE_1 * ()
  vec_N <- mse_data$N %>% unique()
  int_N_unique <- length(vec_N)
  vec_mle <- mse_data$algorithm %>% unique()
  
  df_rates <- data.frame(matrix(nrow = length(vec_N) - 1, ncol = length(vec_mle)))
  rownames(df_rates) <- paste0(vec_N[1:(int_N_unique - 1)], " to ", vec_N[2:int_N_unique])
  colnames(df_rates) <- vec_mle
  
  for(mle in vec_mle){
    
    for(index_N in 1:(int_N_unique - 1)){
      
      df_N1 <- mse_data %>% filter(algorithm == mle, N == vec_N[index_N])
      df_N2 <- mse_data %>% filter(algorithm == mle, N == vec_N[index_N + 1])
      
      df_rates[index_N, mle] <- (df_N2$MSE / df_N1$MSE)
      
    }
    
  }
  
  df_rates
  
}