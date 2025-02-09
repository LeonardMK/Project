library(furrr)
library(magrittr)
library(tidyverse)

source("Code/Utils.R")

# Function for Monte Carlo estimation

# Function to get Estimates and Standard deviation
get_estimates <- function(x, ...){
  UseMethod("get_estimates")
}

# Combines the Output from run_simulation into a dataframe
get_estimates.mcs <- function(mcs_obj){
  
  if (is_empty(mcs_obj$results)) stop("Run Monte Carlo simulation first.", call. = FALSE)
  
  df_results <- map(mcs_obj$results, ~ {
    
    if (is_vector(.x$Output$Estimates)) {
      mcs_result <- c(.x$Output$Estimates, N = .x$N, Sample = .x$Sample)
    } else if(is.matrix(.x$Output$Estimates) | is.data.frame(x)){
      mcs_result <- .x$Output$Estimates
      mcs_result$N <- .x$N
      mcs_result$Sample
    }
    
  }) %>% 
    map_dfr(~ .x)
  
  df_results
  
}

# Mean Squared Error, Bias, and Variance estimatior.
# Calculates MSE for all N and Sample selected. If values are NULL calculate for everything
mse <- function(
  mcs_obj, parameter_names = "theta", N = NULL, Samples = NULL, na.rm = TRUE, by = NULL
){
  
  vec_N <- N
  by <- syms(by)
  
  if ("mcs" %in% class(mcs_obj)) {
    df_results <- get_estimates(mcs_obj)
    
    df_results$parameter <- get_parameter(mcs_obj, parameter_names)
    df_results$parameter_names <- parameter_names
  } else if ("data.frame" %in% class(mcs_obj)){
    df_results <- mcs_obj
  }
  
  if (!all(N %in% df_results$N)) {
    stop("Sample sizes provided through 'N' are not present in this simulation", call. = FALSE)
  }
  
  if (!all(Samples %in% df_results$Sample)) {
    stop("Sample indices provided through 'Samples' have to be present in this simulation.", call. = FALSE)
  }
  
  if (is.null(N)) vec_N <- df_results$N %>% unique()
  
  if (is.null(Samples)) Samples <- df_results$Sample %>% unique()
  
  # Calculate groupwise and overall
  df_results <- df_results %>% 
    filter(N %in% vec_N, Sample %in% Samples) %>% 
    mutate(
      sq_err = (parameter - parameter_est) ^ 2,
      bias = parameter_est - parameter
    )
  
  if (length(vec_N) > 1) {
    
    suppressMessages({df_mse <- df_results %>% 
      group_by(N, parameter_names, !!!by) %>% 
      summarise(
        `Mean` = mean(parameter_est),
        MSE = mean(sq_err, na.rm = na.rm),
        `Squared Bias` = mean(bias) ^ 2,
        Variance = var(parameter_est),
        `Mean Bias` = mean(bias, na.rm = na.rm)
      )})
    
  } else {
    
    df_mse <- df_results %>% 
      group_by(parameter_names, !!!by) %>% 
      summarise(
        MSE = mean(sq_err, na.rm = na.rm),
        `Squared Bias` = mean(bias, na.rm = na.rm) ^ 2,
        Variance = var(parameter_est, na.rm = na.rm),
        `Mean Bias` = mean(bias, na.rm = na.rm)
      )
    
  }
  
  df_mse
  
}

# Consistency Function that gives P(|theta - ^theta| < epsilon)
consistency <- function(
  mcs_obj, epsilon = seq(0.01, 1, length.out = 500),  
  parameter_names = "theta", N = NULL, Samples = NULL, plot = TRUE,
  na.rm = TRUE, by = NULL
){
  
  vec_N <- N
  
  if ("mcs" %in% class(mcs_obj)) {
    df_results <- get_estimates(mcs_obj)
    
    df_results$parameter <- get_parameter(mcs_obj, parameter_names)
    df_results$parameter_names <- parameter_names
  } else if ("data.frame" %in% class(mcs_obj)){
    df_results <- mcs_obj
  }
  
  # Check if results are present
  if (!all(N %in% df_results$N)) {
    stop("Sample sizes provided through 'N' are not present in this simulation", call. = FALSE)
  }
  
  if (!all(Samples %in% df_results$Sample)) {
    stop("Sample indices provided through 'Samples' have to be present in this simulation.", call. = FALSE)
  }
  
  if (is.null(N)) vec_N <- df_results$N %>% unique()
  
  if (is.null(Samples)) Samples <- df_results$Sample %>% unique()
  
  if (length(parameter_names) == 1 & !is.null(by)) {
    gg_by <- reformulate(by)
  } else if (length(parameter_names) > 1){
    gg_by <- reformulate("parameter_names", by)
  } else if (is.null(by)) {
    gg_by <- reformulate("parameter_names")
  }
  
  by <- syms(by)
  
  df_results <- df_results %>% 
    filter(N %in% N, Sample %in% Samples) %>% 
    mutate(
      abs_bias = abs(parameter_est - parameter)
    )
  
  # Go along epsilon and check whether abs bias exceeds epsilon
  df_conv <- map(epsilon, function(eps){
    
    suppressMessages({
      df_results_eps <- df_results %>% 
        mutate(lgl_eps = abs_bias < eps) %>% 
        group_by(N, parameter_names, !!!by) %>% 
        summarise(Probability = mean(lgl_eps), na.rm = na.rm) %>% 
        mutate(
          epsilon = eps,
          N = as.factor(N),
          parameter_names = str_to_title(parameter_names),
        )
      
      if (!is_empty(by)) {
        df_results_eps %>% 
          mutate(
            across(!!!by, str_to_title)
          )
      }
      
      df_results_eps
      
    })
    
  }) %>% map_dfr(~ .x)
  
  if (plot){
    gg_conv_plot <- ggplot(df_conv, aes(x = epsilon, y = Probability, col = N)) + 
      geom_point() + 
      geom_line() +
      labs(x = "Epsilon", y = "Probability") +
      facet_wrap(gg_by) + 
      theme_bw()
    
    list(data = df_conv, plot = gg_conv_plot)
    
  } else {
    
    list(data = df_conv)
    
  }
  
}

# Asymptotic Normality function
# Form for every estimate z_score = sqrt(n) * (theta_hat - theta) / sd(theta_hat)
# Then calculate on a grid mean(z_score <= z)
# Plot this against the cdf of a standard normal.
asymptotic_normal <- function(
  mcs_obj, 
  parameter_names = "theta",
  prob_range = seq(0, 1, length.out = 100), 
  N = NULL, Samples = NULL, plot = TRUE, na.rm = TRUE, by = NULL
){
  
  vec_N <- N
  
  if ("mcs" %in% class(mcs_obj)) {
    df_results <- get_estimates(mcs_obj)
    
    df_results$parameter <- get_parameter(mcs_obj, parameter_names)
    df_results$parameter_names <- parameter_names
  } else if ("data.frame" %in% class(mcs_obj)){
    df_results <- mcs_obj
  }
  
  # Check if results are present
  if (!all(N %in% df_results$N)) {
    stop("Sample sizes provided through 'N' are not present in this simulation", call. = FALSE)
  }
  
  if (!all(Samples %in% df_results$Sample)) {
    stop("Sample indices provided through 'Samples' have to be present in this simulation.", call. = FALSE)
  }
  
  if (is.null(N)) vec_N <- df_results$N %>% unique()
  
  if (is.null(Samples)) Samples <- df_results$Sample %>% unique()
  
  if (length(parameter_names) == 1 & !is.null(by)) {
    gg_by <- reformulate(by)
  } else if (length(parameter_names) > 1){
    gg_by <- reformulate("parameter_names", by)
  } else if (is.null(by)) {
    gg_by <- reformulate("parameter_names")
  }
  
  by <- syms(by)
  
  # Calculate z_score
  df_results <- df_results %>% 
    filter(N %in% N, Sample %in% Samples) %>% 
    mutate(z_score = (parameter_est - parameter) / sd)
  
  # Check that p in prob_range is in (0, 1)
  if(prob_range %>% between(0, 1) %>% all() %>% not()){
    stop("'prob_range' elements have to be between 0 and 1", call. = FALSE)
  }
  
  # Drop prob_range values of 0 and 1
  prob_range <- prob_range[prob_range %in% c(0, 1) %>% not()]
  
  # Loop over values of standard normal distribution
  # If df is not NA for all entries take values from t-distribution
  # TODO: Facetting by parameter names for dof plot
  if (all(!is.na(df_results$df))){
    
    vec_df <- df_results %>% pull(df) %>% unique() %>% as.numeric()
    
    # Get range of t-values as df
    df_dof <- vec_df %>% 
      map(~ {
        qt(prob_range, .x)
      }) %>% 
      set_names(vec_df) %>% 
      map_dfc(~ .x) %>% 
      mutate(cdf = prob_range) %>% 
      pivot_longer(cols = !cdf, names_to = "df", values_to = "t_value")
    
    # Calculate Probabilities
    vec_prob <- map(vec_df, function(dof){
      
      # Select relevant parts of the dataframe
      df_res_dof <- df_results %>% filter(df == dof)
      
      # Calculate for every t-score the logical values and then the mean
      df_dof %>% 
        filter(df == dof) %>% 
        pull(t_value) %>% 
        map_dbl(
          function(t_value){
            df_res_dof$z_score %>% 
              is_weakly_less_than(t_value) %>% 
              mean(na.rm = na.rm)
          }
        ) %>% 
        set_names(
          df_dof %>% 
            filter(df == dof) %>% 
            pull(t_value)
        )
      
    }) %>% 
      unlist()
    
    # Transform to a dataframe
    df_prob <- data.frame(
      emp_prob = vec_prob, 
      t_value = as.numeric(names(vec_prob)),
      cdf = df_dof %>% arrange(desc(df)) %>% pull(cdf),
      df = rep(vec_df, each = length(prob_range)),
      N = rep(df_results %>% pull(N) %>% unique(), each = length(prob_range))
    ) %>% 
      mutate(
        facet_label = paste0(N, " Observations with ", df, " DF")
      )
    
    # Need a plot for every degree of freedom
    if (plot) {
      
      cdf_plot <- ggplot(df_prob, aes(x = t_value, y = emp_prob)) + 
        geom_point() + 
        geom_line(mapping = aes(t_value, cdf), col = "black") +
        facet_wrap(~ facet_label + parameter_names) + 
        labs(x = "T-Value", y = "Probability") + 
        theme_bw()
      
      list(data = df_prob %>% select(N, df, t_value, emp_prob, cdf))
      
    } else {
      
      list(data = df_prob)
      
    }
    
  } else {
    
    vec_z_val <- qnorm(prob_range)
    
    # Calculate probabilities by N
    df_prob <- vec_z_val %>% 
      map(~ {
        df_results$z_score < .x
      }) %>% 
      map_dfc(~ .x) %>% 
      set_colnames(as.character(vec_z_val)) %>% 
      cbind.data.frame(df_results %>% select(N, parameter_names, !!!by)) %>% 
      group_by(N, parameter_names, !!!by) %>% 
      summarise(
        across(.fns = mean, na.rm = na.rm)
      ) %>% 
      pivot_longer(cols = as.character(vec_z_val), names_to = "z_value", values_to = "emp_prob") %>% 
      mutate(
        z_value = as.numeric(z_value),
        N = as.factor(N),
        parameter_names = str_to_title(parameter_names)
      )
    
    # For some weird reason the following code doesn't work inside mutate
    df_prob$cdf <- rep(prob_range, nrow(df_prob) / length(prob_range))
    
    if (!is_empty(by)) {
      df_prob <- df_prob %>% 
        mutate(across(!!!by, str_to_title))
    }
    
    if (plot) {
      
      cdf_plot <- ggplot(df_prob, aes(x = z_value, y = emp_prob, col = N)) + 
        geom_point() + 
        geom_line(mapping = aes(z_value, cdf), col = "black") +
        labs(x = "Z-Value", y = "Probability") + 
        facet_wrap(gg_by) +
        theme_bw()
      
      list(data = df_prob, plot = cdf_plot)
      
    } else {
      
      list(data = df_prob)
      
    }
    
  }
  
}

# Compares the distribution of the estimator to the one of the normal
distribution <- function(
  mcs_obj, 
  parameter_names = "theta",
  N = NULL, Samples = NULL,
  plot = TRUE,
  int_grid = 100,
  na.rm = TRUE,
  by = NULL
){
  
  vec_N <- N
  
  if ("mcs" %in% class(mcs_obj)) {
    df_results <- get_estimates(mcs_obj)
    
    df_results$parameter <- get_parameter(mcs_obj, parameter_names)
    df_results$parameter_names <- parameter_names
  } else if ("data.frame" %in% class(mcs_obj)){
    df_results <- mcs_obj
  }
  
  # Check if results are present
  if (!all(N %in% df_results$N)) {
    stop("Sample sizes provided through 'N' are not present in this simulation", call. = FALSE)
  }
  
  if (!all(Samples %in% df_results$Sample)) {
    stop("Sample indices provided through 'Samples' have to be present in this simulation.", call. = FALSE)
  }
  
  if (is.null(N)) vec_N <- df_results$N %>% unique()
  
  if (is.null(Samples)) Samples <- df_results$Sample %>% unique()
  
  if (length(parameter_names) == 1 & !is.null(by)) {
    gg_by <- reformulate(by, "N")
  } else if (length(parameter_names) > 1){
    gg_by <- reformulate("parameter_names", by, "N")
  } else if (is.null(by)) {
    gg_by <- reformulate("parameter_names", "N")
  }
  
  by <- syms(by)
  
  # Calculate Distribution moments of Estimator
  df_moments <- df_results %>% 
    filter(N %in% vec_N, Sample %in% Samples) %>% 
    group_by(N, parameter_names, !!!by) %>% 
    summarise(
      mean = mean(parameter_est, na.rm = na.rm),
      sd = sd(parameter_est, na.rm = na.rm),
      min = min(parameter_est, na.rm = na.rm),
      max = max(parameter_est, na.rm = na.rm)
    )
  
  # Calculate grid starting and ending points
  vec_abs_max <- (abs(df_moments$min) >= abs(df_moments$max)) %>% 
    if_else(df_moments$min, df_moments$max) %>% 
    abs()
  
  vec_min <- qnorm(0.001, mean = df_moments$mean, sd = df_moments$sd)
  vec_max <- qnorm(0.999, mean = df_moments$mean, sd = df_moments$sd)
  df_moments$start <- vec_min
  df_moments$end <- vec_max
  
  index_by <- 3:(str_which(colnames(df_moments), "mean") - 1)
  names_by <- names(df_moments)[index_by]
  
  # Calculate densities
  df_seq <- do.call(rbind.data.frame, rmap(df_moments, function(col){
    
    # Since parameter names is a character everything in the vector is 
    # turned into a string
    names(col) <- colnames(df_moments)
    vec_seq <- seq(as.numeric(col["start"]), as.numeric(col["end"]), length.out = int_grid)
    
    vec_seq_pdf <- dnorm(vec_seq, mean = as.numeric(col["mean"]), sd = as.numeric(col["sd"]))
    
    df_sub_seq <- data.frame(
      x = vec_seq, 
      pdf = vec_seq_pdf, 
      parameter_names = col["parameter_names"], 
      N = col["N"],
      row.names = NULL
    )
    
    if (!is_empty(by)) {
      
      df_by <- map(names_by, ~ {
        vec_by <- rep(col[.x], nrow(df_sub_seq))
        names(vec_by) <- NULL
        vec_by
      }) %>% 
        set_names(names_by) %>% 
        map_df(~ .x)
      
      cbind(df_sub_seq, df_by)
      
    } else {
      
      df_sub_seq
      
    }
    
  }))
  
  df_seq$N <- trimws(df_seq$N)
  df_seq$N <- factor(df_seq$N, levels = sort(vec_N))
  df_results$N <- factor(df_results$N, levels = sort(vec_N))
  
  # Plot is facetted by N
  if (plot) {
    
    gg_dist <- ggplot(df_results) + 
      geom_histogram(aes(x = parameter_est, y = ..density..), bins = 50) + 
      geom_line(aes(x = x, y = pdf), data = df_seq) +
      geom_vline(xintercept = df_results %>% pull(parameter) %>% unique(), col = "red") +
      labs(y = "Density", x = "Distribution") +
      facet_grid(gg_by, labeller = label_wrap_gen(width = 10), scales = "free")
    
    list(data = df_results, normality = df_seq, plot = gg_dist)
    
  } else {
    list(data = df_results, normality = df_seq)
  }
}

# Coverage Probabilities
# cov probability function should be able to take several mcs objects.
# Just repeat cov_prob. Need to add the specifications.
# Specifications are not stored inside the estimator function. 
# Have to supply them manually.
cov_prob <- function(mcs_obj, parameter_names = "theta", alpha = c(0.1, 0.05, 0.01), 
                         N = NULL, Samples = NULL, plot = TRUE, na.rm = TRUE, by = NULL){
  
  vec_N <- N
  
  if ("mcs" %in% class(mcs_obj)) {
    df_results <- get_estimates(mcs_obj)
    
    df_results$parameter <- get_parameter(mcs_obj, parameter_names)
    df_results$parameter_names <- parameter_names
  } else if ("data.frame" %in% class(mcs_obj)){
    df_results <- mcs_obj
  }
  
  # Check if results are present
  if (!all(N %in% df_results$N)) {
    stop("Sample sizes provided through 'N' are not present in this simulation", call. = FALSE)
  }
  
  if (!all(Samples %in% df_results$Sample)) {
    stop("Sample indices provided through 'Samples' have to be present in this simulation.", call. = FALSE)
  }
  
  if (is.null(N)) vec_N <- df_results$N %>% unique()
  
  if (is.null(Samples)) Samples <- df_results$Sample %>% unique()
  
  by <- syms(by)
  
  # Create dataframe of critical values
  df_results <- matrix(alpha, nrow(df_results), length(alpha), byrow = TRUE) %>% 
    as.data.frame() %>% 
    set_names(alpha) %>% 
    cbind(df_results) %>% 
    mutate(
      across(
        as.character(alpha), 
        ~ if_else(is.na(df), qnorm(1 - .x / 2), qt(1 - .x / 2, df))
      )
    )
  
  # Create empty matrix of lower and upper
  df_lower <- df_results$parameter_est - df_results$sd * df_results[, as.character(alpha)]
  df_upper <- df_results$parameter_est + df_results$sd * df_results[, as.character(alpha)]
  df_theta_in <- (df_results$parameter > df_lower) & (df_results$parameter < df_upper) %>% 
    as.data.frame() %>% 
    set_names(paste0("theta_in_", 100 * (1 - alpha), "%_ci"))
  df_width_ci <- (df_upper - df_lower) %>% 
    set_names(paste0("width_", 100 * (1 - alpha), "%_ci"))
  df_cov_prob <- cbind.data.frame(
    df_results %>% select(!!!by, N),
    df_theta_in,
    df_width_ci
  ) %>% 
    group_by(N, !!!by) %>% 
    summarise(
      across(starts_with("theta_in_"), ~ mean(.x, na.rm = na.rm)),
      across(starts_with("width_"), ~ mean(.x, na.rm = na.rm))
    ) %>% 
    rename_with(
      ~ paste0("Prob. Theta in ", 100 * (1 - alpha), "% CI"), 
      starts_with("theta_in_")
    ) %>% 
    rename_with(
      ~ paste0("Width of ", 100 * (1 - alpha), "% CI"),
      starts_with("width_")
    )
  
  # Need df_cov_prob in long format.
  # Create two intermediate dataframes
  df_cov_prob_1 <- df_cov_prob %>% 
    pivot_longer(
      cols = starts_with("Prob. Theta in"), 
      names_to = "Type of CI", 
      values_to = "Cov. Prob."
    )
  
  df_cov_prob_2 <- df_cov_prob %>% 
    pivot_longer(
      cols = starts_with("Width of "), 
      names_to = "Type of CI", 
      values_to = "Width of CI"
    )
  
  df_n_samples <- df_results %>% 
    group_by(N) %>% 
    summarise(n()) %>% 
    rename("n_obs" = "n()")
  
  df_cov_prob <- cbind(
    df_cov_prob_1 %>% select(!matches("^Width of ..% CI")),
    `Width of CI` = df_cov_prob_2 %>% pull("Width of CI")
  ) %>% 
    mutate(
      `Type of CI` = str_remove(`Type of CI`, "Prob\\. Theta in "),
      `Lower 95%` = 100 * (`Cov. Prob.` + qnorm(0.025) * sqrt(`Cov. Prob.` * (1 - `Cov. Prob.`) / (df_n_samples %>% filter(N == N) %>% pull(n_obs)))),
      `Upper 95%` = 100 * (`Cov. Prob.` + qnorm(0.975) * sqrt(`Cov. Prob.` * (1 - `Cov. Prob.`) / (df_n_samples %>% filter(N == N) %>% pull(n_obs)))),
      `Cov. Prob.` = 100 * `Cov. Prob.`,
      N = as.factor(N)
    ) %>% 
    mutate(
      `Upper 95%` = case_when(
        `Upper 95%` > 100 ~ 100,
        TRUE ~ `Upper 95%`
      ),
      `Lower 95%` = case_when(
        `Lower 95%` < 0 ~ 0,
        TRUE ~ `Lower 95%`
      )
    )
  
  if (plot) {
    
    if (!is_empty(by)) {
      
      cov_prob_plot <- ggplot(
        data = df_cov_prob, 
        mapping = aes(x = N, y = `Cov. Prob.`, col = `Width of CI`, shape = `Type of CI`)) + 
        geom_point(alpha = 0.3) + 
        geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`)) + 
        geom_hline(aes(yintercept = as.numeric(str_remove(`Type of CI`, "% CI$"))), linetype = "dashed") + 
        labs(y = "Coverage Probability", x = "Sample Size", shape = str_to_title(as.character(by[[1]]))) + 
        theme_bw() + 
        scale_color_continuous(type = "viridis") +  
        facet_grid(eval(by[[1]]) ~ `Type of CI`, labeller = label_wrap_gen(width = 10))
      
    } else {
      
      cov_prob_plot <- ggplot(
        data = df_cov_prob, 
        mapping = aes(x = N, y = `Cov. Prob.`, col = `Width of CI`)) + 
        geom_point(alpha = 0.3) + 
        geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`)) + 
        geom_hline(aes(yintercept = as.numeric(str_remove(`Type of CI`, "% CI$"))), linetype = "dashed") + 
        labs(y = "Coverage Probability", x = "Sample Size") + 
        theme_bw() + 
        scale_color_continuous(type = "viridis") + 
        facet_grid(~ `Type of CI`)
      
    }
    
    list(data = df_cov_prob, plot = cov_prob_plot)
    
  } else {
    
    list(data = df_cov_prob)
    
  }
  
}

# Summary statistics

# Need Methods to compare mcs objects.

merge.mcs <- function(...){
  
  list_mcs <- list(...)
  
  # Check that they are all of class mcs
  if (list_mcs %>% map_lgl(~ class(.x) == "mcs") %>% all() %>% not()) {
    stop("Can only merge objects of class 'mcs'.")
  }
  
}