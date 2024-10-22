library(furrr)
library(tidyverse)

source("Code/DGP class.R")
source("Code/Utils.R")

# Object for Monte Carlo simulation mcs

# Estimator is supposed to be a function that returns at least one value
# of interest and its confidence bands. The value is supposed to be a dataframe
# which can be bound together rowwise.

# Constructor
new_mcs <- function(
  estimator,
  dgp_obj
){
  
  list_mcs <- list(
    estimator = estimator,
    dgp = dgp_obj,
    results = list()
  )
  
  structure(list_mcs, class = "mcs")
  
}

validate_mcs <- function(new_mcs){
  
  # Check that estimator is callable
  if (!is_function(new_mcs$estimator)) {
    stop("'estimator' has to be of class 'function'.", call. = FALSE)
  }
  
  if (class(new_mcs$dgp) != "dgp") {
    stop("'dgp_obj' has to be of class 'dgp'.")
  }
  
}

mcs <- function(estimator, dgp_obj){
  
  mcs_obj <- new_mcs(
    estimator, 
    dgp_obj
  )
  
  validate_mcs(mcs_obj)
  
  mcs_obj
  
}

# Print function
print.mcs <- function(mcs_obj){
  
  if (is_empty(mcs_obj$results)) {
    str_print <- "No results yet. Run simulation first."
  } else {
    str_print <- mcs_obj$results
  }
  
  list_print <- list(
    Estimator = mcs_obj$estimator,
    DGP = mcs_obj$dgp,
    Results = str_print
  )
  
  print(list_print)
  
}

# Run MCS function. Allows for parallel execution
run_simulation.mcs <- function(mcs_obj, seed, samples = NULL, N = NULL, 
                               workers = 1, globals = TRUE, ...){
  
  # Create datasets if not already present
  if (length(mcs_obj$dgp$datasets) == 0) {
    mcs_obj$dgp <- mcs_obj$dgp %>% run_simulation(seed, samples, N)
  }
  
  if (workers > 1) {
    plan(multisession, workers = workers)
  }
  
  pbar <- progress::progress_bar$new(
    format = "Calculating [:bar] :percent eta: :eta",
    total = length(mcs_obj$dgp$datasets),
    clear = FALSE,
  )
  
  list_estimates <- map(mcs_obj$dgp$datasets, function(dataset){
    
    pbar$tick()
    
    list(
      Output = mcs_obj$estimator(dataset$data, ...), 
      N = dataset$N, 
      Sample = dataset$Sample
    )
    
  })
  # Keep estimates in whatever form. This allows for more flexibility
  names(list_estimates) <- mcs_obj$dgp$datasets %>% names()
  mcs_obj$results <- list_estimates
  
  mcs_obj
  
}

# Need a summary method for MCS
summary.mcs <- function(mcs_obj, parameter){
  
  # If more than one sample size is present report statistics by group and aggregated.
  
  # MSE
  
  # Coverage Probabilities
  
  # Summary statistics values. Min, 25%, Mean, Median, 75%, Max, Std. Dev.
  
}