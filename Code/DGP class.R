library(formula.tools)
library(magrittr)
library(mlr3verse)
library(tidyverse)

setwd("C:/Users/Wilms/OneDrive - uni-bonn.de/Uni Bonn/6. Semester/Masterarbeit/Project/")

source("Code/Utils.R")

# Create an S3 object called dgp

# Constructor
new_dgp <- function(
  formulas = list(
    y ~ beta_0 + theta * d + x %*% beta + e,
    d ~ m(x, gamma, u)
  ),
  ...
){
  
  if (is.null(formulas)) formulas <- list()
  
  list_arguments <- quos(...)
  
  list_dgp <- list(
    formulas = formulas,
    arguments = list_arguments,
    datasets = list()
  )
  
  structure(list_dgp, class = "dgp")
  
}

# Validator checks the validity of the constructors arguments
validate_dgp <- function(new_dgp){
  
  # Check that all supplied formulas are in fact formulas
  if (new_dgp %>% pluck("formulas") %>% every(is_formula) %>% not() & 
      new_dgp %>% pluck("formulas") %>% is.null() %>% not()) {
    stop(
      "All elements of the 'formulas' argument have to be of class formula.",
      call. = FALSE
    )
  }
  
  # Check that all parameters are provided before using the run simulation method.
  # Not possible here, just throw a Note what is missing
  if (new_dgp %>% pluck("formulas") %>% is.null()) { 
    print("You still need to specify formulas for your dgp")
  }
  
  if (new_dgp %>% pluck("arguments") %>% is.null()) {
    print("You still need to provide arguments for your simulation.")
  }
  
}

# Helper function to construct class
dgp <- function(formulas = NULL, ...){
  
  dgp_obj <- new_dgp(
    formulas,
    ...
  )
  
  validate_dgp(dgp_obj)
  
  dgp_obj
  
}

# Then build methods

# Start with a print function
print.dgp <- function(dgp_obj, ...){
  
  str_data <- dgp_obj %$% 
    length(datasets) %>% 
    equals(0) %>% 
    if_else(
      true = "Simulate data first!", 
      false = paste0(length(dgp_obj$datasets), " simulated datasets.")
    )
  
  list_print <- list(
    Formulas = dgp_obj %$% formulas,
    Arguments = dgp_obj %$% map(arguments, rlang::get_expr),
    Datasets = str_data
  )
  
  print(list_print)
}

# Create Generic
add_level <- function(x, ...){
  UseMethod("add_level")
}

# Add Level function. Need to add formula and arguments.
add_level.dgp <- function(dgp_obj, formulas = NULL, ...){
  
  # Append to Formulas
  if (!is.null(formula)) {
    dgp_obj$formulas <- dgp_obj %$% formulas %>% append(formulas)
  }
  
  list_args <- quos(...)
  
  # Check that objects in the Arguments list are uniquely named
  if (list_args %>% names() %>% unique() %>% length() != list_args %>% length()) {
    stop("Arguments are not uniquely named. Remove duplicates from ...!", call. = FALSE)
  }
  
  # Add to arguments
  if (!is.null(list_args)) {
    
    # Check which arguments are already present
    vec_dup <- (dgp_obj$arguments %>% names()) %in% (list_args %>% names())
    
    if (!is_empty(vec_dup)) {
      
      # Drop old elements
      dgp_obj$arguments <- dgp_obj$arguments %>% discard(vec_dup)
      
    }
    
    # Append those elements of list_args that are not duplicates to dgp_obj's Arguments
    dgp_obj$arguments <- dgp_obj$arguments %>% 
      append(list_args)
    
  }
  
  dgp_obj
  
}

# Add arguments function


# Get/Set formula
get_formulas <- function(x){
  UseMethod("get_formulas")
}

get_formulas.dgp <- function(dgp_obj){
  dgp_obj %>% pluck("formulas")
}

set_formulas <- function(x, ...){
  UseMethod("set_formulas")
}

set_formulas.dgp <- function(dgp_obj, level, formula){
  
  if (!is_formula(formula)) { 
    stop("'formula' must be of class formula", call. = FALSE)
  }
  if (dgp_obj %$% formulas %>% length < level) {
    stop("'level' index out of range. Use 'add_level' if you want to add a formula", call. = FALSE)
  }
  dgp_obj$formulas[[level]] <- formula
  
  dgp_obj
}

# Get/Set arguments
get_arguments <- function(x){
  UseMethod("get_arguments")
}

get_arguments.dgp <- function(dgp_obj){
  dgp_obj %$% arguments
}

set_arguments <- function(x, ...){
  UseMethod("set_arguments")
}

set_arguments.dgp <- function(dgp_obj, ...){
  
  # Actually the same as add_level. Without the ability to add a formula
  list_args <- quos(...)
  
  if (is_empty(list_args)) stop("You didn't provide any arguments.", call. = FALSE)
  
  # Check that objects in the Arguments list are uniquely named
  if (list_args %>% names() %>% unique() %>% length() != list_args %>% length()) {
    stop("Arguments are not uniquely named. Remove duplicates from ...!", call. = FALSE)
  }
  
  # Add to arguments
  if (!is.null(list_args)) {
    
    # Check which arguments are already present
    vec_dup <- (dgp_obj$arguments %>% names()) %in% (list_args %>% names())
    
    if (!is_empty(vec_dup)) {
      
      # Drop old elements
      dgp_obj$arguments <- dgp_obj$arguments %>% discard(vec_dup)
      
    }
    
    # Append those elements of list_args that are not duplicates to dgp_obj's Arguments
    dgp_obj$arguments <- dgp_obj$arguments %>% 
      append(list_args)
    
  }
  
  dgp_obj
  
}

# Simulate
run_simulation <- function(x, ...){
  UseMethod("run_simulation")
}

run_simulation.dgp <- function(dgp_obj, seed = 1, samples = 100, N = c(20, 50, 100)){
  
  set.seed(seed)
  
  # Get LHS of formulas
  dep_var <- dgp_obj$formulas %>% lhs()
  
  # Create a grid of samples and sample sizes
  df_grid <- expand.grid(iteration = seq_len(samples), N = N)
  
  # Simulation part
  list_data <- pmap(df_grid, function(iteration, N){
    # Get arguments from dgp into environment
    walk(names(dgp_obj$arguments), ~ {
      assign(
        .x, 
        rlang::get_expr(
          dgp_obj$arguments[[.x]]
        ) %>% eval(),
        envir = parent.env(environment())
      )
    })
    
    # Assume bottom up structure for now
    walk(dgp_obj %$% formulas[length(formulas):1], ~ {
      
      # Get arguments of function
      dep <- lhs(.x)
      fun_indp <- rhs(.x)
      
      # Evaluate
      assign(
        "dependent", 
        fun_indp %>% eval(), 
        environment()
        )
      
      # If the output is a matrix object or dataframe convert to list and
      # put into environment
      if (
        (is.matrix(dependent %>% eval()) | 
        is.data.frame(dependent %>% eval())) && 
        (dependent %>% eval() %>% ncol() > 1)
        ) {
        df_dep <- data.frame(dependent %>% eval())
        assign("dependent", map(df_dep, ~ .x), environment())
      }
      
      if (dependent %>% eval() %>% is_list()) {
        list2env(dependent %>% eval, env = parent.env(environment()))
      } else {
        assign(dep %>% as.character(), dependent, envir = parent.env(environment()))
      }
      
    })
    
    # Construct a vector returning results
    # Find variables that have the same number of entries and add them to the df.
    ns_data <- ls()[map_lgl(ls(), ~ parse(text = .x) %>% eval() %>% is_data(N))]
    
    list_data <- ns_data %>% map(~ {
      x <- parse(text = .x) %>% eval()
    })
    
    df_data <- do.call(cbind.data.frame, list_data %>% set_names(ns_data))
    list(data = df_data, N = N, Sample = iteration)
    
  }) %>% 
    set_names(paste0("Sample = ", df_grid$iteration, " with N = ", df_grid$N))
  
  dgp_obj$datasets <- list_data
  
  dgp_obj
  
}

get_datasets <- function(x, ...){
  UseMethod("get_datasets")
}

get_datasets.dgp <- function(dgp_obj, index = NULL){
  
  if (is.null(index)) { 
    dgp_obj$datasets
  } else {
    dgp_obj$datasets[index]
    }
  
}
