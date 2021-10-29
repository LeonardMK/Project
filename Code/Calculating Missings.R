library(tidyverse)

vec_mle_unique <- c(
  "G: glmnet M: glmnet", "G: xgboost M: xgboost", "G: ranger M: ranger",  
  "G: rpart M: rpart", "G: kknn M: kknn", "G: nnet M: nnet", "Best"
)
vec_samples <- 1:200
vec_N <- c(50, 100, 400)

vec_cases <- c("not", "rcv")
vec_files <- list.files(
  path = "Results/Data/Final MCS Data/", 
  pattern = "not|rcv")

df_N_MLE <- expand.grid(int_N = vec_N, str_algorithms = vec_mle_unique)

list_missing_samples <- map(vec_files, function(file_path){
  
  load(paste0("Results/Data/Final MCS Data/", file_path))
  
  assign("mcs_obj", eval(parse(text = str_remove(file_path, "\\.RData"))))
  
  list_samples_absent <- pmap(df_N_MLE, function(int_N, str_algorithms){
    
    vec_samples_present <- mcs_obj$Estimates %>% 
      filter(N == int_N, algorithms == str_algorithms) %>% 
      pull(Sample)
    
    vec_index <- setdiff(vec_samples, vec_samples_present)
    
    if (is_empty(vec_index)) {
      NULL
    } else {
      list(
        index = vec_index,
        N = int_N,
        ml_g = str_extract(str_algorithms, "(?<=^G: ).*(?= M: .*$)"),
        ml_m = str_extract(str_algorithms, "(?<= M: ).*")
      )
    }
  })
  
  names(list_samples_absent) <- paste0(df_N_MLE$int_N, 
                                       "; ", 
                                       df_N_MLE$str_algorithms)
  
  list_samples_absent %>% compact()
  
})

names(list_missing_samples) <- str_remove(vec_files, "\\.RData")

# Function to estimate missings
compute_missings <- function(list_indices, dgp, ...){
  browser()
  list_args <- list(...)
  list_mcs_obj <- map(list_indices, function(index){
    browser()
    int_N <- index$N
    vec_index_samples <- index$index
    str_ml_g <- paste0("regr.", index$ml_g)
    str_ml_m <- paste0("classif.", index$ml_m)
    
    # Create mcs
    dgp_subset <- dgp %>% subset(N = int_N, Samples = vec_index_samples)
    mcs_obj <- mcs(dml_estimator, dgp_subset)
    mcs_obj <- exec(
      .fn = run_simulation,
      mcs_obj,
      ml_g = str_ml_g,
      ml_m = str_ml_m,
      list_args
      )
    
    mcs_to_df(mcs_obj)
    
  })
  
  # Bind all results together
  df_estimates <- map_dfr(list_mcs_obj, ~ .x$Estimates)
  df_measures <- map_dfr(list_mcs_obj, ~ .x$Measures)
  
  list(
    Estimates = df_estimates,
    Measures = df_measures
  )
  
}
