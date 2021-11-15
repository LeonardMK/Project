library(DoubleML)
library(mlr3verse)
library(tidyverse)

source("Code/Monte Carlo class.R")
source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# Remove Settings
vec_files <- str_subset(list.files("Results/Data/"), "^mcs_.*\\.RData$")

vec_files_small <- str_subset(
  list.files("Results/Data/MCS Size Efficient/"),
  "^mcs_.*\\.RData$"
)

vec_files <- setdiff(
  str_remove(vec_files, "\\.RData"), 
  str_remove(vec_files_small, "\\.RData")
)

# Iterate over and remove Tuning Results in Settings. Save in new folder
vec_files %>% walk(~ {
  
  str_path <- paste0("Results/Data/", .x, ".RData")
  
  if (0.000001 * file.size(str_path) < 0.5 * memory.size()) {
    
    load(str_path)
    
    # Detect monte carlo simulation object in namespace
    str_mcs_obj <- str_subset(ls(), "^mcs_.*")
    
    if (length(str_mcs_obj) > 1) {
      vec_lgl <- map_lgl(str_mcs_obj, ~ class(eval(parse(text = .x))) == "mcs")
      str_mcs_obj[vec_lgl]
    }
    
    assign(eval(.x), mcs_to_df(eval(parse(text = eval(str_mcs_obj)))))
    
    save(
      list = .x, 
      file = paste0("Results/Data/MCS Size Efficient/", .x, ".RData")
    )
    
  }
  
})

# Merge Datasets
vec_files <- list.files("Results/Data/MCS Size Efficient/")
vec_files_small <- str_subset(vec_files, "_small\\.RData$")
vec_files_big <- str_subset(vec_files, "_big\\.RData$")

vec_files_unique <- str_remove(vec_files, "(|_big|_small)\\.RData") %>% 
  unique()

vec_files_unique %>% walk(~ {
  # browser()
  # Check if present
  str_small <- str_subset(vec_files_small, .x)
  str_big <- str_subset(vec_files_big, .x)
  
  df_estimates <- data.frame()
  df_measures <- data.frame()
  
  if (!is_empty(str_small)) {
    
    str_path_small <- paste0("Results/Data/MCS Size Efficient/", str_small)
    
    load(str_path_small)
    
    list_small <- eval(parse(text = str_remove(str_small, "\\.RData")))
    df_estimates <- df_estimates %>% rbind(list_small$Estimates)
    df_measures <- df_measures %>% rbind(list_small$Measures)
    
  }
  
  if (!is_empty(str_big)) {
    
    str_path_big <- paste0("Results/Data/MCS Size Efficient/", str_big)
    
    load(str_path_big)
    
    list_big <- eval(parse(text = str_remove(str_big, "\\.RData")))
    df_estimates <- df_estimates %>% rbind(list_big$Estimates)
    df_measures <- df_measures %>% rbind(list_big$Measures)
    
  }
  
  if (is_empty(str_small) & is_empty(str_big)) {
    
    str_path <- paste0("Results/Data/MCS Size Efficient/", .x, ".RData")
    
    load(str_path)
    
    
    list_mcs_obj <- eval(parse(text = .x))
    df_estimates <- df_estimates %>% rbind(list_mcs_obj$Estimates)
    df_measures <- df_measures %>% rbind(list_mcs_obj$Measures)
    
  }
  
  # Remove Duplicates
  df_estimates <- distinct(df_estimates)
  df_measures <- distinct(df_measures)
  
  # Turn around bias in eta_0
  df_measures$bias <- - df_measures$bias
  
  # In case tuning is applied an extra entry is needed for best and the original learner
  if ("algorithms" %in% colnames(df_estimates)) {
    
    df_estimates_best <- df_estimates %>% 
      filter(algorithms == "Best", 
             str_remove(ml_g, "^regr\\.") == str_remove(ml_m, "^classif\\.")) %>% 
      mutate(algorithms = paste0("G: ", 
                                 str_remove(ml_g, "^regr\\."), 
                                 " M: ", 
                                 str_remove(ml_m, "^classif\\.")))
    
    df_estimates <- rbind(df_estimates, df_estimates_best) %>% 
      arrange(N, Sample, algorithms) %>% 
      rename("Algorithms" = "algorithms") %>% 
      mutate(Algorithms = case_when(
        Algorithms == "Best" ~ "Best Measure",
        Algorithms == "G: glmnet M: glmnet" ~ "E-Net",
        Algorithms == "G: xgboost M: xgboost" ~ "SGBM",
        Algorithms == "G: ranger M: ranger" ~ "RF",
        Algorithms == "G: rpart M: rpart" ~ "CART",
        Algorithms == "G: kknn M: kknn" ~ "K-KNN",
        Algorithms == "G: nnet M: nnet" ~ "N-Net"
      ))
    
  }
  
  list_mcs_results <- list(Estimates = df_estimates, Measures = df_measures)
  
  # Export
  assign(eval(.x), list_mcs_results)
  save(
    list = .x, 
    file = paste0("Results/Data/Final MCS Data/", .x, ".RData")
  )
  
})
