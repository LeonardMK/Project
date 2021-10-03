library(DoubleML)
library(mlr3verse)
library(tidyverse)

# Remove Settings
vec_files <- str_subset(list.files("Results/Data/"), "^mcs_.*\\.RData$")

vec_files_small <- str_subset(
  list.files("Results/Data/MCS without Tuning Results/"),
  "^mcs_.*\\.RData$"
  )

vec_files <- setdiff(vec_files, vec_files_small)

# Iterate over and remove Tuning Results in Settings. Save in new folder
vec_files %>% map(~ {
  
  str_path <- paste0("Results/Data/", .x)
  load(str_path)
  
  # Detect monte carlo simulation object in namespace
  str_mcs_obj <- str_subset(ls(), "^mcs_.*")
  
  if (length(str_mcs_obj) > 1) {
    vec_lgl <- map_lgl(str_mcs_obj, ~ class(eval(parse(text = .x))) == "mcs")
    str_mcs_obj <- str_mcs_obj[vec_lgl]
  }
  
  mcs_obj <- eval(parse(text = str_mcs_obj))
  rm(list = str_mcs_obj)
  mcs_obj$results <- map(mcs_obj$results, ~ {
    .x$Output$Settings[[1]]$`Tuning Results` <- NULL
    .x$Output$Settings[[1]] %>% compact()
  })
  
  str_name <- eval(.x) %>% str_remove("\\.RData$")
  assign(eval(str_name), mcs_obj)
  rm(mcs_obj)
  save(list = str_name, file = paste0("Results/Data/MCS without Tuning Results/", .x))
  rm(list = str_name)
  
})

mcs_sparse_non_cf <- map(mcs_sparse_non_cf$results, ~ {
  .x$Output$Settings[[1]]$`Tuning Results` <- NULL
  .x
})