source("Code/Analysis Function.R")

vec_files_analysis <- list.files(
  path = "Results/Data/Final MCS Data/",
  pattern = "\\.RData$"
)

# Check which files are already present in Results
vec_dgps <- c("sparse", "sine", "inter", "neural")
vec_cases <- c("naive", "non_cf", "non_orth", "dml", "not", "rcv")
vec_possible <- paste0(
  rep(vec_dgps, each = length(vec_cases)),
  "_", 
  rep(vec_cases, length(vec_dgps))
  )

regex_possible <- paste0("(", paste0(vec_possible, collapse = "|"), ")")

vec_files_present <- c(
  list.files(path = "Results/Plots/Motivation/"),
  list.files(path = "Results/Plots/Tuning/")
  )

vec_files_present <- vec_files_present %>% 
  str_extract(regex_possible) %>% 
  unique()

vec_files_present <- paste0("mcs_", vec_files_present, ".RData")

vec_files_analysis <- setdiff(vec_files_analysis, vec_files_present)

# Iterate over vector
walk(vec_files_analysis, function(file){
  
  load(paste0("Results/Data/Final MCS Data/", file))
  
  str_name_mcs <- str_remove(file, "\\.RData")
  str_name <- str_remove(str_name_mcs, "^mcs_")
  str_file <- file %>% 
    str_detect(c("(_not|_rcv)")) %>% 
    if_else("Tuning", "Motivation")
  
  lgl_by <- eval(parse(text = str_name_mcs))$Estimates %>% 
    pull(algorithms) %>% 
    unique() %>% 
    length() %>% 
    equals(1)
  
  if (lgl_by) {
    by <- NULL
  } else {
    by <- "algorithms"
  }
  
  run_analysis(
    mcs_obj = eval(parse(text = str_name_mcs)),
    name = str_name,
    file = str_file,
    parameter_names = "theta",
    by = by,
    digits = 3,
    na.rm = TRUE,
    scale = 1.5
  )
  
})
