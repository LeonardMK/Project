merge.mcs <- function(mcs_obj_1, mcs_obj_2) {
  
  if (mcs_obj_1$estimator != mcs_obj_2$estimator) {
    stop("Estimators have to be equal to merge mcs objects.", call. = FALSE)
  }
  
  vec_names_results_1 <- names(mcs_obj_1$results)
  vec_names_results_2 <- names(mcs_obj_2$results)
  vec_names_intersect <- intersect(vec_names_results_1, vec_names_results_2)
  
  if () 
  
  if ()
  
  list_results <- mcs_obj_1$results %>% 
    append(mcs_obj_2$results)
  
}