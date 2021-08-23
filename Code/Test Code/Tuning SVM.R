library(mlr3)
library(paradox)
library(data.table)

test_data <- test$datasets$`Sample = 1 with N = 100`$data %>% 
  mutate(D = as.factor(D))
task_regr <- TaskRegr$new("y_col", test_data, target = "Y")
task_classif <- TaskClassif$new("d_col", test_data, target = "D")

# Set feature roles
task_regr$select(c("X.1", "X.2"))
task_classif$select(c("X.1", "X.2"))

search_space = ps(
  cost = p_dbl(lower = 0, upper = 10000),
  kernel = p_fct(levels = c("linear", "polynomial", "radial", "sigmoid")),
  gamma = p_dbl(lower = 0, upper = 10, depends = kernel == "radial"),
  degree = p_int(lower = 1, upper = 5, depends = kernel == "polynomial")
)
terminator = trm("evals", n_evals = 30)
instance_classif = TuningInstanceSingleCrit$new(
  task = task_classif,
  learner = lrn("classif.svm", type = "C-classification"),
  resampling = rsmp("cv"),
  measure = msr("classif.ce"),
  search_space = search_space,
  terminator = terminator
)
tt$optimize(instance_classif)

instance_regr = TuningInstanceSingleCrit$new(
  task = task_regr,
  learner = lrn("regr.svm", type = "eps-regression"),
  resampling = rsmp("cv"),
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = terminator
)
design = data.table(kernel = "radial")
tt$optimize(instance_regr)
instance_regr

# Now create a dml model
list_tune_settings <- list(
  terminator = trm("evals", n_evals = 5),
  algorithm = tnr("random_search"),
  rsmp_tune = rsmp("cv", folds = 5),
  measure = list(ml_g = "regr.mse", ml_m = "classif.logloss")
)
dml_data <- DoubleMLData$new(
  test_data,
  y_col = "Y",
  x_cols = c("X.1", "X.2"),
  d_cols = "D"
)
dml_est <- DoubleMLPLR$new(
  data = dml_data, 
  ml_g = lrn("regr.svm", type = "eps-regression"),
  ml_m = lrn("classif.svm", type = "C-classification"), 
  n_folds = 3,
  n_rep = 1
  )
dml_est$tune(
  param_set = list_svm,
  tune_settings = list_tune_settings,
  tune_on_folds = TRUE
)
dml_est$fit()
dml_est$summary()
