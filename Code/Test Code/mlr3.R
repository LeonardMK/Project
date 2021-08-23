library(magrittr)
library(mlr3verse)
library(tidyverse)
library(tidyquant)

# Generate some random data
set.seed(2093)

int_n <- 2000
int_dim <- 100

vec_mu <- runif(int_dim, min = -5, max = 5)
mat_A <- (runif(int_dim ^ 2) * 2 - 1) %>% matrix(ncol = int_dim)
mat_sigma <- t(mat_A) %*% mat_A
mat_X <- MASS::mvrnorm(n = int_n, mu = vec_mu, Sigma = mat_sigma)
vec_e <- rnorm(int_n, sd = 2)
vec_beta <- rexp(int_dim)

vec_Y <-  mat_X %*% vec_beta + vec_e

# Form dataframe
df_task <- cbind.data.frame(vec_Y, mat_X) %>% 
  magrittr::set_names(value = c("Y", paste0("X", 1:int_dim)))

tsk_example <- as_task_regr(df_task, target = "Y")

tsk_example %>% autoplot(type = "target")

# Get learner
lrn_bl <- lrn("regr.lm")
lrn_rf <- lrn("regr.rpart", cp = 0.02)

# Get tunable parameters
lrn_rf$param_set

# Setting new values
lrn_rf$param_set$values <- list(cp = 0.01, xval = 0)

# Just a simple train and test set approach
vec_train <- tsk_example$nrow %>% sample(size = 0.8 * tsk_example$nrow)
vec_test <- tsk_example$nrow %>% seq_len() %>% setdiff(vec_train)

# Training the learner
tsk_example %>% lrn_bl$train(row_ids = vec_train)
tsk_example %>% lrn_rf$train(row_ids = vec_train)

# Prediction
pred_bl <- tsk_example %>% lrn_bl$predict(row_ids = vec_test)
pred_rf <- tsk_example %>% lrn_rf$predict(row_ids = vec_test)

# For classification confusion matrix is possible
# Get predictions by transforming to data.table
pred_bl %>% autoplot()
pred_rf %>% autoplot()

# Calculate accuracy measures
msrs()

pred_bl$score(measures = msr("regr.mape"))
pred_rf$score(measures = msr("regr.mape"))


# Resampling --------------------------------------------------------------


# Can also supply custom resampling.
# Package mlr3temporalfor timeseries

# Create resampling object
rsmp_example <- rsmp(.key = "repeated_cv", folds = 5, repeats = 5)
rsmp_example$param_set

# Instantiation applies the resampling strategy
rsmp_bl <- tsk_example %>% 
  resample(
    learner = lrn_bl,
    resampling = rsmp_example, 
    store_models = TRUE
  )

rsmp_rf <- tsk_example %>% 
  resample(
    learner = lrn_rf,
    resampling = rsmp_example,
    store_models = TRUE
  )

# Get measure for individual performance
rsmp_rf$score(measures = msrs(c("regr.mape", "regr.mse")))

# Or aggregate 
rsmp_bl$aggregate(measures = msr("regr.mape"))
rsmp_rf$aggregate(measures = msr("regr.mape"))
rsmp_bl$aggregate(measures = msr("regr.mse"))
rsmp_rf$aggregate(measures = msr("regr.mse"))

# Creating custom resampling instances
# Supply indices. Could use Caret's makeTimeSlices
list_ts_cv <- caret::createTimeSlices(
  y = tsk_example$nrow %>% seq_len(), 
  initialWindow = 200,
  horizon = 10,
  fixedWindow = FALSE, 
  skip = 10
)
rsmp_custom <- rsmp("custom")
rsmp_custom$instantiate(
  task = tsk_example,
  train_sets = list_ts_cv$train,
  test_sets = list_ts_cv$test
)

# Execute
rsmp_ts <- tsk_example %>% 
  resample(
    learner = lrn_bl,
    resampling = rsmp_custom
  )

rsmp_ts$aggregate(measures = msr("regr.mape"))

# Plot using autoplot
rsmp_bl %>% autoplot(type = "prediction")

# Comparing different learners
bmk_design_example <- benchmark_grid(
  tasks = tsk_example, # This is a list
  learners = lrns(
    c("regr.km", "regr.lm", "regr.ranger", "regr.svm", "regr.xgboost")
    ), # Supply a vector of learners 
  resamplings = rsmps("repeated_cv", folds = 5, repeats = 5) # List of resampling strategies
)

# Have to actually execute the design
bmk_example <- bmk_design_example %>% 
  benchmark()

# Create a list of measures for aggregation
list_msrs <- list(
  msr("regr.mae"),
  msr("regr.mse")
)

bmk_example$aggregate(list_msrs)

# Hyperparameter tuning
lrn_rf$param_set

ps_rf <- ps(
  cp = p_dbl(lower = 0.01, 0.2),
  minsplit = p_int(1, 20),
  minbucket = p_int(1, 10, trafo = function(x) x * 2)
)

inst_rf <- TuningInstanceSingleCrit$new(
  task = tsk_example,
  learner = lrn_rf,
  resampling = rsmp_example,
  measure = msr("regr.mse"),
  search_space = ps_rf,
  terminator = trm(.key = "stagnation")
)
tnr_rf <- tnr("random_search", batch_size = 40)
tnr_rf$optimize(
  inst = inst_rf
)

# Now set the learner's parameter values to the ones obtained by tuning
lrn_rf$param_set$values <- inst_rf$result_learner_param_vals
lrn_rf$train(tsk_example)

# Nested Resampling is a strategy to prevent over optimistic selection.
# First step is tuning the model using cross validation on the training set.
# Second: Estimate the prediction error on the holdout set.

# Want to use CV at the inner (5) and outer (5) resampling level
future::plan("multisession", workers = 3)

at_rf <- AutoTuner$new(
  learner = lrn_rf,
  resampling = rsmp("cv", folds = 5), 
  measure = msr("regr.mse"), 
  tuner = tnr_rf,
  search_space = ps_rf,
  terminator = trm("stagnation")
)

# Now use resample to perform outer resampling
# The tuner is set as the learner
rsmp_nested_rf <- resample(
  task = tsk_example, 
  learner = at_rf, 
  resampling = rsmp("cv", folds = 5), 
  store_models = TRUE
)

rsmp_nested_rf %>% extract_inner_tuning_results()
rsmp_nested_rf$score()
rsmp_nested_rf$aggregate()

# Get a more realistic example
tbl_ret <- tq_get("AAPL") %>% 
  column_to_rownames(var = "date") %>% 
  mutate(
    ret_high = (high - open) / open,
    ret_low = (low - open) / open,
    ret = (close - open) / open
  ) %>% 
  select(starts_with("ret"))

# Initiate Resampling