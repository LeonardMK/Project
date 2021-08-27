library(ISLR)
library(mlr3extralearners)
library(mlr3verse)
library(tidyverse)

# Create task
tsk_credit <- as_task_regr(Credit[1:350, -1], target = "Rating")
lrn_nnet <- lrn("regr.nnet")

ps_nnet <- ps(
  size = p_int(lower = 1, upper = 20),
  decay = p_dbl(lower = 0, upper = 200),
  maxit = p_int(lower = 500, upper = 10000)
)

rsmp_credit <- rsmp("repeated_cv", folds = 5, repeats = 5)
msr_credit <- msr("regr.mse")

inst_nnet <- TuningInstanceSingleCrit$new(
  task = tsk_credit, 
  learner = lrn_nnet,
  resampling = rsmp_credit, 
  measure = msr_credit, 
  terminator = trm("stagnation"), 
  search_space = ps_nnet
)

ps_ranger <- ps(
  mtry = p_int(lower = 1, upper = 5),
  num.trees = p_int(lower = 100, upper = 1000)
)

inst_ranger <- TuningInstanceSingleCrit$new(
  task = tsk_credit, 
  learner = lrn("regr.ranger"),
  resampling = rsmp_credit, 
  measure = msr_credit, 
  terminator = trm("stagnation"), 
  search_space = ps_ranger
)

tnr_credit <- tnr("random_search")
tnr_credit$optimize(inst_nnet)
tnr_credit$optimize(inst_ranger)

# Compare results
inst_nnet$result_y
inst_nnet$result_learner_param_vals
inst_ranger$result_y
inst_ranger$result_learner_param_vals

nnet_result <- nnet::nnet(Rating ~ ., data = Credit[1:350, -1], size = 3, decay = 3.662855, maxit = 2800, linout = TRUE)
vec_y_hat_nnet <- predict(nnet_result, Credit[351:400, -1])
ranger_result <- ranger::ranger(Rating ~ ., data = Credit[1:350, -1], mtry = 5, num.trees = 415)
vec_y_hat_ranger <- predict(ranger_result, Credit[351:400, -1])
mean((vec_y_hat_nnet - Credit[351:400, "Rating"]) ^ 2)
mean((vec_y_hat_ranger$predictions - Credit[351:400, "Rating"]) ^ 2)
