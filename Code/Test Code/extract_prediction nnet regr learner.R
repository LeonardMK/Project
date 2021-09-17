self <- dml_plr_obj$.__enclos_env__$self
private <- dml_plr_obj$.__enclos_env__$private
learner <- ml_g
X_cols <- paste0("X", 1:int_p)
y_col <- "Y"
data_model <- self$data$data_model
nuisance_id <- "nuis_g"
smpls <- private$get__smpls()
est_params <- self$get_params("ml_g")
return_train_preds <- FALSE
learner_class <- private$learner_class$ml_g
fold_specific_params <- private$fold_specific_params


if (fold_specific_params) {
  stopifnot(length(smpls$train_ids) == length(smpls$test_ids))
}

fold_specific_target = (all(class(data_model) == "list"))

if (!fold_specific_target) {
  n_obs = nrow(data_model)
  task_pred = DoubleML:::initiate_task(
    id = nuisance_id, data = data_model,
    target = y_col,
    select_cols = X_cols,
    learner_class = learner_class)
  
  if (!fold_specific_params) {
    ml_learner = DoubleML:::initiate_learner(
      learner, learner_class,
      est_params, return_train_preds)
    resampling_smpls = rsmp("custom")$instantiate(
      task_pred, smpls$train_ids,
      smpls$test_ids)
    resampling_pred = resample(task_pred, ml_learner, resampling_smpls,
                               store_models = TRUE)
    preds = DoubleML:::extract_prediction(resampling_pred, learner_class, n_obs)
    if (return_train_preds) {
      train_preds = extract_prediction(resampling_pred, learner_class, n_obs,
                                       return_train_preds = TRUE)
    }
  } else {
    # learners initiated according to fold-specific learners, proceed foldwise
    ml_learners = lapply(
      est_params,
      function(x) {
        DoubleML:::initiate_learner(
          learner,
          learner_class, x,
          return_train_preds)
      })
    resampling_smpls = lapply(
      seq_len(length(smpls$train_ids)),
      function(x) {
        rsmp("custom")$instantiate(
          task_pred, list(smpls$train_ids[[x]]),
          list(smpls$test_ids[[x]]))
      })
    
    resampling_pred = lapply(seq_len(length(ml_learners)), function(x) {
      resample(task_pred, ml_learners[[x]],
               resampling_smpls[[x]],
               store_models = TRUE)
    })
    
    preds = DoubleML:::extract_prediction(resampling_pred, learner_class, n_obs)
    if (return_train_preds) {
      train_preds = extract_prediction(resampling_pred, learner_class,
                                       n_obs,
                                       return_train_preds = TRUE)
    }
  }
} else {
  n_obs = nrow(data_model[[1]])
  task_pred = lapply(data_model, function(x) {
    DoubleML:::initiate_task(
      id = nuisance_id, data = x,
      target = y_col,
      select_cols = X_cols,
      learner_class = learner_class)
  })
  # fold_specific_target == TRUE; only required for pliv_partialXZ
  if (!fold_specific_params) {
    ml_learner = DoubleML:::initiate_learner(learner, learner_class, est_params)
    
    resampling_smpls = lapply(
      seq_len(length(data_model)),
      function(x) {
        rsmp("custom")$instantiate(
          task_pred[[x]],
          list(smpls$train_ids[[x]]),
          list(smpls$test_ids[[x]]))
      })
    resampling_pred = lapply(
      seq_len(length(data_model)),
      function(x) {
        resample(task_pred[[x]], ml_learner,
                 resampling_smpls[[x]],
                 store_models = TRUE)
      })
    preds = extract_prediction(resampling_pred, learner_class, n_obs)
  } else {
    # learners initiated according to fold-specific learners, proceed foldwise
    ml_learners = lapply(
      est_params,
      function(x) DoubleML:::initiate_learner(learner, learner_class, x))
    resampling_smpls = lapply(seq_len(length(smpls$train_ids)), function(x) {
      rsmp("custom")$instantiate(
        task_pred[[x]], list(smpls$train_ids[[x]]),
        list(smpls$test_ids[[x]]))
    })
    resampling_pred = lapply(seq_len(length(ml_learners)), function(x) {
      resample(task_pred[[x]], ml_learners[[x]],
               resampling_smpls[[x]],
               store_models = TRUE)
    })
    preds = extract_prediction(resampling_pred, learner_class, n_obs)
  }
}

if (compareVersion(as.character(packageVersion("mlr3")), "0.11.0") < 0) {
  ind_name = "row_id"
} else {
  ind_name = "row_ids"
}

if (learner_class == "LearnerRegr") {
  
  if (is.list(obj_resampling)) {
    
    if (all(purrr::map_lgl(obj_resampling, ~ .x$learner$id == "regr.nnet"))) {
      resp_name = "response.V1"
    } else {
      resp_name = "response"
    }
    
  } else if ("ResampleResult" %in% class(obj_resampling)) {
    resp_name = "response"
  }
  
} else if (learner_class == "LearnerClassif") {
  resp_name = "prob.1"
}

if (return_train_preds) {
  if (testR6(obj_resampling, classes = "ResampleResult")) {
    n_iters = obj_resampling$resampling$iters
    preds = vector("list", n_iters)
    f_hat_list = lapply(
      1:n_iters,
      function(x) as.data.table(obj_resampling$predictions("train")[[x]]))
    for (i_iter in 1:n_iters) {
      preds_vec = rep(NA_real_, n_obs)
      f_hat = f_hat_list[[i_iter]]
      preds_vec[f_hat[[ind_name]]] = f_hat[[resp_name]]
      preds[[i_iter]] = preds_vec
    }
  } else {
    n_obj_rsmp = length(obj_resampling)
    preds = vector("list", n_obj_rsmp)
    for (i_obj_rsmp in 1:n_obj_rsmp) {
      preds_vec = vector("numeric", length = n_obs)
      f_hat = as.data.table(obj_resampling[[i_obj_rsmp]]$prediction("train"))
      preds_vec[f_hat[[ind_name]]] = f_hat[[resp_name]]
      preds[[i_obj_rsmp]] = preds_vec
    }
  }
} else {
  preds = rep(NA_real_, n_obs)
  if (testR6(obj_resampling, classes = "ResampleResult")) obj_resampling = list(obj_resampling)
  n_obj_rsmp = length(obj_resampling)
  for (i_obj_rsmp in 1:n_obj_rsmp) {
    f_hat = as.data.table(obj_resampling[[i_obj_rsmp]]$prediction("test"))
    preds[f_hat[[ind_name]]] = f_hat[[resp_name]]
  }
}