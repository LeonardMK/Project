devtools::load_all()
create_learner(
  pkg = ".",
  classname = "Nnet",
  algorithm = "Single Layer Neural Network",
  type = "regr",
  key = "nnet",
  package = "nnet",
  caller = "nnet.formula",
  feature_types = c("numeric", "factor", "ordered"),
  predict_types = c("response"),
  properties = c("weights"),
  references = FALSE,
  gh_name = "LeonardMK"
)
