
.set_boost_tree_es_args <- function(engine) {
  model_name <- "boost_tree_es"
  parsnip::set_model_arg(
    model        = model_name,
    eng          = engine,
    parsnip      = "tree_depth",
    original     = "max_depth",
    func         = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = model_name,
    eng          = engine,
    parsnip      = "learn_rate",
    original     = "eta",
    func         = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = model_name,
    eng          = engine,
    parsnip      = "mtry",
    original     = "colsample_bytree",
    func         = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = model_name,
    eng          = engine,
    parsnip      = "min_n",
    original     = "min_child_weight",
    func         = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = model_name,
    eng          = engine,
    parsnip      = "loss_reduction",
    original     = "gamma",
    func         = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model        = model_name,
    eng          = engine,
    parsnip      = "sample_size",
    original     = "subsample",
    func         = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )
}

.set_boost_tree_es_xgboost <- function() {

  model_name <- "boost_tree_es"
  engine <- "xgboost"

  nthread <- 1
  verbose <- 0

  ## Regression
  parsnip::set_model_mode(model_name, "regression")
  parsnip::set_model_engine(model_name, "regression", engine)
  parsnip::set_dependency(model_name, engine, engine)

  parsnip::set_fit(
    model = model_name,
    eng   = engine,
    mode  = "regression",
    value = list(
      interface = "matrix",
      protect   = c("x", "y", "test_x", "test_y"),
      func      = c(pkg = "parsnip4es", fun = "xgb_train_es"),
      defaults  = list(nthread = nthread, verbose = verbose)
    )
  )

  parsnip::set_pred(
    model = model_name,
    eng   = engine,
    mode  = "regression",
    type  = "numeric",
    value = list(
      pre  = NULL,
      post = NULL,
      func = c(pkg = "parsnip4es", fun = "xgb_pred_es"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

  parsnip::set_pred(
    model = model_name,
    eng   = engine,
    mode  = "regression",
    type  = "raw",
    value = list(
      pre  = NULL,
      post = NULL,
      func = c(pkg = "parsnip4es", fun = "xgb_pred_es"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

  ## Classification
  parsnip::set_model_mode(model_name, "classification")
  parsnip::set_model_engine(model_name, "classification", engine)
  parsnip::set_dependency(model_name, engine, engine)

  parsnip::set_fit(
    model = model_name,
    eng   = engine,
    mode  = "classification",
    value = list(
      interface = "matrix",
      protect   = c("x", "y", "test_x", "test_y"),
      func      = c(pkg = "parsnip4es", fun = "xgb_train_es"),
      defaults  = list(nthread = nthread, verbose = verbose)
    )
  )

  parsnip::set_pred(
    model = model_name,
    eng   = engine,
    mode  = "classification",
    type  = "class",
    value = list(
      pre  = NULL,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
        } else {
          x <- object$lvl[apply(x, 1, which.max)]
        }
        x
      },
      func = c(pkg = "parsnip4es", fun = "xgb_pred_es"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )
  parsnip::set_pred(
    model = model_name,
    eng   = engine,
    mode  = "classification",
    type  = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- tibble::tibble(v1 = 1 - x, v2 = x)
          colnames(x) <- object$lvl
        } else {
          colnames(x) <- object$lvl
          x <- tibble::as_tibble(x)
        }
        x
      },
      func = c(pkg = "parsnip4es", fun = "xgb_pred_es"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
    )

  parsnip::set_pred(
    model = model_name,
    eng   = engine,
    mode  = "classification",
    type  = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "parsnip4es", fun = "xgb_pred_es"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )
}

.set_boost_tree_es <- function() {

  model_name <- "boost_tree_es"

  .clear_parsnip_env(model_name)
  parsnip::set_new_model(model_name)

  ## arguments
  .set_boost_tree_es_args("xgboost")

  ## mode, engine, fit and predict
  .set_boost_tree_es_xgboost()
}
