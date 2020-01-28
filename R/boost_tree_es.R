
#' General Interface for Boosted Trees with early stopping support
#'
#' @inheritParams parsnip::boost_tree
#' @seealso [parsnip::boost_tree()]
#' @examples
#' boost_tree_es(mode = "classification", tree_depth = 5)
#' # Parameters can be represented by a placeholder:
#' boost_tree_es(mode = "regression", mtry = varying())
#' @export
boost_tree_es <- function(mode = "unknown", mtry = NULL, min_n = NULL,
                          tree_depth = NULL, learn_rate = NULL,
                          loss_reduction = NULL, sample_size = NULL) {

  args <- list(
    mtry           = rlang::enquo(mtry),
    min_n          = rlang::enquo(min_n),
    tree_depth     = rlang::enquo(tree_depth),
    learn_rate     = rlang::enquo(learn_rate),
    loss_reduction = rlang::enquo(loss_reduction),
    sample_size    = rlang::enquo(sample_size)
  )

  parsnip::new_model_spec(
    cls      = "boost_tree_es",
    args     = args,
    eng_args = NULL,
    mode     = mode,
    method   = NULL,
    engine   = NULL
  )
}

#' @export
print.boost_tree_es <- function(x, ...) {
  cat("Boosted Tree Model (with Early Stopping) Specification (",
      x$mode, ")\n\n", sep = "")

  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }
  invisible(x)
}

#' @inheritParams parsnip::update.boost_tree
#' @rdname boost_tree_es
#' @examples
#' model <- boost_tree_es(mtry = 10, min_n = 3)
#' model
#' update(model, mtry = 1)
#' update(model, mtry = 1, fresh = TRUE)
#'
#' param_values <- tibble::tibble(mtry = 10, tree_depth = 5)
#'
#' model %>% update(param_values)
#' model %>% update(param_values, mtry = 3)
#'
#' param_values$verbose <- 0
#' # Fails due to engine argument
#' # model %>% update(param_values)
#' @export
update.boost_tree_es <- function(object, parameters = NULL, mtry = NULL,
                                 min_n = NULL, tree_depth = NULL,
                                 learn_rate = NULL, loss_reduction = NULL,
                                 sample_size = NULL, fresh = FALSE, ...) {

  parsnip::update_dot_check(...)

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }

  args <- list(
    mtry           = rlang::enquo(mtry),
    min_n          = rlang::enquo(min_n),
    tree_depth     = rlang::enquo(tree_depth),
    learn_rate     = rlang::enquo(learn_rate),
    loss_reduction = rlang::enquo(loss_reduction),
    sample_size    = rlang::enquo(sample_size)
  )

  args <- parsnip::update_main_parameters(args, parameters)

  # TODO make these blocks into a function and document well
  if (fresh) {
    object$args <- args
  } else {
    null_args <- purrr::map_lgl(args, parsnip::null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0)
      object$args[names(args)] <- args
  }

  parsnip::new_model_spec(
    cls      = "boost_tree_es",
    args     = object$args,
    eng_args = object$eng_args,
    mode     = object$mode,
    method   = NULL,
    engine   = object$engine
  )
}

#' @export
translate.boost_tree_es <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'xgboost'` for translation.")
    engine <- "xgboost"
  }
  x <- parsnip:::translate.default(x, engine, ...)

  if (engine == "spark") {
    if (x$mode == "unknown") {
      stop(
        "For spark boosted trees models, the mode cannot be 'unknown' ",
        "if the specification is to be translated.",
        call. = FALSE
      )
    } else {
      x$method$fit$args$type <- x$mode
    }
  }
  x
}

check_args.boost_tree_es <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$sample_size) &&
      (args$sample_size < 0 | args$sample_size > 1))
    stop("`sample_size` should be within [0,1]", call. = FALSE)

  if (is.numeric(args$tree_depth) && args$tree_depth < 0)
    stop("`tree_depth` should be >= 1", call. = FALSE)

  if (is.numeric(args$min_n) && args$min_n < 0)
    stop("`min_n` should be >= 1", call. = FALSE)

  invisible(object)
}

xgb_data <- function(x, y) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  xgboost::xgb.DMatrix(x, label = y, missing = NA)
}

#' Boosted trees via xgboost with early stopping support
#'
#' @inheritParams parsnip::xgb_train
#' @param test_x A data frame or matrix of predictors
#' @param test_y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @return A fitted `xgboost` object.
#' @keywords internal
#' @export
xgb_train_es <- function(x, y, test_x, test_y, max_depth = 6, eta  = 0.3,
                         colsample_bytree = 1, min_child_weight = 1, gamma = 0,
                         subsample = 1, ...) {

  num_class <- if (length(levels(y)) > 2) length(levels(y)) else NULL

  if (is.numeric(y)) {
    loss <- "reg:linear"
  } else {
    lvl <- levels(y)
    y <- as.numeric(y) - 1
    test_y <- as.numeric(test_y) - 1
    if (length(lvl) == 2) {
      loss <- "binary:logistic"
    } else {
      loss <- "multi:softprob"
    }
  }

  dtrain <- xgb_data(x, y)
  dtest <- xgb_data(test_x, test_y)

  ## translate `subsample` and `colsample_bytree` to be on (0, 1] if not
  n <- nrow(x)
  p <- ncol(x)
  if (subsample > 1) {
    subsample <- subsample / n
  }
  if (subsample > 1) {
    subsample <- 1
  }
  if (colsample_bytree > 1) {
    colsample_bytree <- colsample_bytree/p
  }
  if (colsample_bytree > 1) {
    colsample_bytree <- 1
  }

  arg_list <- list(
    eta              = eta,
    max_depth        = max_depth,
    gamma            = gamma,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    subsample        = subsample
  )

  ## eval if contains expressions?
  main_args <- list(
    data                   = quote(dtrain),
    params                 = arg_list,
    nrounds                = 10000,
    objective              = loss,
    watchlist              = quote(list(train = dtrain, eval = dtest)),
    early_stopping_rounds  = round(10 / eta)
  )
  if (!is.null(num_class)) {
    main_args$num_class <- num_class
  }

  call <- parsnip:::make_call(fun = "xgb.train", ns = "xgboost", main_args)

  # override or add some other args
  arg_names <- c("data", "weights", "num_class", names(arg_list))
  others <- list(...)
  others <- others[!(names(others) %in% arg_names)]
  if (length(others) > 0) {
    call <- rlang::call_modify(call, !!!others)
  }

  rlang::eval_tidy(call, env = rlang::current_env())
}

#' Predict by xgboost fitted object with early stopping support
#'
#' @param object model_fit object
#' @param newdata new data.
#' @param ... Additional arguments for predict().
#' @importFrom stats predict
#' @keywords internal
#' @export
xgb_pred_es <- function(object, newdata, ...) {
  if (!inherits(newdata, "xgb.DMatrix")) {
    newdata <- as.matrix(newdata)
    newdata <- xgboost::xgb.DMatrix(data = newdata, missing = NA)
  }

  res <- predict(object, newdata, ...)

  switch(object$params$objective,
    "reg:linear" = , "reg:logistic" = , "binary:logistic" = res,
    "binary:logitraw" = stats::binomial()$linkinv(res),
    "multi:softprob" = matrix(res, ncol = object$params$num_class, byrow = TRUE),
    res
  )
}
