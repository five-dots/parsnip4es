
form_xy <- function(object, control, env, target = "none", ...) {

  ## data
  data_obj <- parsnip:::convert_form_to_xy_fit(
    formula = env$formula, data = env$data, ..., composition = target
  )
  env$x <- data_obj$x
  env$y <- data_obj$y

  ## set test data if not null
  if (!is.null(env$test_data)) {
    test_data_obj <- parsnip:::convert_form_to_xy_fit(
      formula = env$formula, data = env$test_data, ..., composition = target
    )
    env$test_x <- test_data_obj$x
    env$test_y <- test_data_obj$y
  }

  ## result object
  res <- list(lvl = parsnip:::levels_from_formula(env$formula, env$data),
              spec = object)
  if (object$mode == "classification") {
    if (is.null(res$lvl))
      stop("For classification models, the outcome should be a factor.",
           call. =  FALSE)
  }
  res <- xy_xy(object = object, env = env, control = control, target = target)

  data_obj$y_var <- all.vars(env$formula[[2]])
  data_obj$x <- NULL
  data_obj$y <- NULL
  data_obj$weights <- NULL
  data_obj$offset <- NULL
  res$preproc <- data_obj
  res
}

xy_xy <- function(object, env, control, target = "none", ...) {

  if (inherits(env$x, "tbl_spark") | inherits(env$y, "tbl_spark"))
    stop("spark objects can only be used with the formula interface to `fit()`",
         call. = FALSE)

  ## check label type
  object <- parsnip:::check_mode(object, levels(env$y))
  if (object$mode == "classification") {
    if (is.null(levels(env$y)))
      stop("For classification models, the outcome should be a factor.",
           call. =  FALSE)
  }

  ## if descriptors are needed, update descr_env with the calculated values
  if (parsnip:::requires_descrs(object)) {
    data_stats <- parsnip:::get_descr_xy(env$x, env$y)
    parsnip:::scoped_descrs(data_stats)
  }

  ## evaluate quoted args once here to check them
  object <- parsnip:::check_args(object)

  ## sub in arguments to actual syntax for corresponding engine
  object <- parsnip::translate(object, engine = object$engine)

  ## assign data arguments
  object$method$fit$args[["y"]] <- quote(y)
  object$method$fit$args[["x"]] <-
    switch(target,
      none       = quote(x),
      data.frame = quote(as.data.frame(x)),
      matrix     = quote(as.matrix(x)),
      stop("Invalid data type target: ", target)
    )

  ## assign test_data if not null
  if (!is.null(env$test_data)) {
    object$method$fit$args[["test_y"]] <- quote(test_y)
    object$method$fit$args[["test_x"]] <-
      switch(target,
        none       = quote(test_x),
        data.frame = quote(as.data.frame(test_x)),
        matrix     = quote(as.matrix(test_x)),
        stop("Invalid data type target: ", target)
      )
  }

  ## call
  fit_call <- parsnip:::make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    object$method$fit$args
  )

  res <- list(lvl = levels(env$y), spec = object)

  ## eval model
  elapsed <- system.time(
    res$fit <- parsnip:::eval_mod(
      fit_call, capture = control$verbosity == 0,
      catch = control$catch, env = env, ...)
  )
  if (is.vector(env$y)) {
    y_name <- character(0)
  } else {
    y_name <- colnames(env$y)
  }
  res$preproc <- list(y_var = y_name)
  res$elapsed <- elapsed
  res
}
