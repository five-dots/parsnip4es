
#' Fit a Model Specification to a Dataset
#'
#' @inheritParams parsnip::fit.model_spec
#' @param test_data Optional for models that require validation data during
#' the model fitting. The same requirement as \code{data}.
#' @rdname fit
#' @export
#' @export fit.model_spec
fit.model_spec <- function(object, formula, data, test_data = NULL,
                           control = parsnip::control_parsnip(), ...) {

  if (object$mode == "unknown") {
    stop("Please set the mode in the model specification.", call. = FALSE)
  }

  ## Default engine
  if (is.null(object$engine)) {
    eng_vals <- parsnip:::possible_engines(object)
    object$engine <- eng_vals[1]
    if (control$verbosity > 0) {
      warning("Engine set to `", object$engine, "`", call. = FALSE)
    }
  }

  ## Check if ... contain "x" and "y"
  dots <- rlang::quos(...)
  if (all(c("x", "y") %in% names(dots)))
    stop("`fit.model_spec()` is for the formula methods. Use `fit_xy()` instead.",
          call. = FALSE)

  ## Evaluation environment
  eval_env <- rlang::env()
  eval_env$data <- data
  eval_env$test_data <- test_data
  eval_env$formula <- formula

  ## interface: "formula", "matrix", "data.frame"
  cl <- match.call(expand.dots = TRUE)
  fit_interface <- parsnip:::check_interface(eval_env$formula, eval_env$data,
                                             cl, object)

  if (object$engine == "spark" && !inherits(eval_env$data, "tbl_spark"))
    stop(
      "spark objects can only be used with the formula interface to `fit()` ",
      "with a spark data object.", call. = FALSE
    )

  ## populate `method` with the details for this model type
  object <- parsnip:::add_methods(object, engine = object$engine)

  parsnip:::check_installs(object)

  interfaces <- paste(fit_interface, object$method$fit$interface, sep = "_")

  ## Call wrapper
  res <- switch(interfaces,
    ## homogeneous combinations:
    formula_formula =
      parsnip:::form_form(
        object = object,
        control = control,
        env = eval_env
      ),
    ## heterogenous combinations
    formula_matrix =
      form_xy(
        object = object,
        control = control,
        env = eval_env,
        target = object$method$fit$interface,
        ...
      ),
    formula_data.frame =
      form_xy(
        object = object,
        control = control,
        env = eval_env,
        target = object$method$fit$interface,
        ...
      ),
    stop(interfaces, " is unknown")
  )

  model_classes <- class(res$fit)
  class(res) <- c(paste0("_", model_classes[1]), "model_fit")
  res
}
