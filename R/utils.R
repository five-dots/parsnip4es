
.clear_parsnip_env <- function(model_name) {
  suffix <- c("", "_args", "_modes", "_pkgs", "_fit", "_predict")
  vars <- paste0(model_name, suffix)
  env <- parsnip::get_model_env()

  model_idx <- which(env$models == model_name)
  if (length(model_idx) != 0) {
    env$models <- env$models[-model_idx]
  }

  purrr::walk(vars, function(var) {
    if (var %in% names(env))
      rm(list = var, envir = env)
  })

  invisible(NULL)
}
