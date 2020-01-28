
rsplit <- rsample::initial_split(iris)
train <- rsample::training(rsplit)
test <- rsample::testing(rsplit)

test_that("xgb_train_es", {

  x <- dplyr::select(train, -Species) %>% as.matrix()
  y <- dplyr::pull(train, Species)
  test_x <- dplyr::select(test, -Species) %>% as.matrix()
  test_y <- dplyr::pull(test, Species)

  fit <- xgb_train_es(x, y, test_x, test_y, verbose = 0)
  expect_is(fit, "xgb.Booster")

})

test_that("boost_tree_es + xgboost for classification", {

  ## spec
  mode <- "classification"
  pkg <- "xgboost"
  mod <- boost_tree_es(mode = mode)
  mod <- parsnip::set_engine(mod, pkg)

  expect_is(mod, "boost_tree_es")
  expect_is(mod, "model_spec")
  expect_equal(mod$mode, mode)
  expect_equal(mod$engine, pkg)

  ## update spec
  mod <- update(mod, mtry = 2, min_n = 2, tree_depth = 5,
                learn_rate = 0.1, loss_reduction = 0.01,
                sample_size = 0.8)
  trans <- translate(mod)

  params <- tibble::tibble(mtry = 1, min_n = 1, tree_depth = 6,
                           learn_rate = 0.3, loss_reduction = 0,
                           sample_size = 1)
  mod <- update(mod, params)
  trans <- translate(mod)

  ## fit
  fit <- fit.model_spec(mod, Species ~ ., data = train, test_data = test)
  fit$fit$call

  expect_is(fit, "_xgb.Booster")
  expect_is(fit, "model_fit")
  expect_is(fit$fit, "xgb.Booster")
  expect_equal(fit$lvl, c("setosa", "versicolor", "virginica"))

  ## predict
  pred_class <- predict(fit, new_data = test, type = "class")
  expect_is(pred_class, "data.frame")
  expect_is(pred_class$.pred_class, "factor")
  expect_named(pred_class, ".pred_class")
  expect_equal(nrow(pred_class), nrow(test))

  pred_prob <- predict(fit, new_data = test, type = "prob")
  expect_is(pred_prob, "data.frame")
  expect_is(pred_prob$.pred_setosa, "numeric")
  expect_is(pred_prob$.pred_versicolor, "numeric")
  expect_is(pred_prob$.pred_virginica, "numeric")
  expect_named(pred_prob, c(".pred_setosa", ".pred_versicolor", ".pred_virginica"))
  expect_equal(nrow(pred_prob), nrow(test))

  pred_raw <- predict(fit, new_data = test, type = "raw")
  expect_is(pred_raw, "matrix")
  expect_length(pred_raw, nrow(test) * 3)

})

test_that("boost_tree_es + xgboost for regression", {

  ## spec
  mode <- "regression"
  pkg <- "xgboost"
  mod <- boost_tree_es(mode = mode)
  mod <- parsnip::set_engine(mod, pkg, verbose = 0)

  expect_is(mod, "boost_tree_es")
  expect_is(mod, "model_spec")
  expect_equal(mod$mode, mode)
  expect_equal(mod$engine, pkg)

  ## fit
  fit <- fit.model_spec(mod, Sepal.Length ~ ., data = train, test_data = test)
  expect_is(fit, "_xgb.Booster")
  expect_is(fit, "model_fit")
  expect_is(fit$fit, "xgb.Booster")
  expect_null(fit$lvl)

  ## predict
  pred_num <- predict(fit, new_data = test, type = "numeric")
  expect_is(pred_num, "data.frame")
  expect_is(pred_num$.pred, "numeric")
  expect_named(pred_num, ".pred")
  expect_equal(nrow(pred_num), nrow(test))

  pred_raw <- predict(fit, new_data = test, type = "raw")
  expect_is(pred_raw, "numeric")
  expect_length(pred_raw, nrow(test))

})
