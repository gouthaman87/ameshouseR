#' Linear Regression
#'
#' @return model
#' @export
ml_lm <- function(){
  parsnip::linear_reg(mode = "regression") |>
    parsnip::set_engine("lm")
}


#' GLM (Lasso / Ridge ) Regression
#'
#' @return model
#' @export
ml_glmnet <- function(){
  parsnip::linear_reg(
    mode = "regression",
    penalty = tune::tune(),
    mixture = tune::tune()
  ) |>
    parsnip::set_engine("glmnet")
}


#' Stan Regression
#'
#' @return model
#' @export
ml_stan <- function(){
  parsnip::linear_reg(mode = "regression") |>
    parsnip::set_engine("stan")
}


#' Random Forest
#'
#' @return model
#' @export
ml_rf <- function(){
  parsnip::rand_forest(
    mode = "regression",
    trees = 100
    # mtry = tune::tune()
  ) |>
    parsnip::set_engine("ranger",
                        importance = "permutation")
}


#' Distributed Random Forest
#'
#' @return model
#' @export
ml_drf <- function(){
  parsnip::rand_forest(
    mode = "regression"
    # trees = 100
    # mtry = tune::tune()
  ) |>
    parsnip::set_engine("h2o",
                        categorical_encoding = "SortByResponse")
}


#' Decision Tree
#'
#' @return model
#' @export
ml_dt <- function(){
  parsnip::decision_tree(
    mode = "regression"
  ) |>
    parsnip::set_engine("rpart")
}


#' LightGBM
#'
#' @return model
#' @export
ml_lgbm <- function(){
  parsnip::boost_tree(
    mode = "regression",
    tree_depth = tune::tune(),
    learn_rate = tune::tune(),
    loss_reduction = tune::tune(),
    min_n = tune::tune(),
    sample_size = tune::tune(),
    trees = tune::tune()
    # mtry = tune::tune()
  ) |>
    parsnip::set_engine("lightgbm")
}


#' catBoost
#'
#' @return model
#' @export
ml_catboost <- function(){
  parsnip::boost_tree(
    mode = "regression"
    # mtry = tune::tune()
  ) |>
    parsnip::set_engine("catboost")
}

