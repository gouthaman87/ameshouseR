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
  parsnip::linear_reg(mode = "regression",
                      penalty = 0) |> 
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
  parsnip::rand_forest(mode = "regression") |> 
    parsnip::set_engine("ranger",
                        importance = "permutation")
}


#' Decision Tree
#'
#' @return model
#' @export
ml_dt <- function(){
  parsnip::decision_tree(mode = "regression") |> 
    parsnip::set_engine("rpart")
}
