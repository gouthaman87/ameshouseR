#' Linear Regression
#'
#' @return model
#' @export
ml_lm <- function(){
  parsnip::linear_reg(mode = "regression") %>%
    parsnip::set_engine("lm")
}


#' GLM (Lasso / Ridge ) Regression
#'
#' @return model
#' @export
ml_glmnet <- function(){
  parsnip::linear_reg(mode = "regression") %>%
    parsnip::set_engine("glmnet")
}


#' Stan Regression
#'
#' @return model
#' @export
ml_stan <- function(){
  parsnip::linear_reg(mode = "regression") %>%
    parsnip::set_engine("stan")
}
