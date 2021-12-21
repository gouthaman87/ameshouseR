#' Train/Test Split
#'
#' @param DF The Data Frame
#' @param prop The proportion of Training Data
#' @param resp The name of response variable
#'
#' @return rsplit
#' @export
data_split <- function(
  DF,
  prop,
  resp
) {

  withr::with_seed(
    seed = 1987,

    rsample::initial_split(data = DF, prop = prop, strata = resp)
  )
}
