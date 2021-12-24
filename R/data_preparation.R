#' Data to Read
#'
#' @param path The path of the data
#'
#' @return Data Frame
#' @export
read_from_path <- function (path) {
  envir <- environment()
  data_name <- load(path, envir = envir)
  get(data_name)
}


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

    rsample::initial_split(data = DF, prop = prop, strata = dplyr::all_of(resp))
  )
}


data_recipe <- function(
  DF,
  features
) {

  recipes::recipe(Sale_Price ~ ., data = DF) |>
    recipes::step_select(dplyr::matches(features)) |>
    recipes::step_log(Gr_Liv_Area, base = 10) |>
    recipes::step_other(dplyr::matches("Neighborhood"), threshold = 0.01) |>
    recipes::step_dummy(recipes::all_nominal_predictors())
}
