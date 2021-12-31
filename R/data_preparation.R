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


#' The Data Recipe with Feature Engineering
#'
#' @param DF The Data to create Recipe
#' @param features The predictor variables to fit
#'
#' @importFrom dplyr matches
#' @importFrom dplyr starts_with
#'
#' @return The recipe
#' @export
data_recipe <- function(
  DF,
  features
) {

  DF <- DF |>
    dplyr::select(dplyr::matches(features))

  recipes::recipe(Sale_Price ~ ., data = DF) |>
    recipes::step_log(dplyr::matches("Gr_Liv_Area"), base = 10) |>
    recipes::step_other(dplyr::matches("Neighborhood"), threshold = 0.01) |>

    # Add Feature Hashing
    # embed::step_feature_hash(recipes::all_nominal_predictors()) |>

    recipes::step_dummy(recipes::all_nominal_predictors()) |>

    # Add Interaction Term
    recipes::step_interact(
     terms = ~ matches("Gr_Liv_Area"):starts_with("Bldg_Type_")
    ) |>
    recipes::step_zv(recipes::all_predictors()) |>

    # Add Spline Features
    recipes::step_ns(dplyr::matches("Latitude"), deg_free = 20) |>

    # Add PCA
    recipes::step_pca(dplyr::matches("(SF$)|(GR_LIV)"))
}
