#' Data to Read
#'
#' @param path The path of the data
#'
#' @return Data Frame
#' @export
read_from_path <- function (path) {
  tryCatch({

    cli::cli_alert_info(cli::col_blue("Reading Raw Data From Path: {path}"))
    envir <- environment()
    data_name <- load(path, envir = envir)
    df <- get(data_name)
    cli::cli_alert_success(cli::col_green("Successfully Read Data"))
    return(df)
  }, error = function(e) {
    cli::cli_alert_warning(cli::col_br_red("{e}"))
  })
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

  tryCatch({
    cli::cli_alert_info(cli::col_blue("Split Data on Proportion : {prop}"))

    splt <- withr::with_seed(
      seed = 1987,

      rsample::initial_split(data = DF, prop = prop, strata = dplyr::all_of(resp))
    )

    cli::cli_alert_success(cli::col_green("Successfully Splitted Data"))

    return(splt)

  }, error = function(e) {
    cli::cli_alert_danger(cli::col_br_red("{e}"))
  })

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

  tryCatch({

    cli::cli_alert_info(cli::col_blue("Create Recipe"))

    rec <- recipes::recipe(Sale_Price ~ ., data = DF) |>
      recipes::step_select(dplyr::matches(features))
    # recipes::step_log(dplyr::matches("Gr_Liv_Area"), base = 10) |>
    # recipes::step_other(dplyr::matches("Neighborhood"), threshold = tune::tune()) |>
    #
    # # Add Feature Hashing
    # # embed::step_feature_hash(recipes::all_nominal_predictors()) |>
    #
    # recipes::step_dummy(recipes::all_nominal_predictors()) |>
    #
    # # Add Interaction Term
    # recipes::step_interact(
    #  terms = ~ matches("Gr_Liv_Area"):starts_with("Bldg_Type_")
    # ) |>
    # recipes::step_zv(recipes::all_predictors()) |>
    #
    # # Add Spline Features
    # recipes::step_ns(dplyr::matches("Latitude"), deg_free = tune::tune("lat df")) |>
    # recipes::step_ns(dplyr::matches("Longitude"), deg_free = tune::tune("long df")) |>
    #
    # # Add PCA
    # recipes::step_pca(dplyr::matches("(SF$)|(GR_LIV)"))

    cli::cli_alert_success(cli::col_green("Successfully Created Recipe"))

    return(rec)

  }, error = function(e) {
    cli::cli_alert_danger(cli::col_br_red("{e}"))
  })

}
