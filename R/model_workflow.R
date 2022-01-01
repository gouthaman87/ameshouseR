
#' Create workflow sets
#'
#' @param DF The training Data Frame
#' @param model_name The model names
#' @param features The predictor variables to fit
#'
#' @return Workflow Set object
#' @export
model_workflow <- function(
  DF,
  model_name,
  features
) {

  rec <- data_recipe(DF = DF, features = features)

  # Read models ----
  mod <- purrr::map(.x = model_name, ~eval(call(glue::glue("ml_{.x}")))) |>
    purrr::set_names(model_name)

  workflowsets::workflow_set(preproc = list(recipe = rec), models = mod)
}


#' Fit the workflow set
#'
#' @param DF The training Data Frame
#' @param model_set The workflow set which is output of `model_workflow`
#'
#' @return Workflow Set with fit column
#' @export
model_fit <- function(
  DF,
  model_set
) {

  model_set |>
    dplyr::mutate(
      fit = purrr::map(info, ~parsnip::fit(.x$workflow[[1]], data = DF))
    ) |>
    dplyr::select(wflow_id, fit)
}


#' Predict the values
#'
#' @param model_fit The fitted model workflow sets is output of `model_fit`
#' @param DF The Testing / New Data Frame to predict.
#'
#' @return Predicted Data Frame
#' @export
predict_values <- function(model_fit,
                           DF) {

  model_fit |>
    dplyr::mutate(
      predict_value = purrr::map(fit, ~predict(.x, new_data = DF)),
      data = list(DF |> dplyr::select(Sale_Price))
    ) |>
    dplyr::select(wflow_id, predict_value, data) |>
    tidyr::unnest(cols = c(predict_value, data))
}


#' Create Accuracy Metric Set
#'
#' @param DF The Predicted Data Frame
#'
#' @return The Accuracy metric set
#' @export
accuracy_metric <- function(DF) {

  ames_metrics <- yardstick::metric_set(
    yardstick::rmse,
    yardstick::mae,
    yardstick::rsq
  )

  DF |>
    dplyr::group_by(wflow_id) |>
    ames_metrics(truth = Sale_Price, estimate = .pred) |>
    tidyr::pivot_wider(id_cols = .metric, names_from = wflow_id, values_from = .estimate)
}
