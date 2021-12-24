
#' Create workflow sets
#'
#' @param DF The training Data Frame
#' @param model_name The model names
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



#' #' Fit the workflow set
#' #'
#' #' @param DF The training Data Frame
#' #' @param model_set The workflow set which is output of `model_workflow`
#' #'
#' #' @return Workflow Set with fit column
#' #' @export
#' model_fit <- function(
#'   DF,
#'   model_set
#' ) {
#'
#'   model_set |>
#'     # dplyr::mutate(
#'     #   fit = purrr::map(info, ~parsnip::fit(.x$workflow[[1]], DF))
#'     # )
#'     workflowsets::workflow_map(data = DF)
#' }


# predict_values <- function(model_fit,
#                            DF) {
#
#   model_fit |>
#     dplyr::mutate(
#       predict_value = purrr::map(fit, ~predict(.x$workflow[[1]], newdata = DF))
#     )
#
#   # extracted_model |>
#   #   purrr::map_dfr(~{
#   #     DF |>
#   #       dplyr::select(Sale_Price) |>
#   #       dplyr::bind_cols(predict_value = predict(.x, newdata = DF))
#   #   },
#   #   .id = "model")
# }
