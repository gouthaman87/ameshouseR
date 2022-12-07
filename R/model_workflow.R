init_predict <- function(
  DF,
  model_name,
  features
) {

  workflow_set <- model_workflow(

  )
}



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

  tryCatch({

    # Call Recipe ----
    rec <- data_recipe(DF = DF, features = features)

    cli::cli_alert_info(cli::col_blue("Create Workflow Set"))

    # Read models ----
    mod <- purrr::map(
      .x = model_name,
      ~eval(call(glue::glue("ml_{.x}")))
    ) |>
      purrr::set_names(model_name)

    wflw_set <- workflowsets::workflow_set(preproc = list(recipe = rec), models = mod)

    cli::cli_alert_success(cli::col_green("Successfully Created Workflow Set"))

    return(wflw_set)

  }, error = function(e) {
    cli::cli_alert_danger(cli::col_br_red("{e}"))
  })
}


#' Fit the workflow set
#'
#' @param DF The training Data Frame
#' @param model_set The workflow set which is output of `model_workflow`
#' @param type_of_resample The type of resample Cross-Validation/Validation/Bootstrap
#'
#' @return Workflow Set with fit column
#' @export
model_fit <- function(
  splt,
  type_of_resample = as.character(),
  model_set
) {

  DF <- rsample::training(splt)

  capture.output(h2o::h2o.init())
  withr::defer(h2o::h2o.shutdown(prompt = FALSE))

  if(!type_of_resample %in% c("cv", "validation", "bootstrap"))
    stop("Please Select a Resample method")

  logger::log_info("Model fitting on {type_of_resample} Method")

  # Create Cross-Validation Folds
  if(type_of_resample %in% "cv") {
    folds <- rsample::vfold_cv(DF, v = 10)
  } else if(type_of_resample %in% "validation") {
    folds <- rsample::validation_split(DF, prop = 3/4)
  } else {
    folds <- rsample::bootstraps(DF, times = 5)
  }

  grd_ctrl <- tune::control_grid(
    save_pred = TRUE,
    save_workflow = TRUE
    # parallel_over = "everything",
    # pkgs = "treesnip"
  )

  grd_results <- model_set |>
    workflowsets::workflow_map(
      resamples = folds,
      grid = 10,
      metrics = yardstick::metric_set(yardstick::rmse),
      verbose = TRUE,
      control = grd_ctrl
    )

  # grd_results |>
  #   workflowsets::rank_results() |>
  #   dplyr::filter(.metric == "rmse") |>
  #   dplyr::select(model, .config, rmse = mean, rank)
  #
  # best_results <- grd_results |>
  #   workflowsets::extract_workflow_set_result("recipe_drf") |>
  #   tune::select_best(metric = "rmse")
  #
  # final_mod <- grd_results |>
  #   workflowsets::extract_workflow("recipe_drf") |>
  #   tune::finalize_workflow(best_results) |>
  #   tune::last_fit(splt)
  #
  # final_mod |>
  #   workflowsets::extract_fit_parsnip() |>
  #   vip::vip(num_features = 50)
}


#' Predict the values
#'
#' @param model_fit The fitted model workflow sets is output of `model_fit`
#' @param DF The Testing / New Data Frame to predict.
#'
#' @return Predicted Data Frame
#' @export
predict_values <- function(model_fit, DF) {

  model_fit |>
    dplyr::mutate(
      predict_value = purrr::map(result, ~predict(.x, new_data = DF)),
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
