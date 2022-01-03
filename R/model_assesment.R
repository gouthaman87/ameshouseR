#' ANOVA test for models with in resamples
#'
#' @param fitted_models The Workflowsets
#' @param metric_name The accuracy metric to filter
#'
#' @return Pair-Wise comparison results (Tukeys)
#' @export
anova_resample_test <- function(
  fitted_models,
  metric_name
) {

  dt <- tune::collect_metrics(fitted_models, summarize = FALSE) |>
    dplyr::filter(.metric == metric_name) |>
    dplyr::select(wflow_id, .estimate, id)

  anova_result <- stats::aov(.estimate ~ wflow_id, data = dt)

  res <- yardstick::tidy(anova_result)

  if(res$p.value[[1]] <= 0.05)
    stats::TukeyHSD(anova_result)
  else
    logger::log_messages("There is no change between models")
}


#' Bayesian Posterior Analysis for resamples
#'
#' @inheritParams anova_resample_test
#'
#' @return Posterior Distribution Values
#' @export
bayesian_resample_test <- function(
  fitted_models,
  metric_name
) {

  tidyposterior::perf_mod(
    fitted_models,
    metric = metric_name,
    chains = 4,
    iter = 5000,
    seed = 1987
  ) |>
    tidyposterior::tidy()
}
