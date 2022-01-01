#' Plot Histogram
#'
#' @param DF The Data Frame
#' @param var The Variable name
#' @param log_scale Boolean: Plot with log scale or not
#'
#' @return Plot
#' @export
plot_hist <- function(
  DF,
  var,
  log_scale = FALSE
) {

  p <- ggplot2::ggplot(data = DF, ggplot2::aes_string(x = var)) +
    ggplot2::geom_histogram(bins = 50) +
    ggplot2::labs(title = "Histogram with Bins:50", y = NULL) +
    ggplot2::theme_minimal()

  if(log_scale)
    p + ggplot2::scale_x_log10()
  else
    p + ggplot2::scale_x_continuous(labels = scales::dollar)

  plotly::ggplotly(p)
}


#' Plot Spatial
#'
#' @inheritParams plot_hist
#'
#' @return Plot
#' @export
plot_spatial <- function(DF) {

  p <- ggplot2::ggplot(data = DF) +
    ggplot2::geom_point(ggplot2::aes(Longitude, Latitude, col = Neighborhood)) +
    viridis::scale_color_viridis(discrete = TRUE) +
    ggplot2::theme_minimal()

  plotly::ggplotly(p)
}


#' Scatter Plot of Actual Vs. Predicted
#'
#' @inheritParams plot_hist
#'
#' @importFrom tune coord_obs_pred
#'
#' @return Plot
#' @export
plot_error <- function(DF) {

  p <- ggplot2::ggplot(data = DF, ggplot2::aes(Sale_Price, .pred, col = wflow_id)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth(method = "loess") +
    viridis::scale_color_viridis(discrete = TRUE) +
    ggplot2::scale_x_continuous(labels = scales::dollar) +
    ggplot2::scale_y_continuous(labels = scales::dollar) +
    ggplot2::theme_minimal() +
    tune::coord_obs_pred()

  plotly::ggplotly(p)
}
