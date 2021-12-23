
fit_model <- function(
  DF,
  model_name
) {

  # Read models ----
  purrr::map(
    .x = model_name,
    ~{eval(call(glue::glue("ml_{.x}"))) |>
        parsnip::fit(Sale_Price ~ Longitude + Latitude, data = DF)}
  ) |> 
    purrr::set_names(model_name)
}


extract_fit <- function(fitted_model) {
  purrr::map(fitted_model, ~parsnip::extract_fit_engine(.x))
}


predict_values <- function(extracted_model,
                           DF) {
  
  extracted_model |>
    purrr::map_dfr(~{
      DF |>
        dplyr::select(Sale_Price) |>
        dplyr::bind_cols(predict_value = predict(.x, newdata = DF))
    },
    .id = "model")
}
