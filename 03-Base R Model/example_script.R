data(crickets, package = "modeldata")

ggplot2::ggplot(data = crickets, ggplot2::aes(x = temp, y = rate, col = species)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = FALSE) +
  
  viridis::scale_color_viridis(discrete = TRUE) +
  ggplot2::labs(x = "Temperature (C)", y = "Chirp Rate (per minute)") +
  ggplot2::theme_minimal()

interaction_fit <- lm(rate ~ (temp + species)^2, data = crickets)

interaction_fit

par(mfrow = c(1,2))
# Show residuals vs predicted values:
plot(interaction_fit, which = 1)

# A normal quantile plot on the residuals:
plot(interaction_fit, which = 2)

main_effect_fit <- lm(rate ~ temp + species, data = crickets)

anova(main_effect_fit, interaction_fit)

summary(main_effect_fit)

new_values <- data.frame(species = "O. exclamationis", temp = 15:20)
predict(main_effect_fit, new_values)

# Add a missing value to the prediction set
new_values$temp[1] <- NA

# The predict method for `lm` defaults to `na.pass`:
predict(main_effect_fit, new_values)

# Alternatively 
predict(main_effect_fit, new_values, na.action = na.fail)

predict(main_effect_fit, new_values, na.action = na.omit)


# 3.2 ---------------------------------------------------------------------

data(mtcars, package = "datasets")
corr_res <- purrr::map(mtcars |> dplyr::select(-mpg), cor.test, y = mtcars$mpg)

corr_res |>
  purrr::map_dfr(broom::tidy, .id = "predictor") |>
  ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(predictor, estimate))) +
  ggplot2::geom_point(ggplot2::aes(y = estimate)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = .1) +
  ggplot2::labs(x = NULL, y = "Correlation with mpg") +
  ggplot2::theme_minimal()


# 3.4 ---------------------------------------------------------------------

split_by_species <- crickets |>
  dplyr::group_nest(species)

model_by_species <- split_by_species |>
  dplyr::mutate(model = purrr::map(data, ~lm(rate ~ temp, data = .x)))

model_by_species |>
  dplyr::mutate(coef = purrr::map(model, broom::tidy)) |>
  dplyr::select(species, coef) |>
  tidyr::unnest(cols = coef)
