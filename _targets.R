library(targets)

file_functions = list.files(path = "R", full.names = TRUE)

sapply(file_functions, source)

tar_option_set(
  packages = c("tidyverse", "tidymodels", "treesnip")
)

list(
  # Read Data ----
  tar_target(name = raw_data,
             read_from_path(path = "data/ames.rda")),

  tar_target(name = param_file,
             "params.yaml",
             format = "file"),

  tar_target(name = params,
             yaml::read_yaml(param_file)),

  # Split Data ----
  tar_target(name = train_test_split,
             data_split(DF = raw_data,
                        prop = 0.8,
                        resp = "Sale_Price")),

  # Recipe ----
  tar_target(name = model_recipe,
             data_recipe(DF = rsample::training(train_test_split),
                         features = params[["features"]])),

  # Model workflow sets ----
  tar_target(name = models_workflowset,
             model_workflow(DF = rsample::training(train_test_split),
                            model_name = "lgbm",
                            features = params[["features"]])),

  # Train the models ----
  tar_target(name = fitted_models,
             model_fit(DF = rsample::training(train_test_split),
                       type_of_resample = "cv",
                       model_set = models_workflowset))

  # # Asses the models ----
  # tar_target(name = train_metrics,
  #            tune::collect_metrics(fitted_models, summarize = FALSE) |>
  #              dplyr::filter(.metric == "rmse")),
  #
  # # Predict Values ----
  # tar_target(name = predict_data,
  #            predict_values(DF = rsample::testing(train_test_split),
  #                           model_fit = fitted_models))
)
