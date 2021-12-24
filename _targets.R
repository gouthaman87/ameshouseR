library(targets)

file_functions = list.files(path = "R", full.names = TRUE)

sapply(file_functions, source)

# tar_option_set(
#   packages = c("treesnip")
# )

list(
  # Read Data ----
  tar_target(name = raw_data,
             read_from_path(path = "data/ames.rda")),

  # Split Data ----
  tar_target(name = train_test_split,
             data_split(DF = raw_data,
                        prop = 0.8,
                        resp = "Sale_Price")),

  # Recipe ----
  tar_target(name = model_recipe,
             data_recipe(DF = rsample::training(train_test_split),
                         features = c("Neighborhood", "Gr_liv_Area", "Year_Built", "Bldg_Type"))),

  # Model workflow sets ----
  tar_target(name = models_workflowset,
             model_workflow(DF = rsample::training(train_test_split),
                            model_name = c("lm"),
                            features = c("Neighborhood", "Gr_liv_Area", "Year_Built", "Bldg_Type")))

  # # Train the models ----
  # tar_target(name = fitted_models,
  #            model_fit(DF = rsample::training(train_test_split),
  #                      model_set = models_workflowset))

  # # Predict Values ----
  # tar_target(name = predict_data,
  #            predict_values(DF = rsample::testing(train_test_split),
  #                           model_fit = fitted_models))
)
