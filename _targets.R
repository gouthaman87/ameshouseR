library(targets)

file_functions = list.files(path = "ameshouseR/R", full.names = TRUE)

sapply(file_functions, source)

# tar_option_set(
#   packages = c("treesnip")
# )

list(
  # Read Data ----
  tar_target(name = raw_data,
             read_from_path(path = "ameshouseR/data/ames.rda")),
  
  # Split Data ----
  tar_target(name = train_test_split,
             data_split(DF = raw_data,
                        prop = 0.8,
                        resp = "Sale_Price")),
  
  # Train the models ----
  tar_target(name = fitted_models,
             fit_model(DF = rsample::training(train_test_split), 
                       model_name = c("lm", "stan", "dt"))),
  
  # Predict Values ----
  tar_target(name = predict_data,
             extract_fit(fitted_model = fitted_models) |> 
               predict_values(DF = rsample::testing(train_test_split)))
)