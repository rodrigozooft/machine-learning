# Load the package
library(tidymodels)

# Pick a model class
tree_spec <- decision_tree() %>% 
  # Set the engine
  set_engine("rpart") %>% 
  # Set the mode
  set_mode('classification')

# Print the result
tree_spec

# Create the specification
tree_spec <- decision_tree() %>% 
  set_engine ("rpart") %>% 
  set_mode('classification')

# Train the model
tree_model_bmi <- tree_spec %>% 
  fit(formula = outcome ~ bmi, data = diabetes)

# Print the model
tree_model_bmi

# Create the split
diabetes_split <- initial_split(diabetes, prop = 0.8)

# Extract the training and test set
diabetes_train <- training(diabetes_split)
diabetes_test  <- testing(diabetes_split)

# Verify the proportions of both sets
round(nrow(diabetes_train) / nrow(diabetes), 2) == 0.80
round(nrow(diabetes_test) / nrow(diabetes), 2) == 0.20