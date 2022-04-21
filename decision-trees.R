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

