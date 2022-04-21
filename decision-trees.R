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