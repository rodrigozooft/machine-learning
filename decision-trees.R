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

# Create a split with a constant outcome distribution
diabetes_split <- initial_split(diabetes, prop = 0.75, strata = outcome)

# Proportion of 'yes' outcomes in the training data
counts_train <- table(training(diabetes_split)$outcome)
prop_yes_train <- counts_train["yes"] / sum(counts_train)

# Proportion of 'yes' outcomes in the test data
counts_test <- table(testing(diabetes_split)$outcome)
prop_yes_test <- counts_test["yes"] / sum(counts_test)

paste("Proportion of positive outcomes in training set:", round(prop_yes_train, 2))
paste("Proportion of positive outcomes in test set:", round(prop_yes_test, 2))

set.seed(9)

# Create the balanced data split
diabetes_split <- initial_split(diabetes, prop = 0.75, strata = outcome)

# Build the specification of the model
tree_spec <- decision_tree() %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

# Train the model
model_trained <- tree_spec %>% 
  fit(outcome ~ bmi + skin_thickness, 
      data = training(diabetes_split))

model_trained

# Train your model
model <- tree_spec %>% 
  fit(outcome ~., data = diabetes_train)

# Generate predictions
predictions <- predict(model,
                   diabetes_test)

# Add the true outcomes
predictions_combined <- predictions %>% 
  mutate(true_class = diabetes_test$outcome)

# Print the first lines of the result
head(predictions_combined)

# The confusion matrix
diabetes_matrix <- conf_mat(predictions_combined,
                       estimate = .pred_class,
                       truth = true_class)

# Print the matrix
diabetes_matrix

# The number of correctly predicted classes
correct_predictions <- 84 + 24

# The number of all predicted classes
all_predictions <- 84 + 24 + 28 + 17

# The accuracy calculated by hand
acc_manual <- correct_predictions / all_predictions
acc_manual

# The accuracy calculated by a function
acc_auto <- accuracy(predictions_combined,
                estimate = .pred_class, 
                truth = true_class)

acc_auto$.estimate

library(tidymodels)

# Build the specification
model_spec <- decision_tree() %>%
  set_mode('regression') %>%
  set_engine('rpart')

# Fit to the data
model_fit <- model_spec %>%
  fit(formula = final_grade ~ cocoa_percent + review_date,
      data = chocolate_train)

model_fit