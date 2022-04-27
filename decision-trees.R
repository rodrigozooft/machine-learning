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

# Train the model
chocolate_model <- model_spec %>%
  fit(final_grade ~ ., data = chocolate_train)

# Predict new data
predictions <- predict(chocolate_model,
                       chocolate_test) %>%
  # Add the test set
  bind_cols(chocolate_test)

predictions

# Predict using the training set
in_sample_predictions <- predict(model,
                                 chocolate_train)

# Calculate the vector of absolute differences
abs_diffs <- abs(chocolate_train$final_grade - in_sample_predictions$.pred)

# Calculate the mean absolute error
1 / nrow(chocolate_train) * sum(abs_diffs)

# Predict ratings on test set and add true grades
test_enriched <- predict(model, new_data = chocolate_test) %>%
    bind_cols(chocolate_test)
    
# Compute the mean absolute error using one single function
mae(test_enriched,
    estimate = .pred,
    truth = final_grade)

# Calculate the squared differences
squared_diffs <- (test_enriched$final_grade - test_enriched$.pred)^2

# Compute the RMSE using the formula
rmse_manual <- sqrt(1 / nrow(test_enriched) * sum(squared_diffs))

# Compute the RMSE using a function
rmse_auto <- rmse(test_enriched,
                 estimate = .pred,
                 truth = final_grade)

# Print both errors
rmse_manual
rmse_auto

# Set seed for reproducibility
set.seed(20)

# Build 10 folds
chocolate_folds <- vfold_cv(chocolate_train, v = 10)

chocolate_folds

# Create a specification
tree_spec <- decision_tree() %>%
    set_engine('rpart') %>%
    set_mode('regression')

# Fit all folds to the specification
fits_cv <- fit_resamples(tree_spec,
               final_grade ~ .,
               resamples = chocolate_folds,
               metrics = metric_set(mae,rmse))

fits_cv

library(ggplot2)

# Collect the errors
all_errors <- collect_metrics(fits_cv, summarize = FALSE)

# Plot an error histogram
ggplot(all_errors, aes(x = .estimate, fill = .metric)) +
        geom_histogram()

# Collect and print error statistics
collect_metrics(fits_cv, summarize = TRUE)

# Create a model that can grow arbitrarily complex
chocolate_model <- decision_tree(cost_complexity = 0,
			  					 min_n = 2) %>%
		set_mode("regression") %>%
		set_engine("rpart") %>% 
		fit(final_grade ~ ., data = chocolate_train)
chocolate_model

# Predict on and combine with test data and calculate the error
predict(complex_model, new_data = chocolate_test) %>%
	bind_cols(chocolate_test) %>% 
	mae(estimate = .pred,
        truth = final_grade)

# Create a specification with tuning placeholders
tune_spec <- decision_tree(tree_depth = tune(),
                           cost_complexity = tune()) %>% 
  # Specify mode
  set_mode("classification") %>%
  # Specify engine
  set_engine("rpart") 

# Create a regular grid
tree_grid <- grid_regular(parameters(tune_spec),
                 levels = 2)

tree_grid

set.seed(275)

# Create CV folds of the customers tibble
folds <- vfold_cv(customers, v = 3)

# Tune along the grid
tune_results <- tune_grid(tune_spec, 
                          still_customer ~ .,
                          resamples = folds,
                          grid = tree_grid,
                          metrics = metric_set(accuracy))

# Plot the tuning results
autoplot(tune_results) 

# Select the parameters that perform best
final_params <- select_best(tune_results)

# Finalize the specification
best_spec <- finalize_model(tune_spec, final_params)

# Build the final model
final_model <- fit(best_spec,
                   still_customer ~ .,
                   data = customers)

final_model

# Create CV folds of the training data
folds <- vfold_cv(customers_train, v = 3)

# Calculate CV specificity
specificities <- fit_resamples(tree_spec, 
                     still_customer ~ .,
                     resamples = folds,
                     metrics = metric_set(spec))

# Collect the metrics
collect_metrics(specificities)

# Predict probabilities on test set
predictions <- predict(model, 
                       customers_test, 
                       type = "prob") %>% 
  # Add test set
  bind_cols(customers_test)

# Calculate the ROC curve for all thresholds
roc <- roc_curve(predictions,
           estimate = .pred_yes, 
           truth = still_customer)

# Plot the ROC curve
autoplot(roc)

# Calculate area under the curve
auc_result <- roc_auc(predictions, 
                  estimate = .pred_yes, 
                  truth = still_customer)

print(paste("The area under the ROC curve is", round(auc_result$.estimate, 3)))

# Create the specification
spec_bagged <- baguette::bag_tree() %>%
  set_mode("classification") %>%
  set_engine("rpart", times = 20)

# Fit to the training data
model_bagged <- fit(spec_bagged,
                    still_customer ~ total_trans_amt + customer_age + education_level, 
                    customers_train)

# Print the model
model_bagged

# Predict on training set and add to training set
predictions <- predict(model_bagged,
                   new_data = customers_train, 
                   type = "prob") %>% 
    bind_cols(customers_train)

# Create and plot the ROC curve
roc_curve(predictions,
          estimate = .pred_yes,
          truth = still_customer) %>% autoplot()

# Calculate the AUC
roc_auc(predictions,
    estimate = .pred_yes, 
    truth = still_customer)

set.seed(55)

# Estimate AUC using cross-validation
cv_results <- fit_resamples(spec_bagged,
                            still_customer ~ total_trans_amt + customer_age + education_level, 
                            resamples = vfold_cv(customers_train, v = 3),
                            metrics = metric_set(roc_auc))

# Collect metrics
collect_metrics(cv_results)

# Specify a random forest
spec <- rand_forest() %>%
	set_mode("classification") %>%
    set_engine("ranger", importance = "impurity")

# Train the forest
model <- spec %>%
    fit(still_customer ~ .,
        customers_train)

# Plot the variable importance
vip::vip(model)

# Specify the model class
boost_spec <- boost_tree() %>%
	# Set the mode
	set_mode("classification") %>%
	# Set the engine
	set_engine("xgboost")

boost_spec

# Train the model on the training set
boost_model <- fit(boost_spec,
                   still_customer ~ .,
                   customers_train)

boost_model

set.seed(99)

# Create CV folds
folds <- vfold_cv(customers_train, v = 5)

# Fit and evaluate models for all folds
cv_results <- fit_resamples(boost_spec,
                            still_customer ~ . ,
                            resamples = folds,
                            metrics = metric_set(roc_auc))

# Collect cross-validated metrics
collect_metrics(cv_results)

set.seed(100)

# Specify, fit, predict, and combine with training data
predictions <- boost_tree() %>%
  set_mode("classification") %>%
  set_engine("xgboost") %>% 
  fit(still_customer ~ ., data = customers_train) %>%
  predict(new_data = customers_train, type = "prob") %>% 
  bind_cols(customers_train)


# Calculate AUC
roc_auc(predictions, 
        truth = still_customer, 
        estimate = .pred_yes)

set.seed(100)

# Specify, fit, predict and combine with training data
predictions <- decision_tree() %>%
  set_mode("classification") %>%
  set_engine("rpart") %>% 
  fit(still_customer ~ ., data = customers_train) %>%
  predict(new_data = customers_train, type = "prob") %>% 
  bind_cols(customers_train)

# Calculate AUC
roc_auc(predictions, 
        truth = still_customer,
        estimate = .pred_yes)