# Explore gapminder
head(gapminder, 6)

# Prepare the nested data frame gap_nested
library(tidyverse)
gap_nested <- gapminder %>% 
  group_by(country) %>% 
  nest()

# Explore gap_nested
head(gap_nested, 6)

# Create the unnested data frame called gap_unnnested
gap_unnested <- gap_nested %>% 
  unnest()
  
# Confirm that your data was not modified  
identical(gapminder, gap_unnested)

# Extract the data of Algeria
algeria_df <- gap_nested$data[[1]]

# Calculate the minimum of the population vector
min(algeria_df$population)

# Calculate the maximum of the population vector
max(algeria_df$population)

# Calculate the mean of the population vector
mean(algeria_df$population)

# Calculate the mean population for each country
pop_nested <- gap_nested %>%
  mutate(mean_pop = map(.x = data, ~mean(.x$population)))

# Take a look at pop_nested
head(pop_nested)

# Extract the mean_pop value by using unnest
pop_mean <- pop_nested %>% 
  unnest(mean_pop)

# Take a look at pop_mean
head(pop_mean)

# Calculate mean population and store result as a double
pop_mean <- gap_nested %>%
  mutate(mean_pop = map_dbl(.x = data, ~mean(.x$population)))

# Take a look at pop_mean
head(pop_mean)

# Build a linear model for each country
gap_models <- gap_nested %>%
    mutate(model = map(.x = data, ~lm(formula = life_expectancy~year, data = .x)))
    
# Extract the model for Algeria    
algeria_model <- gap_models$model[[1]]

# View the summary for the Algeria model
summary(algeria_model)

library(broom)

# Extract the coefficients of the algeria_model as a data frame
tidy(algeria_model)

# Extract the statistics of the algeria_model as a data frame
glance(algeria_model)

# Build the augmented data frame
algeria_fitted <- augment(algeria_model)

# Compare the predicted values with the actual values of life expectancy
algeria_fitted %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = life_expectancy)) + 
  geom_line(aes(y = .fitted), color = "red")

# Extract the coefficient statistics of each model into nested data frames
model_coef_nested <- gap_models %>% 
    mutate(coef = map(model, ~tidy(.x)))
    
# Simplify the coef data frames for each model    
model_coef <- model_coef_nested %>%
    unnest(coef)

# Plot a histogram of the coefficient estimates for year         
model_coef %>% 
  filter(term == "year") %>% 
  ggplot(aes(x = estimate)) +
  geom_histogram()

# Extract the fit statistics of each model into data frames
model_perf_nested <- gap_models %>% 
    mutate(fit = map(model, ~glance(.x)))

# Simplify the fit data frames for each model    
model_perf <- model_perf_nested %>% 
    unnest(fit)
    
# Look at the first six rows of model_perf
head(model_perf)

# Plot a histogram of rsquared for the 77 models    
model_perf %>% 
  ggplot(aes(x = r.squared)) + 
  geom_histogram()  
  
# Extract the 4 best fitting models
best_fit <- model_perf %>% 
  top_n(n = 4, wt = r.squared)

# Extract the 4 models with the worst fit
worst_fit <- model_perf %>% 
  top_n(n = 4, wt = -r.squared)

best_augmented <- best_fit %>% 
  # Build the augmented data frame for each country model
  mutate(augmented = map(model, ~augment(.x))) %>% 
  # Expand the augmented data frames
  unnest(augmented)

worst_augmented <- worst_fit %>% 
  # Build the augmented data frame for each country model
  mutate(augmented = map(model, ~augment(.x))) %>% 
  # Expand the augmented data frames
  unnest(augmented)

# Compare the predicted values with the actual values of life expectancy 
# for the top 4 best fitting models
best_augmented %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = life_expectancy)) + 
  geom_line(aes(y = .fitted), color = "red") +
  facet_wrap(~country, scales = "free_y")

# Compare the predicted values with the actual values of life expectancy 
# for the top 4 worst fitting models
worst_augmented %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = life_expectancy)) + 
  geom_line(aes(y = .fitted), color = "red") +
  facet_wrap(~country, scales = "free_y")
  
# Build a linear model for each country using all features
gap_fullmodel <- gap_nested %>% 
  mutate(model = map(data, ~lm(formula = life_expectancy ~ ., data = .x)))

fullmodel_perf <- gap_fullmodel %>% 
  # Extract the fit statistics of each model into data frames
  mutate(fit = map(model, ~glance(.x))) %>% 
  # Simplify the fit data frames for each model
  unnest(fit)
  
# View the performance for the four countries with the worst fitting 
# four simple models you looked at before
fullmodel_perf %>% 
  filter(country %in% worst_fit$country) %>% 
  select(country, adj.r.squared)

set.seed(42)

# Prepare the initial split object
gap_split <- initial_split(gapminder, prop = 0.75)

# Extract the training data frame
training_data <- training(gap_split)

# Extract the testing data frame
testing_data <- testing(gap_split)

# Calculate the dimensions of both training_data and testing_data
dim(training_data)
dim(testing_data)

set.seed(42)

# Prepare the data frame containing the cross validation partitions
cv_split <- vfold_cv(training_data, v = 5)

cv_data <- cv_split %>% 
  mutate(
    # Extract the train data frame for each split
    train = map(.x = splits, ~training(.x)), 
    # Extract the validate data frame for each split
    validate = map(.x = splits, ~testing(.x))
  )

# Use head() to preview cv_data
head(cv_data)

# Build a model using the train data for each fold of the cross validation
cv_models_lm <- cv_data %>% 
  mutate(model = map(.x = train, ~lm(formula = life_expectancy ~ ., data = .x)))

cv_prep_lm <- cv_models_lm %>% 
  mutate(
    # Extract the recorded life expectancy for the records in the validate data frames
    validate_actual = map(validate, ~.x$life_expectancy),
    # Predict life expectancy for each validate set using its corresponding model
    validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y))
  )

library(Metrics)
# Calculate the mean absolute error for each validate fold       
cv_eval_lm <- cv_prep_lm %>% 
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, ~mae(actual = .x, predicted = .y)))

# Print the validate_mae column
cv_eval_lm$validate_mae

# Calculate the mean of validate_mae column
mean(cv_eval_lm$validate_mae)

library(ranger)

# Build a random forest model for each fold
cv_models_rf <- cv_data %>% 
  mutate(model = map(train, ~ranger(formula = life_expectancy ~ ., data = .x,
                                    num.trees = 100, seed = 42)))

# Generate predictions using the random forest model
cv_prep_rf <- cv_models_rf %>% 
  mutate(validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

library(ranger)

# Calculate validate MAE for each fold
cv_eval_rf <- cv_prep_rf %>% 
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, ~mae(actual = .x, predicted = .y)))

# Print the validate_mae column
cv_eval_rf$validate_mae

# Calculate the mean of validate_mae column
mean(cv_eval_rf$validate_mae)

# Prepare for tuning your cross validation folds by varying mtry
cv_tune <- cv_data %>% 
  crossing(mtry = 2:5) 

# Build a model for each fold & mtry combination
cv_model_tunerf <- cv_tune %>% 
  mutate(model = map2(.x = train, .y = mtry, ~ranger(formula = life_expectancy~., 
                                           data = .x, mtry = .y, 
                                           num.trees = 100, seed = 42)))

# Generate validate predictions for each model
cv_prep_tunerf <- cv_model_tunerf %>% 
  mutate(validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

# Calculate validate MAE for each fold and mtry combination
cv_eval_tunerf <- cv_prep_tunerf %>% 
  mutate(validate_mae = map2_dbl(.x = validate_actual, .y = validate_predicted, ~mae(actual = .x, predicted = .y)))

# Calculate the mean validate_mae for each mtry used  
cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_mae = mean(validate_mae))

# Build the model using all training data and the best performing parameter
best_model <- ranger(formula = life_expectancy ~ ., data = training_data,
                     mtry = 4, num.trees = 100, seed = 42)

# Prepare the test_actual vector
test_actual <- testing_data$life_expectancy

# Predict life_expectancy for the testing_data
test_predicted <- predict(best_model, testing_data)$predictions

# Calculate the test MAE
mae(test_actual, test_predicted)

set.seed(42)

# Prepare the initial split object
data_split <- initial_split(attrition, prop = 0.75)

# Extract the training data frame
training_data <- training(data_split)

# Extract the testing data frame
testing_data <- testing(data_split)

set.seed(42)
cv_split <- vfold_cv(training_data, v = 5)

cv_data <- cv_split %>% 
  mutate(
    # Extract the train data frame for each split
    train = map(splits, ~training(.x)),
    # Extract the validate data frame for each split
    validate = map(splits, ~testing(.x))
  )

# Build a model using the train data for each fold of the cross validation
cv_models_lr <- cv_data %>% 
  mutate(model = map(train, ~glm(formula = Attrition ~ ., 
                               data = .x, family = "binomial")))

# Extract the first model and validate 
model <- cv_models_lr$model[[1]]
validate <- cv_models_lr$validate[[1]]

# Prepare binary vector of actual Attrition values in validate
validate_actual <- validate$Attrition == "Yes"

# Predict the probabilities for the observations in validate
validate_prob <- predict(model, validate, type = "response")

# Prepare binary vector of predicted Attrition values for validate
validate_predicted <- validate_prob > 0.5

library(Metrics)

# Compare the actual & predicted performance visually using a table
table(validate_actual, validate_predicted)

# Calculate the accuracy
accuracy(validate_actual, validate_predicted)

# Calculate the precision
precision(validate_actual, validate_predicted)

# Calculate the recall
recall(validate_actual, validate_predicted)

cv_prep_lr <- cv_models_lr %>% 
  mutate(
    # Prepare binary vector of actual Attrition values in validate
    validate_actual = map(validate, ~.x$Attrition == "Yes"),
    # Prepare binary vector of predicted Attrition values for validate
    validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y, type = "response") > 0.5)
  )

# Calculate the validate recall for each cross validation fold
cv_perf_recall <- cv_prep_lr %>% 
  mutate(validate_recall = map2_dbl(validate_actual, validate_predicted, 
                                    ~recall(actual = .x, predicted = .y)))

# Print the validate_recall column
cv_perf_recall$validate_recall

# Calculate the average of the validate_recall column
mean(cv_perf_recall$validate_recall)

library(ranger)

# Prepare for tuning your cross validation folds by varying mtry
cv_tune <- cv_data %>%
  crossing(mtry = c(2,4,8,16)) 

# Build a cross validation model for each fold & mtry combination
cv_models_rf <- cv_tune %>% 
  mutate(model = map2(train, mtry, ~ranger(formula = Attrition~., 
                                           data = .x, mtry = .y,
                                           num.trees = 100, seed = 42)))

cv_prep_rf <- cv_models_rf %>% 
  mutate(
    # Prepare binary vector of actual Attrition values in validate
    validate_actual = map(validate, ~.x$Attrition == "Yes"),
    # Prepare binary vector of predicted Attrition values for validate
    validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y, type = "response")$predictions == "Yes")
  )

# Calculate the validate recall for each cross validation fold
cv_perf_recall <- cv_prep_rf %>% 
  mutate(recall = map2_dbl(.x = validate_actual, .y = validate_predicted, ~recall(actual = .x, predicted = .y)))

# Calculate the mean recall for each mtry used  
cv_perf_recall %>% 
  group_by(mtry) %>% 
  summarise(mean_recall = mean(recall))

# Build the logistic regression model using all training data
best_model <- glm(formula = Attrition ~ ., 
                  data = training_data, family = "binomial")


# Prepare binary vector of actual Attrition values for testing_data
test_actual <- testing_data$Attrition == "Yes"

# Prepare binary vector of predicted Attrition values for testing_data
test_predicted <- predict(best_model, testing_data, type = "response") > 0.5

# Compare the actual & predicted performance visually using a table
table(test_actual, test_predicted)

# Calculate the test accuracy
accuracy(test_actual, test_predicted)

# Calculate the test precision
precision(test_actual, test_predicted)

# Calculate the test recall
recall(test_actual, test_predicted)

# Create a data split object
home_split <- initial_split(home_sales, 
                            prop = 0.7, 
                            strata = selling_price)

# Create the training data
home_training <- home_split %>%
  training()

# Create the test data
home_test <- home_split %>% 
  testing()

# Check number of rows in each dataset
nrow(home_training)
nrow(home_test)

# Distribution of selling_price in test data
home_test %>% 
  summarize(min_sell_price = min(selling_price),
            max_sell_price = max(selling_price),
            mean_sell_price = mean(selling_price),
            sd_sell_price = sd(selling_price))

# Specify a linear regression model, linear_model
linear_model <- linear_reg() %>% 
  # Set the model engine
  set_engine('lm') %>% 
  # Set the model mode
  set_mode('regression')

# Train the model with the training data
lm_fit <- linear_model %>% 
  fit(selling_price ~ home_age + sqft_living,
      data = home_training)

# Print lm_fit to view model information
lm_fit

# Predict selling_price
home_predictions <- predict(lm_fit,
                        new_data = home_test)

# View predicted selling prices
home_predictions

# Combine test data with predictions
home_test_results <- home_test %>% 
  select(selling_price, home_age, sqft_living) %>% 
  bind_cols(home_predictions)

# View results
home_test_results

# Print home_test_results
home_test_results

# Caculate the RMSE metric
home_test_results %>% 
  rmse(selling_price, .pred)

# Calculate the R squared metric
home_test_results %>% 
  rsq(selling_price, .pred)

# Create an R squared plot of model performance
ggplot(home_test_results, aes(x = selling_price, y = .pred)) +
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred() +
  labs(x = 'Actual Home Selling Price', y = 'Predicted Selling Price')

# Define a linear regression model
linear_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

# Train linear_model with last_fit()
linear_fit <- linear_model %>% 
  last_fit(selling_price ~ ., split = home_split)

# Collect predictions and view results
predictions_df <- linear_fit %>% collect_predictions()
predictions_df
                                        
# Make an R squared plot using predictions_df
ggplot(predictions_df, aes(x = selling_price, y = .pred)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred() +
  labs(x = 'Actual Home Selling Price', y = 'Predicted Selling Price')

# Create data split object
telecom_split <- initial_split(telecom_df, prop = 0.75,
                     strata = canceled_service)

# Create the training data
telecom_training <- telecom_split %>% 
  training()

# Create the test data
telecom_test <- telecom_split %>% 
  testing()

# Check the number of rows
nrow(telecom_training)
nrow(telecom_test)

# Specify a logistic regression model
logistic_model <- logistic_reg() %>% 
  # Set the engine
  set_engine('glm') %>% 
  # Set the mode
  set_mode('classification')

# Fit to training data
logistic_fit <- logistic_model %>% 
  fit(canceled_service ~ avg_call_mins + avg_intl_mins + monthly_charges,
      data = telecom_training)

# Print model fit object
logistic_fit

# Predict outcome categories
class_preds <- predict(logistic_fit, new_data = telecom_test,
                       type = 'class')

# Obtain estimated probabilities for each outcome value
prob_preds <- predict(logistic_fit, new_data = telecom_test, 
                      type = 'prob')

# Combine test set results
telecom_results <- telecom_test %>% 
  select(canceled_service) %>% 
  bind_cols(class_preds, prob_preds)

# View results tibble
telecom_results

# Create a custom metric function
telecom_metrics <- metric_set(accuracy, sens, spec)

# Calculate metrics using model results tibble
telecom_metrics(telecom_results, truth = canceled_service,
                estimate = .pred_class)

# Create a confusion matrix
conf_mat(telecom_results,
         truth = canceled_service,
         estimate = .pred_class) %>% 
  # Pass to the summary() function
  summary()

  # Create a confusion matrix
conf_mat(telecom_results,
         truth = canceled_service,
         estimate = .pred_class) %>% 
  # Create a mosaic plot
  autoplot(type = 'mosaic')

# Calculate metrics across thresholds
threshold_df <- telecom_results %>% 
  roc_curve(truth = canceled_service, .pred_yes)

# View results
threshold_df

# Plot ROC curve
threshold_df %>% 
  autoplot()

# Calculate ROC AUC
roc_auc(telecom_results,
    truth = canceled_service, 
    .pred_yes)

# Train model with last_fit()
telecom_last_fit <- logistic_model %>% 
  last_fit(canceled_service ~ avg_call_mins + avg_intl_mins + monthly_charges,
           split = telecom_split)

# View test set metrics
telecom_last_fit %>% 
  collect_metrics()

# Collect predictions
last_fit_results <- telecom_last_fit %>% 
  collect_predictions()

# View results
last_fit_results

# Custom metrics function
last_fit_metrics <- metric_set(accuracy, sens,
                               spec, roc_auc)

# Calculate metrics
last_fit_metrics(last_fit_results,
                 truth = canceled_service,
                 estimate = .pred_class,
                 .pred_yes)

# Train a logistic regression model
logistic_fit <- logistic_model %>% 
  last_fit(canceled_service ~ avg_call_mins + avg_intl_mins + monthly_charges + months_with_company, 
           split = telecom_split)

# Collect metrics
logistic_fit %>% 
  collect_metrics()

# Collect model predictions
logistic_fit %>% 
  collect_predictions() %>% 
  # Plot ROC curve
  roc_curve(truth = canceled_service, .pred_yes) %>% 
  autoplot()

# Specify feature engineering recipe
telecom_log_rec <- recipe(canceled_service ~ ., 
                          data = telecom_training) %>%
  # Add log transformation step
  step_log(avg_call_mins, avg_intl_mins, base = 10)

# View variable roles and data types
telecom_log_rec %>%
  summary()

# Train the telecom_log_rec object
telecom_log_rec_prep <- telecom_log_rec %>% 
  prep(training = telecom_training)

# Apply to test data
telecom_log_rec_prep %>% 
  bake(new_data = telecom_test)

telecom_training %>% 
  # Select numeric columns
  select_if(is.numeric) %>% 
  # Calculate correlation matrix
  cor()

# Plot correlated predictors
ggplot(telecom_training, aes(x = avg_data_gb, y = monthly_charges)) + 
  # Add points
  geom_point()  + 
  # Add title
  labs(title = 'Monthly Charges vs. Average Data Usage',
       y = 'Monthly Charges ($)', x = 'Average Data Usage (GB)') 

# Specify a recipe object
telecom_cor_rec <- recipe(canceled_service ~ .,
                          data = telecom_training) %>% 
  # Remove correlated variables
  step_corr(all_numeric(), threshold = 0.8)

# Train the recipe
telecom_cor_rec_prep <- telecom_cor_rec %>% 
  prep(training = telecom_training)

# Apply to training data
telecom_cor_rec_prep %>% 
  bake(new_data = NULL)

# Apply to test data
telecom_cor_rec_prep %>% 
  bake(new_data = telecom_test)

# Specify a recipe object
telecom_norm_rec <- recipe(canceled_service ~ .,
                          data = telecom_training) %>% 
  # Remove correlated variables
  step_corr(all_numeric(), threshold = 0.8) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric())

# Train the recipe
telecom_norm_rec_prep <- telecom_norm_rec %>% 
  prep(training = telecom_training)

# Apply to test data
telecom_norm_rec_prep %>% 
  bake(new_data = telecom_test)

telecom_recipe_2 <- 
  recipe(canceled_service ~ avg_data_gb + contract, data = telecom_training)  %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_normalize(all_numeric(), -all_outcomes())

# Train and apply telecom_recipe_2 on the test data
telecom_recipe_2 %>% 
  prep(telecom_training) %>% 
  bake(new_data = telecom_test)

# Create a recipe that predicts canceled_service using the training data
telecom_recipe <- recipe(canceled_service ~ ., data = telecom_training) %>% 
  # Remove correlated predictors
  step_corr(all_numeric(), threshold = 0.8) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric()) %>% 
  # Create dummy variables
  step_dummy(all_nominal(), -all_outcomes())

# Train your recipe and apply it to the test data
telecom_recipe %>% 
  prep(training = telecom_training) %>% 
  bake(new_data = telecom_test)

telecom_recipe <- recipe(canceled_service ~ ., data = telecom_training) %>% 
  # Removed correlated predictors
  step_corr(all_numeric(), threshold = 0.8) %>% 
  # Log transform numeric predictors
  step_log(all_numeric(), base = 10) %>%
  # Normalize numeric predictors
  step_normalize(all_numeric()) %>% 
  # Create dummy variables
  step_dummy(all_nominal(), -all_outcomes())

# Train recipe
telecom_recipe_prep <- telecom_recipe %>% 
  prep(training = telecom_training)

# Transform training data
telecom_training_prep <- telecom_recipe_prep %>% 
  bake(new_data = NULL)

# Transform test data
telecom_test_prep <- telecom_recipe_prep %>% 
  bake(new_data = telecom_test)

telecom_test_prep

# Train logistic model
logistic_fit <- logistic_model %>% 
  fit(canceled_service ~ ., data = telecom_training_prep)

# Obtain class predictions
class_preds <- predict(logistic_fit, new_data = telecom_test_prep,
                       type = 'class')

# Obtain estimated probabilities
prob_preds <- predict(logistic_fit, new_data = telecom_test_prep, 
                      type = 'prob')

# Combine test set results
telecom_results <- telecom_test_prep %>% 
  bind_cols(class_preds, prob_preds)

telecom_results

# Create a confusion matrix
telecom_results %>% 
  conf_mat(truth = canceled_service, estimate = .pred_class)

# Calculate sensitivity
telecom_results %>% 
  sens(truth = canceled_service, estimate = .pred_class)

# Calculate specificity
telecom_results %>% 
  spec(truth = canceled_service, estimate = .pred_class)

# Plot ROC curve
telecom_results %>% 
  roc_curve(truth = canceled_service, .pred_yes) %>% 
  autoplot()

# Create data split object
loans_split <- initial_split(loans_df, 
                             strata = loan_default)

# Build training data
loans_training <- loans_split %>% 
  training()

# Build test data
loans_test <- loans_split %>% 
  testing()

# Check for correlated predictors
loans_training %>% 
  # Select numeric columns
  select_if(is.numeric) %>%
  # Calculate correlation matrix
  cor()

dt_model <- decision_tree() %>% 
  # Specify the engine
  set_engine('rpart') %>% 
  # Specify the mode
  set_mode('classification')

# Build feature engineering pipeline
loans_recipe <- recipe(loan_default ~ .,
                        data = loans_training) %>% 
  # Correlation filter
  step_corr(all_numeric, threshold = 0.85) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric, all_predictors) %>% 
  # Create dummy variables
  step_dummy(all_nominal, -all_outcomes)

loans_recipe

# Create a workflow
loans_dt_wkfl <- workflow() %>% 
  # Include the model object
  add_model(dt_model) %>% 
  # Include the recipe object
  add_recipe(loans_recipe)

# Train the workflow
loans_dt_wkfl_fit <- loans_dt_wkfl %>% 
  last_fit(split = loans_split)

# Calculate performance metrics on test data
loans_dt_wkfl_fit %>% 
  collect_metrics()

# Create cross validation folds
set.seed(290)
loans_folds <- vfold_cv(loans_training, v = 5,
                       strata = loan_default)

# Create custom metrics function
loans_metrics <- metric_set(roc_auc, sens, spec)

# Fit resamples
loans_dt_rs <- loans_dt_wkfl %>% 
  fit_resamples(resamples = loans_folds,
                metrics = loans_metrics)

# View performance metrics
loans_dt_rs %>% 
  collect_metrics()

logistic_model <- logistic_reg() %>% 
  # Specify the engine
  set_engine('glm') %>% 
  # Specify the mode
  set_mode('classification')

# Create workflow
loans_logistic_wkfl <- workflow() %>% 
  # Add model
  add_model(logistic_model) %>% 
  # Add recipe
  add_recipe(loans_recipe)

# Fit resamples
loans_logistic_rs <- loans_logistic_wkfl %>% 
  fit_resamples(resamples = loans_folds,
                metrics = loans_metrics)

# View performance metrics
loans_logistic_rs %>% 
  collect_metrics()

# Detailed cross validation results
logistic_rs_results <- loans_logistic_rs %>% 
  collect_metrics(summarize = FALSE)

# Explore model performance for logistic regression
logistic_rs_results %>% 
  group_by(.metric) %>% 
  summarize(min = min(.estimate),
            median = median(.estimate),
            max = max(.estimate))

# Set tuning hyperparameters
dt_tune_model <- decision_tree(cost_complexity = tune(),
                               tree_depth = tune(),
                               min_n = tune()) %>% 
  # Specify engine
  set_engine('rpart') %>% 
  # Specify mode
  set_mode('classification')

# Create a tuning workflow
loans_tune_wkfl <- loans_dt_wkfl %>% 
  # Replace model
  update_model(dt_tune_model)

loans_tune_wkfl

# Hyperparameter tuning with grid search
set.seed(214)
dt_grid <- grid_random(parameters(dt_tune_model),
                       size = 5)

# Hyperparameter tuning
dt_tuning <- loans_tune_wkfl %>% 
  tune_grid(resamples = loans_folds,
            grid = dt_grid,
            metrics = loans_metrics)

# View results
dt_tuning %>% 
  collect_metrics

# Collect detailed tuning results
dt_tuning_results <- dt_tuning %>% 
  collect_metrics(summarize = FALSE)

# Explore detailed ROC AUC results for each fold
dt_tuning_results %>% 
  filter(.metric == 'roc_auc') %>% 
  group_by(id) %>% 
  summarize(min_roc_auc = min(.estimate),
            median_roc_auc = median(.estimate),
            max_roc_auc = max(.estimate))

# Display 5 best performing models
dt_tuning %>% 
  show_best(metric = 'roc_auc', n = 5)

# Select based on best performance
best_dt_model <- dt_tuning %>% 
  # Choose the best model based on roc_auc
  select_best(metric = 'roc_auc')

# Finalize your workflow
final_loans_wkfl <- loans_tune_wkfl %>% 
  finalize_workflow(best_dt_model)

final_loans_wkfl