# Fit lm model: model
model <- lm(price ~ ., data = diamonds)

# Predict on full data: p
p <- predict(model, diamonds, type = "response")


# Compute errors: error
error <- p - diamonds$price

# Calculate RMSE

sqrt(mean(error ^ 2))

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(diamonds))

# Randomly order data
shuffled_diamonds <- diamonds[rows, ]

# Determine row to split on: split
split <- round(nrow(diamonds) * 0.8)

# Create train
train <- diamonds[1:split, ]

# Create test
test <- diamonds[(split + 1):nrow(diamonds), ] 

# Fit lm model on train: model
model <- lm(price ~ ., data = train)

# Predict on test: p
p <- predict(model, test, type = "response")

# Compute errors: error
error <- p - test$price

# Calculate RMSE
sqrt(mean(error ^ 2))

# Fit lm model using 10-fold CV: model
model <- train(
  price ~ ., 
  diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
