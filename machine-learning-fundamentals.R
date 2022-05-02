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