# Fit lm model: model
model <- lm(price ~ ., data = diamonds)

# Predict on full data: p
p <- predict(model, diamonds, type = "response")


# Compute errors: error
error <- p - diamonds$price

# Calculate RMSE

sqrt(mean(error ^ 2))