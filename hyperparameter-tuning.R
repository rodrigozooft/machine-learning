# Fit a linear model on the breast_cancer_data.
linear_model <- lm(concavity_mean ~ symmetry_mean, data = breast_cancer_data)

# Look at the summary of the linear_model.
summary(linear_model)

# Extract the coefficients.
linear_model$coefficients

library(ggplot2)

# Plot linear relationship.
ggplot(data = breast_cancer_data, 
        aes(x = symmetry_mean, y = concavity_mean)) +
  geom_point(color = "grey") +
  geom_abline(slope = linear_model$coefficients[2], 
      intercept = linear_model$coefficients[1])