# 4A. Salary data: Salary vs Years of Experience
data <- data.frame(
  Years_Experience = c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7, 3.9, 4.0, 4.0, 4.1, 4.5, 4.9, 5.1, 5.3, 5.9, 6.0, 6.8, 7.1, 7.9, 8.2, 8.7, 9.0, 9.5, 9.6, 10.3, 10.5),
  Salary = c(39343, 46205, 37731, 43525, 39891, 56642, 60150, 54445, 64445, 57189, 63218, 55794, 56957, 57081, 61111, 67938, 66029, 83088, 81363, 93940, 91738, 98273, 101302, 113812, 109431, 105582, 116969, 112635, 122391, 121872)
)
plot(data)

# Fit the model and view the summary
model <- lm(Salary ~ Years_Experience, data = data)
summary(model)

# Plot the regression line
plot(data$Years_Experience, data$Salary, pch = 16, xlab = "Years of Experience", ylab = "Salary", main = "Salary vs Experience", col = "blue")
abline(model, col = "red", lwd = 2)

# Predict salary for new values
new_data <- data.frame(Years_Experience = c(4.0, 4.5, 5.0))
predicted_salaries <- predict(model, newdata = new_data)
print(predicted_salaries)


# 4B. mtcars data: mpg vs weight
summary(mtcars$wt)
model <- lm(mpg ~ wt, data = mtcars)
summary(model)

plot(mtcars$wt, mtcars$mpg, pch = 16, xlab = "Weight (1000 lbs)", ylab = "Miles per Gallon", main = "mpg vs weight", col = "blue")
abline(model, col = "red", lwd = 2)

# Residual plot
plot(fitted(model), resid(model), xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, lty = 2)
