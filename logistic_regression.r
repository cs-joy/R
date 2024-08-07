library(ggplot2)
library(dplyr)

set.seed(123) # For reproducibility
n <- 100
x <- rnorm(n)
y <- rbinom(n, 1, prob = 1 / (1 + exp(-x))) # Logistic function

data <- data.frame(x = x, y = y)

model <- glm(y ~ x, data = data, family = binomial)
summary(model)

data$predicted_prob <- predict(model, type = "response")

ggplot(data, aes(x = x, y = y)) +
  geom_point(aes(color = as.factor(y))) +
  geom_line(aes(y = predicted_prob), color = "blue") +
  labs(title = "Logistic Regression Plot",
       x = "Predictor (x)",
       y = "Probability",
       color = "Outcome") +
  theme_minimal()
