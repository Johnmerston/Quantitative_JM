# Install and load necessary package

library(splines)

# Fit a spline regression model
spline_model <- lm(inhibition ~ ns(concentration, df = 4), data = data)
summary(spline_model)

# Plot the data and the spline fit
plot(data$concentration, data$inhibition, log = "x", main = "Spline Regression Fit", xlab = "Concentration", ylab = "Inhibition (%)", pch = 19)
lines(data$concentration, predict(spline_model), col = "blue", lwd = 2)
