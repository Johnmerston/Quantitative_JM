library(nlme)
library(mgcv)
library(readxl)

IC50_raw <- read_xlsx("File_name.xlsx")
head(IC50_raw)

concentration <- IC50_raw$Concentrations
inhibition <- IC50_raw$Inhibition

# Create a data frame if not already in that format
data <- data.frame(concentration, inhibition)


# Fit a GAM
gam_model <- gam(inhibition ~ s(concentration, bs = "cs", k=5), data = data)
summary(gam_model)


# Fit a logistic regression model
fit <- nls(inhibition ~ SSlogis(concentration, Asym, xmid, scal), data = data)

# Extrapolate to predict inhibition at higher concentrations
new_concentrations <- seq(10, 100, by = 10)  # Example new concentrations
predicted_inhibition <- predict(fit, newdata = data.frame(concentration = new_concentrations))

# Plot observed data and extrapolated predictions
plot(data$concentration, data$inhibition, pch = 16, xlab = "Concentration", ylab = "Inhibition")
lines(new_concentrations, predicted_inhibition, col = "red", lwd = 2)
