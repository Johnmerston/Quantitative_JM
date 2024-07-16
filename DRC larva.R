# Load necessary libraries
library(drc)
library(ggplot2)

# Define the data
concentration <- c(0, 0.5, 1, 2, 4)
inhibition <- c(0, 33.33, 50, 83.33, 100)

# Create a data frame
data <- data.frame(concentration, inhibition)

# Fit the 4PL model
model <- drm(inhibition ~ concentration, data = data, fct = LL.4())

# Display the summary of the model
summary(model)

# Extract the IC50 value
IC50 <- ED(model, 50)
print(IC50)

IC90 <- ED(model, 90)
print(IC90)

# Create the fitted values for the plot
new_data <- data.frame(concentration = seq(min(concentration), max(concentration), length.out = 100))
new_data$inhibition <- predict(model, new_data)

# Plot the data and the fitted curve using ggplot2
ggplot(data, aes(x = concentration, y = inhibition)) +
  geom_point(size = 3) +
  geom_line(data = new_data, aes(x = concentration, y = inhibition), color = "blue", size = 1) +
  scale_x_log10() +
  labs(title = "Dose-Response Curve",
       x = "Concentration",
       y = "Inhibition (%)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))
