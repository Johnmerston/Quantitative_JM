# Load required libraries
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)  # Added for data manipulation

# Path to the Excel file (update this path to match your system)
file_path <- "C:\\JOHN\\OTHERS\\christ\\STUDENTS\\Sharath\\emission results.xlsx"

# Read data from sheet "Sheet2"
em_data <- read_xlsx(file_path, sheet = "Sheet2")

# Ensure data is a dataframe and convert relevant columns to numeric
Test_df <- em_data %>%
  mutate(across(starts_with("Y"), as.numeric),  # Convert Y columns to numeric
         X = as.numeric(X))  # Convert X column to numeric

# Convert to long format with pivot_longer
Test_df_smooth_long <- Test_df %>%
  pivot_longer(
    cols = ends_with("0"),
    names_to = "Series",
    values_to = "Intensity"
  ) %>%
  filter(Intensity != 0)  # Remove rows where Intensity is 0

# Remove the "_smooth" suffix from Series names for legend labels
Test_df_smooth_long <- Test_df_smooth_long %>%
  mutate(Series = gsub("_smooth", "", Series)) %>%
  mutate(Series = factor(Series, levels = unique(Series)))  # Ensure correct order in legend

# Create the plot with updated legend labels
ggplot(Test_df_smooth_long, aes(x = X, y = Intensity, color = Series)) +
  geom_line() +
  labs(
    title = "Emission Spectra",
    x = expression("Wavelength"),
    y = "Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center align the title
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # Tilt x-axis labels
  )
