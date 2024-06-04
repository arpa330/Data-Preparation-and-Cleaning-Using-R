
# BDA Data Cleaning

# Install the readr package (if not already installed)
install.packages("readr")
install.packages("dplyr")

# Load the readr package
library(readr)
library(dplyr)

# Load the CSV dataset 
df <- read.csv("filtered_BDA_assignment_data.csv")
# View the structure of the dataframe
str(df)

# Find missing values in the specific column
missing_values <- sum(is.na(df$`Kt/V Adult Hemodialysis Measure Score`))
percentage_missing <- (missing_values / nrow(df)) * 100

# Print the results
cat("Number of missing values:", missing_values, "\n")
cat("Percentage of missing values:", percentage_missing, "%\n")

# Compute the average (mean) of the specific column
average_column <- mean(df$`Kt/V Adult Hemodialysis Measure Score`, na.rm = TRUE)

# Replace missing values with the computed average
df <- df %>%
  mutate(`Kt/V Adult Hemodialysis Measure Score` = ifelse(is.na(`Kt/V Adult Hemodialysis Measure Score`), average_column, `Kt/V Adult Hemodialysis Measure Score`))

# Verify that missing values have been replaced
missing_values_after <- sum(is.na(df$`Kt/V Adult Hemodialysis Measure Score`))
cat("Number of missing values after replacement:", missing_values_after, "\n")

# Calculate z-scores for the specific column
df <- df %>%
  mutate(z_score = abs(scale(`Kt/V Adult Hemodialysis Measure Score`)))

# Define threshold for outlier detection (e.g., z-score > 3)
threshold <- 3

# Remove outliers based on z-score
df_clean <- df %>%
  filter(z_score <= threshold) %>%
  select(-z_score)  # Remove the temporary z_score column
