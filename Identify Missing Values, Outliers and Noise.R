# Install caret package if not already installed
install.packages("VIM")


# Load caret libraries
library(VIM)
library(dplyr)

# Assuming df is your data frame named "filtered_BDA_assignment_data -2"
# Read the data into a dataframe
df <- read.csv("filtered_BDA_assignment_data_2.csv")

# Replace all non-numeric missing value representations with NA
df_selected <- df %>%
  mutate_all(~ ifelse(. %in% c("NA", "N/A", "", "N.A."), NA, .))

# View the structure of the cleaned data frame
str(df_selected)

# Function to count missing values (including NA) in each column
count_missing_values <- function(data) {
  missing_counts <- sapply(data, function(x) sum(is.na(x) | x == ""))
  return(missing_counts)
}

# Count missing values in each column (including "NA" values)
missing_counts <- count_missing_values(df_selected)

# Display the missing value counts for each column
cat("Missing Value Counts:\n")
print(missing_counts)

# Function to identify outlier and noise in character data
identify_outliers_noise <- function(data) {
  outliers_noise <- list()
  for (col in names(data)) {
    unique_values <- unique(data[[col]])
    if (length(unique_values) > 10) {  # Adjust threshold based on data size and characteristics
      # Identify infrequent values (potential outliers or noise)
      infrequent_values <- unique_values[table(data[[col]]) < 10]  # Adjust frequency threshold as needed
      outliers_noise[[col]] <- infrequent_values
    } else {
      outliers_noise[[col]] <- NULL  # No outlier detection for columns with few unique values
    }
  }
  return(outliers_noise)
}

# Identify outliers and noise in character columns
outliers_noise <- identify_outliers_noise(df_selected)

# Display identified outliers and noise for each column
cat("\nIdentified Outliers and Noise:\n")
print(outliers_noise)

# Replace missing values with KNN imputation
df_imputed <- replace_missing_with_KNN(df_selected)

# View the structure of the imputed data frame
str(df_imputed)




