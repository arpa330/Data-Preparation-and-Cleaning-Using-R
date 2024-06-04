# Install the necessary packages (if not already installed)
install.packages(c("readr", "dplyr", "ggplot2"))

# Load the required libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the CSV dataset
df <- read_csv("BDA_assignment_data.csv")

# Select the most relevant attributes
relevant_attributes <- c("Fistula Measure Score", "Kt/V Adult Hemodialysis Measure Score",
                         "Catheter Measure Score", "Hypercalcemia Measure Score",
                         "NHSN Measure Score", "Anemia Management Reporting Score",
                         "Total Performance Score", "Mineral Metabolism Reporting Score")

# Filter the dataset to include only the desired columns
df_selected <- df %>%
  select(one_of(relevant_attributes))

# Summary statistics
summary_stats <- summary(df_selected)
print(summary_stats)

# Explore categorical variables
categorical_vars <- sapply(df_selected, is.factor)
unique_categories <- lapply(df_selected[, categorical_vars], levels)
print(unique_categories)

# Visualize distributions using appropriate plots based on variable type
plots <- lapply(names(df_selected), function(var) {
  if (is.numeric(df_selected[[var]])) {
    # For numeric variables, plot histogram
    ggplot(df_selected, aes(x = !!sym(var))) +
      geom_histogram(fill = "skyblue", color = "black", bins = 20) +
      labs(title = paste("Histogram of", var),
           x = var, y = "Frequency")
  } else {
    # For categorical variables, plot bar chart
    ggplot(df_selected, aes(x = factor(!!sym(var)))) +
      geom_bar(fill = "skyblue", color = "black") +
      labs(title = paste("Bar Chart of", var),
           x = var, y = "Count")
  }
})

# Print plots
for (plot in plots) {
  print(plot)
}

# Save filtered data to CSV
write_csv(df_selected, "filtered_BDA_assignment_data_2.csv")

# Convert all columns to factors (assuming all are character/string)
df_factors <- df_selected %>%
  mutate_all(as.factor)

# Custom function to plot histograms on diagonal in scatterplot matrix
panel_hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "skyblue", ...)
}

# Visualize relationships using pair-wise plots (scatterplot matrix)
pairs(df_factors, 
      lower.panel = panel.smooth,   # Add smooth curve in lower panel
      upper.panel = NULL,           # No upper panel (blank)
      diag.panel = panel_hist,      # Histograms on diagonal
      main = "Pair-wise Scatterplot Matrix of Categorical Variables")
