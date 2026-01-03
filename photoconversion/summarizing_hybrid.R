
# Load necessary library
library(dplyr)

# Function to summarize a single CSV file
summarize_csv <- function(file_path) {
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Extract relevant columns
  green_normalized <- data$Green_normalized
  red_normalized <- data$Red_normalized
  green_hybrid <- data$Green_hybrid
  
  return(list(green_normalized, red_normalized, green_hybrid))
}

# Function to summarize all normalized CSV files in the directory
summarize_all_csvs <- function(directory = getwd()) {
  # List all normalized CSV files in the directory
  files <- list.files(directory, pattern = "_normalized\\.csv$", full.names = TRUE)
  
  # Initialize lists to store the values
  green_values <- list()
  red_values <- list()
  hybrid_values <- list()
  
  # Summarize each file
  for (file in files) {
    values <- summarize_csv(file)
    green_values <- append(green_values, list(values[[1]]))
    red_values <- append(red_values, list(values[[2]]))
    hybrid_values <- append(hybrid_values, list(values[[3]]))
  }
  
  # Combine the values into data frames
  green_df <- do.call(cbind, green_values)
  red_df <- do.call(cbind, red_values)
  hybrid_df <- do.call(cbind, hybrid_values)
  
  # Calculate mean and standard deviation for each row
  summary_df <- data.frame(
    Green_mean = rowMeans(green_df, na.rm = TRUE),
    Green_sd = apply(green_df, 1, sd, na.rm = TRUE),
    Red_mean = rowMeans(red_df, na.rm = TRUE),
    Red_sd = apply(red_df, 1, sd, na.rm = TRUE),
    Hybrid_mean = rowMeans(hybrid_df, na.rm = TRUE),
    Hybrid_sd = apply(hybrid_df, 1, sd, na.rm = TRUE)
  )
  
  # Define the time values
  time_values <- c(-9, -6, -3, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59, 62, 65, 68, 71, 74, 77, 80, 83, 86, 89, 92, 95, 98, 101, 104, 107, 110, 113, 116, 119, 122, 125)
  
  # Add the Time column as the first column
  summary_df <- summary_df %>%
    mutate(Time = time_values) %>%
    select(Time, everything())
  
  # Save the results to a CSV file
  write.csv(summary_df, file.path(directory, "summary_results.csv"), row.names = FALSE)
  
  return(summary_df)
}

# Example usage
summary_results <- summarize_all_csvs()

# Print the results
print(summary_results)
