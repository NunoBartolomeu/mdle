# Set the working directory to the 'src' directory
setwd("D:/Cadeiras/MDLE/mdle/project1/src")

# List of filenames
filenames <- c(
  "Lisbon_2023-01-01_2023-01-31.csv",
  "Lisbon_2022-12-01_2022-12-31.csv",
  "Lisbon_2023-02-01_2023-02-28.csv",
  "Lisbon_2023-03-01_2023-03-31.csv",
  "Lisbon_2023-04-01_2023-04-30.csv",
  "Lisbon_2023-05-01_2023-05-31.csv",
  "Lisbon_2023-06-01_2023-06-30.csv",
  "Lisbon_2023-07-01_2023-07-31.csv",
  "Lisbon_2023-08-01_2023-08-31.csv",
  "Lisbon_2023-09-01_2023-09-30.csv",
  "Lisbon_2023-10-01_2023-10-31.csv",
  "Lisbon_2023-11-01_2023-11-30.csv",
  "Lisbon_2023-12-01_2023-12-31.csv"
)

# Initialize an empty data frame to store merged data
merged_data <- data.frame()

# Loop through each file, read it, and append to merged_data
for (filename in filenames) {
  file_path <- paste("../mdle_data/Weather/", filename, sep="")
  data <- read.csv(file_path, header=TRUE)
  merged_data <- rbind(merged_data, data)
}

# Write merged data to a new CSV file
write.csv(merged_data, file="../mdle_data/Weather/Lisbon_2022-12-01_2023-12-31_full.csv", row.names=FALSE)

################################################################################

# Read the merged data file
merged_data <- read.csv("../mdle_data/Weather/Lisbon_2022-12-01_2023-12-31_full.csv", header=TRUE)

# Loop through each attribute
for (attr in names(merged_data)) {
  cat("\nAttribute:", attr, "\n")
  
  # Get unique values for the attribute
  unique_values <- unique(merged_data[[attr]])
  cat("Number of Unique Values:", length(unique_values), "\n")
  
  # Print the most frequent values and their percentages
  cat("Most frequent Values:\n")
  value_counts <- sort(table(merged_data[[attr]]), decreasing = TRUE)
  for (value in names(value_counts)) {
    percentage <- (value_counts[value] / sum(!is.na(merged_data[[attr]]))) * 100
    if (!is.na(percentage) && percentage > 2) {
      cat("\t", value, ":", round(percentage, 2), "%\n")
    }
  }
}

################################################################################

# Columns to be removed
columns_to_remove <- c(
  "name", 
  "precipprob", 
  "preciptype", 
  "snow", 
  "snowdepth",
  "uvindex", 
  "severerisk", 
  "icon",
  
  "feelslike",
  "visibility",
  "precip"
  )

# Remove the specified columns
filtered_data <- merged_data[, !(names(merged_data) %in% columns_to_remove)]

# Write the filtered data to a new CSV file
write.csv(filtered_data, file="../mdle_data/Weather/Lisbon_2022-12-01_2023-12-31_filtered.csv", row.names=FALSE)



