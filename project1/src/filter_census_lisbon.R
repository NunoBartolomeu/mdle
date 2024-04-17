# Set the working directory to the 'src' directory
setwd("D:/Cadeiras/MDLE/mdle/project1/src")

# Read the CSV file using a relative path
data <- read.csv("../mdle_data/INE/BGRI2021_1106.csv", sep = ",")

################################################################################

# Loop through each attribute
for (attr in names(data)) {
  cat("\nAttribute:", attr, "\n")
  
  # Get unique values for the attribute
  unique_values <- unique(data[[attr]])
  cat("Number of Unique Values:", length(unique_values), "\n")
  
  # Print the most frequent values and their percentages
  cat("Most frequent Values:\n")
  value_counts <- sort(table(data[[attr]]), decreasing = TRUE)
  for (value in names(value_counts)) {
    percentage <- (value_counts[value] / sum(!is.na(data[[attr]]))) * 100
    if (!is.na(percentage) && percentage > 2) {
      cat("\t", value, ":", round(percentage, 2), "%\n")
    }
  }
}

################################################################################


# Write the filtered data to a new CSV file
write.csv(data, file="../mdle_data/INE/Censos_2021_Lisbon_filtered.csv", row.names=FALSE)
