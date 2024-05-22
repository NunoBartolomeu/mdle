# Set the working directory to the 'src' directory
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))

#Attention!!!!! Correct "Lisbon_ 2023-01-01_2023-01-31.csv" -> "Lisbon_2023-01-01_2023-01-31.csv"
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
write.csv(merged_data, file="../mdle_data/Weather/Lisbon_2022-12-01_2023-12-31_full.csv", row.names = FALSE)

################################################################################

# Read the merged data file
#merged_data <- read.csv("../mdle_data/Weather/Lisbon_2022-12-01_2023-12-31_full.csv")

# 
variance_lisbon <- apply(merged_data, 2, function(x) var(x, na.rm = TRUE))

# Combine relevance measures for each dataset
relevance_lisbon <- data.frame(features = colnames(merged_data), variance = variance_lisbon)

print("Lisbon Dataset:")
print(relevance_lisbon)

################################################################################

# Remove columns with 0 variance
valid_indices <- which(relevance_lisbon$variance > 0 | relevance_lisbon$features == "datetime")

# Remove the specified columns
filtered_merged_data <- merged_data[, valid_indices]


print("Lisbon Dataset (filtered):")
print(filtered_merged_data)

# Write the filtered data to a new CSV file
write.csv(filtered_merged_data, file="../mdle_data/Weather/filtered_weather_Lisbon.csv", row.names=FALSE)

