###################
#
#Lab 2
#
###################
###Feature Selection####
#Ex (a)#
# Load necessary libraries
library(dplyr)

# Load datasets
pima_dataset <- read.csv("C:/Users/pedro/Desktop/mestradoLab/mdle/lab02/pima.csv")
lisbon_dataset <- read.csv("C:/Users/pedro/Desktop/mestradoLab/mdle/lab02/Lisbon_ 2023-01-01_2023-01-31.csv")

# Compute variance and mean-median for each dataset
variance_pima <- apply(pima_dataset, 2, var)
mean_median_pima <- apply(pima_dataset, 2, function(x) mean(x) - median(x))

variance_lisbon <- apply(lisbon_dataset, 2, var)
mean_median_lisbon <- apply(lisbon_dataset, 2, function(x) mean(x) - median(x))

# Combine relevance measures for each dataset
relevance_pima <- data.frame(feature = colnames(pima_dataset), variance = variance_pima, mean_median = mean_median_pima)
relevance_lisbon <- data.frame(feature = colnames(lisbon_dataset), variance = variance_lisbon, mean_median = mean_median_lisbon)

# Print tables with values of each feature
print("Pima Dataset:")
print(relevance_pima)

print("Lisbon Dataset:")
print(relevance_lisbon)

# Plot the sorted relevance values for each dataset
par(mfrow=c(2,1))

# Pima Dataset
relevance_pima_sorted <- relevance_pima[order(relevance_pima$variance, decreasing = TRUE), ]
barplot(relevance_pima_sorted$variance, main = "Relevance of Features - Pima Dataset (Variance)", names.arg = relevance_pima_sorted$feature, las = 2)

# Lisbon Dataset
relevance_lisbon_sorted <- relevance_lisbon[order(relevance_lisbon$variance, decreasing = TRUE), ]
barplot(relevance_lisbon_sorted$variance, main = "Relevance of Features - Lisbon Dataset (Variance)", names.arg = relevance_lisbon_sorted$feature, las = 2)

#--------------------------------------------------------------------------------------------------------------

#Ex (b)#
# Define function to calculate cumulative relevance
calculate_cumulative_relevance <- function(relevance_values, threshold) {
  relevance_sorted <- sort(relevance_values, decreasing = TRUE)
  total_relevance <- sum(relevance_sorted)
  cumulative_relevance <- cumsum(relevance_sorted) / total_relevance
  m <- sum(cumulative_relevance < threshold) + 1  # Add 1 to include the first feature
  return(m)
}

# Define thresholds
thresholds <- c(0.5, 0.75, 0.9)

# Compute m for each threshold for Pima Dataset
m_pima <- sapply(thresholds, function(threshold) {
  calculate_cumulative_relevance(relevance_pima$variance, threshold)
})

# Compute m for each threshold for Lisbon Dataset
m_lisbon <- sapply(thresholds, function(threshold) {
  calculate_cumulative_relevance(relevance_lisbon$variance, threshold)
})

# Print the results
cat("For the Pima Dataset:\n")
cat(paste("Threshold:", thresholds, "\t m:", m_pima), "\n\n")

cat("For the Lisbon Dataset:\n")
cat(paste("Threshold:", thresholds, "\t m:", m_lisbon), "\n\n")

#--------------------------------------------------------------------------------------------------------------

#Ex c#

# Load datasets
pima <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/pima.csv")
lisbon <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/Lisbon_ 2023-01-01_2023-01-31.csv")

# Define function to calculate Fisher Ratio
calculate_fisher_ratio <- function(data, classes) {
  class_means <- tapply(data, classes, mean)
  class_vars <- tapply(data, classes, var)
  between_class_variance <- sum(table(classes) * (class_means - mean(data))^2) / (length(data) - length(unique(classes)))
  within_class_variance <- sum((data - rep(class_means[as.character(classes)], each = length(classes)))^2) / (length(data) - length(unique(classes)))
  fisher_ratio <- between_class_variance / within_class_variance
  return(fisher_ratio)
}

# Compute Fisher Ratio for Pima Dataset
fisher_ratio_pima <- sapply(pima[, -ncol(pima)], calculate_fisher_ratio, classes = pima$Outcome)

# Compute Fisher Ratio for Lisbon Dataset
# Assuming 'severerisk' is the class label
fisher_ratio_lisbon <- sapply(lisbon[, c("temp", "feelslike", "dew", "humidity", "precip", "snow", "snowdepth", "windgust", "windspeed", "winddir", "sealevelpressure", "cloudcover", "visibility", "solarradiation", "solarenergy", "uvindex", "severerisk")], calculate_fisher_ratio, classes = lisbon$severerisk)

# Combine Fisher Ratio values for each dataset
relevance_pima_fisher <- data.frame(feature = colnames(pima[, -ncol(pima)]), fisher_ratio = fisher_ratio_pima)
relevance_lisbon_fisher <- data.frame(feature = colnames(lisbon[, c("temp", "feelslike", "dew", "humidity", "precip", "snow", "snowdepth", "windgust", "windspeed", "winddir", "sealevelpressure", "cloudcover", "visibility", "solarradiation", "solarenergy", "uvindex", "severerisk")]), fisher_ratio = fisher_ratio_lisbon)

# Print tables with values of each feature based on Fisher Ratio
print("Pima Dataset (Fisher Ratio):")
print(relevance_pima_fisher)

print("Lisbon Dataset (Fisher Ratio):")
print(relevance_lisbon_fisher)

# Plot the sorted relevance values based on Fisher Ratio for each dataset
par(mfrow=c(2,1))

# Pima Dataset
relevance_pima_fisher_sorted <- relevance_pima_fisher[order(relevance_pima_fisher$fisher_ratio, decreasing = TRUE), ]
barplot(relevance_pima_fisher_sorted$fisher_ratio, main = "Relevance of Features - Pima Dataset (Fisher Ratio)", names.arg = relevance_pima_fisher_sorted$feature, las = 2)

# Lisbon Dataset
relevance_lisbon_fisher_sorted <- relevance_lisbon_fisher[order(relevance_lisbon_fisher$fisher_ratio, decreasing = TRUE), ]
barplot(relevance_lisbon_fisher_sorted$fisher_ratio, main = "Relevance of Features - Lisbon Dataset (Fisher Ratio)", names.arg = relevance_lisbon_fisher_sorted$feature, las = 2)

# Compute m for each threshold for Pima Dataset using Fisher Ratio
m_pima_fisher <- sapply(thresholds, function(threshold) {
  calculate_cumulative_relevance(relevance_pima_fisher$fisher_ratio, threshold)
})

# Compute m for each threshold for Lisbon Dataset using Fisher Ratio
m_lisbon_fisher <- sapply(thresholds, function(threshold) {
  calculate_cumulative_relevance(relevance_lisbon_fisher$fisher_ratio, threshold)
})

# Print the results using Fisher Ratio
cat("For the Pima Dataset (Fisher Ratio):\n")
cat(paste("Threshold:", thresholds, "\t m:", m_pima_fisher), "\n\n")

cat("For the Lisbon Dataset (Fisher Ratio):\n")
cat(paste("Threshold:", thresholds, "\t m:", m_lisbon_fisher), "\n\n")



