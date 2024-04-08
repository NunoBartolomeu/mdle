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
pima_dataset <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/pima.csv")
lisbon_dataset <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/Lisbon_ 2023-01-01_2023-01-31.csv")

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

# Load required libraries
library(magrittr)

# Load datasets
pima_dataset <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/pima.csv")
lisbon_dataset <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/Lisbon_ 2023-01-01_2023-01-31.csv")


# Function to calculate Fisher Ratio
fisher_ratio <- function(x, y) {
  numerator <- (mean(x) - mean(y))^2
  denominator <- var(x) + var(y)
  ratio <- numerator / denominator
  return(ratio)
}

# Calculate Fisher Ratio for each feature
calculate_fisher_ratios <- function(pima_dataset, lisbon_dataset) {
  ratios <- sapply(names(pima_dataset), function(feature) {
    fisher_ratio(pima_dataset[[feature]], lisbon_dataset[[feature]])
  })
  return(ratios)
}

# Calculate Fisher Ratios for the datasets
fisher_ratios <- calculate_fisher_ratios(pima_dataset, lisbon_dataset)

# Rank features based on Fisher Ratios
ranked_features <- names(fisher_ratios)[order(fisher_ratios, decreasing = TRUE)]

# Output ranked features with their Fisher Ratios
result <- data.frame(Feature = ranked_features, Fisher_Ratio = fisher_ratios[ranked_features])
print(result)
