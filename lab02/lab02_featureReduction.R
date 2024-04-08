

#Feature Reduction

#Ex a) PCA decomposition

# Load necessary libraries
library(dplyr)


# Load datasets
pima <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/pima.csv")
lisbon <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/Lisbon_ 2023-01-01_2023-01-31.csv")

# Check the structure of the datasets
str(pima)
str(lisbon)

# Encode categorical variables
pima_encoded <- pima %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.integer))

lisbon_encoded <- lisbon %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.integer))

# Remove constant columns
lisbon_encoded <- lisbon_encoded[, -which(sapply(lisbon_encoded, function(x) length(unique(x))) == 1)]

# Perform PCA
pima_pca <- prcomp(pima_encoded[, -ncol(pima_encoded)], center = TRUE, scale. = TRUE)
lisbon_pca <- prcomp(lisbon_encoded[, -c(1, 2, 9, 17, 18)], center = TRUE, scale. = TRUE)

# Compute eigenvalues
pima_eigenvalues <- (pima_pca$sdev)^2
lisbon_eigenvalues <- (lisbon_pca$sdev)^2

# Sort eigenvalues in decreasing order
pima_eigenvalues <- sort(pima_eigenvalues, decreasing = TRUE)
lisbon_eigenvalues <- sort(lisbon_eigenvalues, decreasing = TRUE)

# Plot the eigenvalues
plot(pima_eigenvalues, type = "b", xlab = "Principal Component", ylab = "Eigenvalue", main = "Eigenvalues - Pima Dataset")
plot(lisbon_eigenvalues, type = "b", xlab = "Principal Component", ylab = "Eigenvalue", main = "Eigenvalues - Lisbon Dataset")

# Determine the adequate number of reduced dimensions (m)
# For example, you can look for the point where the eigenvalues drop off substantially

# Kaiser criterion: Retain components with eigenvalues > 1
pima_m <- sum(pima_eigenvalues > 1)
lisbon_m <- sum(lisbon_eigenvalues > 1)

# Print the adequate number of reduced dimensions
cat("Adequate number of reduced dimensions for Pima dataset:", pima_m, "\n")
cat("Adequate number of reduced dimensions for Lisbon dataset:", lisbon_m, "\n")


# Dimensionality reduction using PCA results
pima_reduced_pca <- predict(pima_pca, newdata = pima_encoded)[, 1:pima_m]
lisbon_reduced_pca <- predict(lisbon_pca, newdata = lisbon_encoded)[, 1:lisbon_m]

# Print number of features of reduced datasets
cat("Number of features in reduced Pima dataset (PCA):", ncol(pima_reduced_pca), "\n")
cat("Number of features in reduced Lisbon dataset (PCA):", ncol(lisbon_reduced_pca), "\n")

##############################################################################################


#Ex b 

# Load datasets
pima <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/pima.csv")
lisbon <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/Lisbon_ 2023-01-01_2023-01-31.csv")

# Function to handle missing or infinite values
handle_missing_infinite <- function(data) {
  for (col in names(data)){
    data[[col]][is.infinite(data[[col]])] <- NA
    data[[col]][is.nan(data[[col]])] <- NA
  }
  data <- na.omit(data)
  return(data)
}

# Encode categorical variables
pima_encoded <- pima %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.integer))


# Apply the function to handle missing or infinite values
lisbon_encoded <- handle_missing_infinite(lisbon_encoded)

# Compute SVD for Pima dataset
pima_svd <- svd(scale(pima_encoded[, -ncol(pima_encoded)]))

# Compute SVD for Lisbon dataset
lisbon_svd <- svd(scale(lisbon_encoded[, -c(1, 2, 9, 17, 18)]))

# Plot singular values for Pima dataset
plot(pima_svd$d, type = "b", xlab = "Singular Value", ylab = "Value", main = "Singular Values - Pima Dataset")

# Plot singular values for Lisbon dataset
plot(lisbon_svd$d, type = "b", xlab = "Singular Value", ylab = "Value", main = "Singular Values - Lisbon Dataset")

# Determine the adequate number of reduced dimensions (m)
# For example, you can look for the point where the singular values drop off substantially

# Kaiser criterion: Retain components with singular values > 1
pima_m <- sum(pima_svd$d > 1)
lisbon_m <- sum(lisbon_svd$d > 1)

# Print the adequate number of reduced dimensions
cat("Adequate number of reduced dimensions for Pima dataset:", pima_m, "\n")
cat("Adequate number of reduced dimensions for Lisbon dataset:", lisbon_m, "\n")

# Dimensionality reduction using SVD results
pima_reduced_svd <- pima_encoded %*% pima_svd$v[, 1:pima_m]
lisbon_reduced_svd <- lisbon_encoded %*% lisbon_svd$v[, 1:lisbon_m]

cat("Number of features in reduced Pima dataset (SVD):", ncol(pima_reduced_svd), "\n")
cat("Number of features in reduced Lisbon dataset (SVD):", ncol(lisbon_reduced_svd), "\n")
