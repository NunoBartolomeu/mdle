#Unsupervised Discretization using Equal Width Binning

# Load required libraries
library(dplyr)
library(infotheo)

# Load datasets
pima <- read.csv("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab02/pima.csv")


# Equal width discretization
pima_discretized <- pima %>%
    mutate(
        age_discretized = cut(Age, breaks = 5),
        glucose_discretized = cut(Glucose, breaks = 5),
        insulin_discretized = cut(Insulin, breaks = 5),
        bmi_discretized = cut(BMI, breaks = 5),
        diabetes_pedigree_discretized = cut(DiabetesPedigreeFunction, breaks = 5),
        outcome_discretized = cut(Outcome, breaks = 5),
        pregnancies_discretized = cut(Pregnancies, breaks = 5),
        blood_pressure_discretized = cut(BloodPressure, breaks = 5),
        skin_thickness_discretized = cut(SkinThickness, breaks = 5)
    )

# Show the intervals for each attribute
intervals <- sapply(pima_discretized, function(x) levels(x))
intervals

#####################################################################################################

#Supervised Discretization using ChiMerge Algorithm
library(discretization)

pima_discretized <- chiM(pima, alpha = 0.05)

# Accessing discretization intervals for each feature
discretization_intervals <- pima_discretized$cutp

# Printing discretization intervals for each feature
for (i in 1:length(discretization_intervals)) {
  feature_name <- names(discretization_intervals)[i]
  cat(feature_name, ":\n")
  print(discretization_intervals[[i]])
  cat("\n")
}



