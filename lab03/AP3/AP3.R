################# Preparation ################
library(dplyr) #data manipulation
library(sparklyr) #spark 
library(smotefamily) #For SMOTE sampling
library(data.table) #To be used when possible, as a more performant data.frame
library(e1071)
library(caret)

setwd("C:/Users/pedro/Desktop/Mestrado/MDLE/lab/lab03/AP3")

if(!exists("printConfusionMatrix", mode="function")) 
  source("helperfunctions.R")

################# Spark setup ################
spark_disconnect_all() #just preventive code
sc <- spark_connect('local', version = '3.4.2', hadoop_version = '3', config = list())


################# Load data ################
basepath <- "../data"
tr.data <- c("train_data_25.csv","train_data_30.csv") #The data to use
labels<- c("train_labels_25.csv","train_labels_30.csv") #the lables for the data


fun1 <- function(i) { #read CSV data
  read.csv(paste(basepath,"train",i,sep = "/"), header=FALSE,stringsAsFactors = FALSE)
}

fun2 <- function(i) { #read and transpose CSV data
  read.csv(paste(basepath,"train",i,sep = "/"), header=FALSE,stringsAsFactors = FALSE) %>% t %>% as.data.table
}

df<-do.call(rbind, lapply(tr.data, fun1 )) #bind csv together
df.l<-do.call(rbind, lapply(labels, fun2 )) #bind class together
names(df.l) <-c("CLASS") #rename dependent variable
df.local<- cbind(df.l,df) #bind them together

df <- copy_to(sc, df.local)

################# G2 #######################
#Glimpse of the data set
#TODO
sdf <- sdf_schema(df)
head(df)
actual_rows <- nrow(spark_dataframe(df))
actual_cols <- ncol(spark_dataframe(df))
stopifnot(
  actual_rows==546,
  actual_cols==2190)

################# G3 #######################
#Feature Selection
idx <- c(1,2,5,6,9,10,11,14,16,17,19,21,24,25,26,31,32,33,34,35,41,44,49,50,54)

#TODO
df.sel <- df %>% select(all_of(idx))
head(df.sel)


################# G4 #######################
#Generating train and test data
set.seed(123)
df.split <- df %>% sdf_random_split(training = 2/3, testing = 1/3)
df.train <- df.split$training 
df.test <-  df.split$testing

#TODO Baseline
# Collect the data from Spark DataFrame to local R environment
train_data_local <- collect(df.train)
test_data_local <- collect(df.test)

# Use the table function to determine the number of instances for each class
train_class_counts <- table(train_data_local$CLASS)
test_class_counts <- table(test_data_local$CLASS)

# Display the class counts
print("Train Data Class Counts:")
print(train_class_counts)

print("Test Data Class Counts:")
print(test_class_counts)

random_forest_model <- ml_random_forest(df.train, formula = ("CLASS ~ ."))

# Make predictions on the test data using the trained model
predictions <- ml_predict(random_forest_model, df.test)

mdle.printConfusionMatrix(predictions, "Random Forest Baseline")

################# G5 #######################
num_pos_instances <- df.train %>%
  filter(CLASS == 1) %>%
  sdf_nrow() %>%
  as.integer()
num_neg_instances <- df.train %>%
  filter(CLASS == 0) %>%
  sdf_nrow() %>%
  as.integer()
# Determine the minimum number of instances for balanced sampling
min_instances <- min(num_pos_instances, num_neg_instances)
# Undersample the positive and negative classes
df.pos.train <- df.train %>%
  filter(CLASS == 1) %>%
  sdf_sample(fraction = min_instances / num_pos_instances)

df.neg.train <- df.train %>%
  filter(CLASS == 0) %>%
  sdf_sample(fraction = min_instances / num_neg_instances)

# Combine the undersampled dataframes
df.train_balanced <- sdf_bind_rows(df.pos.train, df.neg.train)

# Check the number of instances for each class after undersampling
num_instances_class_0 <- df.train_balanced %>%
  filter(CLASS == 0) %>%
  sdf_nrow() %>%
  as.integer()

num_instances_class_1 <- df.train_balanced %>%
  filter(CLASS == 1) %>%
  sdf_nrow() %>%
  as.integer()

cat("Number of instances for CLASS=0 after undersampling:", num_instances_class_0, "\n")
cat("Number of instances for CLASS=1 after undersampling:", num_instances_class_1, "\n")


random_forest_model <- ml_random_forest(df.train_balanced, formula = ("CLASS ~ ."))
# Make predictions on the test data using the trained model
predictions <- ml_predict(random_forest_model, df.test)

# Print confusion matrix and performance metrics
mdle.printConfusionMatrix(predictions, "Random Forest Undersampling")

###############################################################################################

#Oversampling
num_pos_instances <- df.train %>%
  filter(CLASS == 1) %>%
  sdf_nrow() %>%
  as.integer()
num_neg_instances <- df.train %>%
  filter(CLASS == 0) %>%
  sdf_nrow() %>%
  as.integer()
# Determine the maximum number of instances for balanced sampling
max_instances <- max(num_pos_instances, num_neg_instances)
# Undersample the positive and negative classes
df.pos.train <- df.train %>%
  filter(CLASS == 1) %>%
  sdf_sample(fraction = max_instances / num_pos_instances)

df.neg.train <- df.train %>%
  filter(CLASS == 0) %>%
  sdf_sample(fraction = max_instances / num_neg_instances)

# Combine the undersampled dataframes
df.train_balanced <- sdf_bind_rows(df.pos.train, df.neg.train)

# Check the number of instances for each class after undersampling
num_instances_class_0 <- df.train_balanced %>%
  filter(CLASS == 0) %>%
  sdf_nrow() %>%
  as.integer()

num_instances_class_1 <- df.train_balanced %>%
  filter(CLASS == 1) %>%
  sdf_nrow() %>%
  as.integer()

cat("Number of instances for CLASS=0 after undersampling:", num_instances_class_0, "\n")
cat("Number of instances for CLASS=1 after undersampling:", num_instances_class_1, "\n")

random_forest_model <- ml_random_forest(df.train_balanced, formula = ("CLASS ~ ."))
# Make predictions on the test data using the trained model
predictions <- ml_predict(random_forest_model, df.test)

# Print confusion matrix and performance metrics
mdle.printConfusionMatrix(predictions, "Random Forest Oversampling")

#############################################################################################
#BSMOTE
# Convert tbl_spark to regular R data frame
df <- as.data.frame(df.train)

# Extract features (exclude the class label)
features <- df[, -1]  # Exclude the first column, assuming "CLASS" is the first column


# Extract the class labels
labels <- df$CLASS

# Apply Borderline-SMOTE sampling using BLSMOTE

# Apply BLSMOTE oversampling
oversampled_data <- BLSMOTE(X = features, target = labels, method = "type1")
class(oversampled_data)

# Combine oversampled features and labels into a new data frame
oversampled_df <- data.frame(oversampled_data$data)

class(oversampled_df)

spark_df <- sdf_copy_to(sc, oversampled_df)

random_forest_model <- ml_random_forest(spark_df, formula = ("class ~ ."))
# Make predictions on the test data using the trained model
predictions <- ml_predict(random_forest_model, df.test)

# Print confusion matrix and performance metrics
mdle.printConfusionMatrix(predictions, "Random Forest BLSMOTE")

################# Spark cleanup ################
spark_disconnect(sc)
