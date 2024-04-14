################# Preparation ################
library(dplyr) #data manipulation
library(sparklyr) #spark 
library(smotefamily) #For SMOTE sampling
library(data.table) #To be used when possible, as a more performant data.frame

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
head(sdf)
stopifnot(
  ncol(sdf)==545,
  nrow(sdf)==2190)


################# G3 #######################
#Feature Selection
idx <- c(1,2,5,6,9,10,11,14,16,17,19,21,24,25,26,31,32,33,34,35,41,44,49,50,54)

#TODO
df.sel <- df %>% select(all_of(idx))
head(df.sel)


################# G4 #######################
#Generating train and test data
set.seed(123)
df.split <- df %>% sdf_random_split(training = 0.67, testing = 0.33)
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

mdle.printConfusionMatrix <- function(actual, predicted) {
  conf_matrix <- table(Actual = actual, Predicted = predicted)
  print(conf_matrix)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  cat("Accuracy:", accuracy, "\n")
}

# Make predictions on the test data using the trained model
predictions <- ml_predict(random_forest_model, df.test)

predictions
# Extract the predicted labels from the predictions
predicted_labels <- collect(predictions) %>%
  select(CLASS) %>%
  as.vector()

# Extract the actual labels from the test data
actual_labels <- collect(df.test) %>%
  select(CLASS) %>%
  as.vector()

# Print confusion matrix and performance metrics
mdle.printConfusionMatrix(actual_labels, predicted_labels)

################# G5 #######################
#Using imbalanced correcting sampling techniques
#df.pos.train<- #TODO
#df.neg.train<- #TODO
#TODO

#Oversampling
#TODO

#BSMOTE
#TODO

################# Spark cleanup ################
spark_disconnect(sc)
