################# Preparation ################
library(dplyr) #data manipulation
library(sparklyr) #spark 
library(smotefamily) #For SMOTE sampling
library(data.table) #To be used when possible, as a more performant data.frame
library(e1071)
library(caret)

# Set the working directory to the 'src' directory
#setwd("D:/Cadeiras/MDLE/mdle/project1/src")
setwd("C:/Users/pedro/Desktop/mestradoLab/mdle/project1/src")

################# Spark setup ################
spark_disconnect_all() #just preventive code
sc <- spark_connect('local', version = '3.4.2', hadoop_version = '3', config = list())

census_dataset <- read.csv("../mdle_data/INE/Censos_2021_Lisbon_filtered.csv", sep = ',')
weather_dataset <- read.csv("../mdle_data/Weather/Lisbon_2022-12-01_2023-12-31_filtered.csv", sep = "," )
energy_dataset <- read.csv("../mdle_data/consumos_horario_codigo_postal_lisboa.csv", sep = ',')

weather_dataset$datetime <- as.POSIXct(weather_dataset$datetime, format = "%Y-%m-%dT%H:%M:%S")
energy_dataset$Date.Time <- as.POSIXct(energy_dataset$Date.Time, format = "%Y-%m-%dT%H:%M:%S")

#Merge datasets
merged1 <- merge(weather_dataset, energy_dataset, by.x = "datetime", by.y = "Date.Time", all = FALSE)
str(merged1)
#View first few rows of merged dataset
head(merged1)

str(merged1)

merged2 <- merge(census_dataset, energy_dataset, by.x = "MUNICIPIO", by.y = "Zip.Code", all = FALSE)
head(merged2)




##############################################################################################################
df <- copy_to(sc, merged1)
#Generating train and test data
set.seed(123)
df.split <- df %>% sdf_random_split(training = 2/3, testing = 1/3)
df.train <- df.split$training 
df.test <-  df.split$testing

#Baseline
# Collect the data from Spark DataFrame to local R environment
train_data_local <- collect(df.train)
test_data_local <- collect(df.test)

# Use the table function to determine the number of instances for each class
train_class_counts <- table(train_data_local$Active_Energy_kWh_)
test_class_counts <- table(test_data_local$Active_Energy_kWh_)

#Linear_Regression_Model
model_linear_regression <- lm(Active_Energy_kWh_ ~ ., data = df.train)

predictions <- predict(model_linear_regression, df.test)

#R^2 
r_squared <- summary(model_linear_regression)$r.squared
r_squared


############################################################################
################# Spark cleanup ################
spark_disconnect(sc)


