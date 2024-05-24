install.packages("dplyr")
install.packages("rpart")
library(dplyr)
library(rpart)
library(sparklyr)
library(caret)
library(randomForest) #Implements Random Forest
library(class) #Implements KNN
library(e1071) #Implements SVM
library(pROC) #ROC Curve

set.seed(123)
# Set the working directory to the 'src' directory
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))

################# Spark setup ################
spark_disconnect_all() #just preventive code
sc <- spark_connect('local', version = '3.4.2', hadoop_version = '3', config = list())

#########################AUXILIAR FUNCTIONS###################################################
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
##############################################################################################

df <- read.csv("../../mdle_data/out/merged_dataset.csv")


#Use with Spark
#df_spark <- copy_to(sc, df, overwrite = TRUE)

#mean_energy <- df_spark %>% 
 # summarise(mean_energy = mean(Active_Energy_kWh_, na.rm = TRUE)) %>% 
  #collect() %>% 
  #.$mean_energy

#df_spark <- df_spark %>%
 # mutate(Label = ifelse(Active_Energy_kWh_ > mean_energy, "High", "Low"))

# Colect the DataFrame back to R
#df_collected <- df_spark %>% collect()

# Transform the column into a factor
#df_collected$Label <- as.factor(df_collected$Label)






#Use without Spark

#Calculate the mean of column Active.Energy..kWh.
mean_energy <- mean(df$Active.Energy..kWh., na.rm = TRUE)

#Creates the new column
df$Active.Energy.Class <- ifelse(df$Active.Energy..kWh. > mean_energy, "High", "Low")

#Transforms the new column to factor
df$Active.Energy.Class <- as.factor(df$Active.Energy.Class)

################Random Forrest###############################################################
#With Spark
#df_collected <- df_collected %>% na.omit()

#Prepare the data
#df_spark <- df_spark %>% sdf_random_split(training = 0.65, test = 0.35, seed = 1234)
#df_train <- df_spark$training
#df_test <- df_spark$test


# Trains the Random Forest
#rf_model <- df_train %>%
 # ml_random_forest(Label ~ ., type = "classification", num_trees = 10)

#Make predictions
#predictions <- ml_predict(rf_model, df_test)







#Without Spark
df <- na.omit(df)

#train_index <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_index <- sample(2, nrow(df), replace = TRUE, prob = c(0.7,0.3))

df_train <- df[train_index==1, ]
df_test <- df[train_index==2, ]

# Remover linhas com valores ausentes
#df_train_clean <- na.omit(df_train)

# Train Random Forest model
rf_model <- randomForest(Active.Energy.Class ~ ., data = df_train)

predictions <- predict(rf_model, newdata = df_test)
probabilities <- predict(rf_model, newdata = df_test, type = "prob")

confusion_matrix <- confusionMatrix(predictions, df_test$Active.Energy.Class)
print(confusion_matrix)

#Calcules the ROC curve
roc_curve <- roc(df_test$Active.Energy.Class, probabilities[, 2])
plot(roc_curve, col = "blue", main = "Curva ROC para o Modelo Random Forest")
abline(a = 0, b = 1, lty = 2, col = "red")

#################K-Nearest Neighbors - KNN#########################################################################
#Showing good values

df <- na.omit(df)
df <- subset(df, select = -c(datetime, Date, Hour))
numeric_cols <- sapply(df, is.numeric)

#Don't normalize 
numeric_cols["Active.Energy.Class"] <- FALSE
numeric_cols["Zip.Code"] <- FALSE
df[numeric_cols] <- as.data.frame(lapply(df[numeric_cols], normalize))

#train_index <- sample(1:nrow(df), size = 0.7 * nrow(df))
#df_train <- df[train_index, ]
#df_test <- df[-train_index, ]

train_index <- sample(2, nrow(df), replace = TRUE, prob = c(0.7,0.3))

df_train <- df[train_index==1, ]
df_test <- df[train_index==2, ]

# Separate features and labels
features <- names(df)[!names(df) %in% c("Active.Energy.Class")]

train_features <- df_train[, features]
train_labels <- df_train$Active.Energy.Class
test_features <- df_test[, features]
test_labels <- df_test$Active.Energy.Class

# Verificar valores ausentes nos dados de treino
anyNA(train_features)
anyNA(test_features)
anyNA(train_labels)
anyNA(test_labels)


#Define number of neighbours
k <- 3

#Train the model
predictions <- knn(train = train_features, test = test_features, cl = train_labels, k = k)
confusion_matrix <- confusionMatrix(predictions, test_labels)
print(confusion_matrix)

probabilities <- predict(rf_model, newdata = df_test, type = "prob")

#Calcules the ROC curve
roc_curve <- roc(df_test$Active.Energy.Class, probabilities[, 2])
plot(roc_curve, col = "blue", main = "Curva ROC para o Modelo Random Forest")
abline(a = 0, b = 1, lty = 2, col = "red")
#############################################################################################
################SMV##########################################################################
df <- na.omit(df)
df <- subset(df, select = -c(datetime, Date, Hour))

train_index <- sample(1:nrow(df), size = 0.7 * nrow(df))
df_train <- df[train_index, ]
df_test <- df[-train_index, ]

# Separate features and labels
train_features <- df_train[, features]
train_labels <- df_train$Active.Energy.Class
test_features <- df_test[, features]
test_labels <- df_test$Active.Energy.Class

# Trains SVM model
svm_model <- svm(train_features, train_labels, kernel = "radial")

predictions <- predict(svm_model, test_features)

confusion_matrix <- confusionMatrix(predictions, test_labels)
print(confusion_matrix)
############################################################################################


