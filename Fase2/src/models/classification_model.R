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

#########################AUXILIAR FUNCTIONS###################################################
selecionar_amostragem <- function(df, target_column, train_ratio = 0.7) {
  cat("Escolha a técnica de amostragem:\n")
  cat("1: Amostragem Aleatória Simples\n")
  cat("2: Amostragem Estratificada\n")
  cat("3: Amostragem Sistemática\n")
  choice <- as.integer(readLines(con = stdin(), n = 1))
  
  if(choice == 1) {
    set.seed(123)
    sample_index <- sample(1:nrow(df), size = train_ratio * nrow(df))
    train <- df[sample_index, ]
    test <- df[-sample_index, ]
  } else if(choice == 2) {
    set.seed(123)
    train_index <- createDataPartition(df[[target_column]], p = train_ratio, list = FALSE)
    train <- df[train_index, ]
    test <- df[-train_index, ]
  } else if(choice == 3) {
    set.seed(123)
    k <- 5
    sample_index <- seq(1, nrow(df), by = k)
    train <- df[sample_index, ]
    test <- df[-sample_index, ]
  }else {
    stop("Opção inválida. Tente novamente.")
  }
  
  return(list(train = train, test = test))
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
##############################################################################################
df <- read.csv("../../mdle_data/out/merged_dataset.csv")

#Calculate the mean of column Active.Energy..kWh.
mean_energy <- mean(df$Active.Energy..kWh., na.rm = TRUE)

#Creates the new column
df$Active.Energy.Class <- ifelse(df$Active.Energy..kWh. > mean_energy, "High", "Low")

#Transforms the new column to factor
df$Active.Energy.Class <- as.factor(df$Active.Energy.Class)

#Remove column Active.Energy..kWh.
df <- df %>% select(-c(Active.Energy..kWh.))
################Random Forrest###############################################################
df <- na.omit(df)

resultados <- selecionar_amostragem(df, target_column = "Active.Energy..kWh.")

df_train <- resultados$train
df_test <- resultados$test

# Train Random Forest model
rf_model <- randomForest(Active.Energy.Class ~ ., data = df_train, ntree = 200, maxnodes = 40)

predictions <- predict(rf_model, newdata = df_test)
probabilities <- predict(rf_model, newdata = df_test, type = "prob")

confusion_matrix <- confusionMatrix(predictions, df_test$Active.Energy.Class)
print(confusion_matrix)

#Calcules the ROC curve
roc_curve <- roc(df_test$Active.Energy..kWh., probabilities[, 2])
plot(roc_curve, col = "blue", main = "Curva ROC para o Modelo Random Forest")
abline(a = 0, b = 1, lty = 2, col = "red")

#roc_curve <- roc(df_test$Active.Energy.Class, probabilities[, 2])
#plot(roc_curve, col = "blue", main = "Curva ROC para o Modelo Random Forest")
#abline(a = 0, b = 1, lty = 2, col = "red")

#################K-Nearest Neighbors - KNN#########################################################################
#Showing good values

df <- na.omit(df)
df <- subset(df, select = -c(datetime, Date, Hour))
numeric_cols <- sapply(df, is.numeric)

#Don't normalize
numeric_cols["Active.Energy.Class"] <- FALSE


#Normalize the numeric columns
df[numeric_cols] <- as.data.frame(lapply(df[numeric_cols], normalize))

resultados <- selecionar_amostragem(df, target_column = "Active.Energy..kWh.")

df_train <- resultados$train
df_test <- resultados$test


# Separate features and labels
features <- names(df)[!names(df) %in% c("Active.Energy.Class")]

train_features <- df_train[, features]
train_labels <- df_train$Active.Energy.Class
test_features <- df_test[, features]
test_labels <- df_test$Active.Energy.Class



#Define number of neighbours
k <- 3

#Train the model
predictions <- knn(train = train_features, test = test_features, cl = train_labels, k = k)

#Confusion Matrix
confusion_matrix <- confusionMatrix(predictions, test_labels)
print(confusion_matrix)


#############################################################################################
################SMV##########################################################################
df <- na.omit(df)
df <- subset(df, select = -c(datetime, Date, Hour))

resultados <- selecionar_amostragem(df, target_column = "Active.Energy..kWh.")

df_train <- resultados$train
df_test <- resultados$test

# Separate features and labels
features <- df %>% names()
features <- features[features != "Active.Energy.Class"]

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


