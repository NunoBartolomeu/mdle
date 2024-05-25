library(dplyr)
library(caret)
library(randomForest) #Implements Random Forest
library(class) #Implements KNN
library(e1071) #Implements SVM
library(smotefamily)#For SMOTE sampling


set.seed(123)
# Set the working directory to the 'src' directory
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))

######################Auxiliar Function###############################
selecionar_amostragem_1 <- function(df, target_column, train_ratio = 0.7, num_folds = 5) {
  cat("Escolha a técnica de amostragem:\n")
  cat("1: Amostragem Aleatória Simples\n")
  cat("2: Amostragem Estratificada\n")
  cat("3: Amostragem Sistemática\n")
  cat("4: Oversampling\n")
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
  } else if(choice == 4) {
    set.seed(123)
    df <- subset(df, select = -c(datetime, Date, Hour))
    train_index <- sample(1:nrow(df), size = train_ratio * nrow(df))
    
    train <- df[train_index, ]
    
    features <- train %>% select(-c(target_column))
    str(features)
    
    target <- train %>% select(c(target_column))
    cat("TARGET")
    str(target)
    
    # Aplying BLSMOTE 
    train <- BLSMOTE(X = features, target = target, method = "type1")
    
    test <- df[-train_index, ]
  }else {
    stop("Opção inválida. Tente novamente.")
  }
  
  return(list(train = train, test = test))
}



#######################################################################
df <- read.csv("../../mdle_data/out/merged_dataset.csv")

#Remove instances with NA values
df <- df %>% na.omit()



#######################Random Forrest###################################
resultados <- selecionar_amostragem(df, target_column = "Active.Energy..kWh.")

df_train <- resultados$train
df_test <- resultados$test


rf_model <- randomForest(Active.Energy..kWh.  ~ ., data = df_train, ntree = 200, maxnodes = 40)
# Predict Model
predictions <- predict(rf_model, newdata = df_test)

#Avaluate Model
actuals <- df_test$Active.Energy..kWh.
mae <- mean(abs(predictions - actuals))
mse <- mean((predictions - actuals)^2)
r2 <- 1 - sum((predictions - actuals)^2) / sum((actuals - mean(actuals))^2)


# Show results
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("R-squared (R²):", r2, "\n")
  


ggplot(data = data.frame(predictions, actuals = df_test$Active.Energy..kWh.), aes(x = actuals, y = predictions)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predictions vs Real Values", x = "Real Values", y = "Predictions") +
  theme_minimal()


####################KNN#############################################################
df <- subset(df, select = -c(datetime, Date, Hour))
resultados <- selecionar_amostragem(df, target_column = "Active.Energy..kWh.")

# Split the dataset
train_index <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
df_train <- df[train_index == 1, ]
df_test <- df[train_index == 2, ]

# Remove the column before PCA
train_features <- df_train %>% select(-Active.Energy..kWh.)
test_features <- df_test %>% select(-Active.Energy..kWh.)

# Apply the PCA
preProc <- preProcess(train_features, method = "pca", thresh = 0.95)
train_pca <- predict(preProc, train_features)
test_pca <- predict(preProc, test_features)

# Bind the new data with the target column
train_pca <- cbind(train_pca, Active.Energy..kWh. = df_train$Active.Energy..kWh.)
test_pca <- cbind(test_pca, Active.Energy..kWh. = df_test$Active.Energy..kWh.)

# Defines the number of neighbours
k <- 3

# Normalize the data
train_pca_norm <- scale(train_pca)
test_pca_norm <- scale(test_pca)

# Separate features and target
train_features_norm <- train_pca_norm[, -ncol(train_pca_norm)]
train_target <- train_pca$Active.Energy..kWh.
test_features_norm <- test_pca_norm[, -ncol(test_pca_norm)]
test_target <- test_pca$Active.Energy..kWh.

# Train the model
predictions_knn <- knn(train = train_features_norm, test = test_features_norm, cl = train_target, k = k)

# Converts to numeric
predictions_knn <- as.numeric(as.character(predictions_knn))

#Calculate residuals
residuals_knn <- test_target - predictions_knn

#Calculate metrics
mae_knn <- mean(abs(residuals_knn))
mse_knn <- mean(residuals_knn^2)
r2_knn <- 1 - (sum(residuals_knn^2) / sum((test_target - mean(test_target))^2))

# Print metrics
cat("Mean Absolute Error (MAE):", mae_knn, "\n")
cat("Mean Squared Error (MSE):", mse_knn, "\n")
cat("R-squared (R²):", r2_knn, "\n")
