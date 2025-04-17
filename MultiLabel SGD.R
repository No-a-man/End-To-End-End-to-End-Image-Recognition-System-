# Install required packages if not installed
if (!requireNamespace("LiblineaR", quietly = TRUE)) install.packages("LiblineaR")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")

# Load libraries
library(LiblineaR)
library(caret)
library(ggplot2)
library(reshape2)

# Load dataset
data <- read.csv("~/MGS 662 Project/combinedDataSet.csv")

# Convert all label columns to factors
data$MythAnnotation <- as.factor(data$MythAnnotation)
data$TreeAnnotation <- as.factor(data$TreeAnnotation)
data$AnimalAnnotation <- as.factor(data$AnimalAnnotation)

# Normalize features
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
feature_cols <- setdiff(colnames(data), c("MythAnnotation", "TreeAnnotation", "AnimalAnnotation"))
data_norm <- as.data.frame(lapply(data[, feature_cols], normalize))
data_norm <- cbind(data_norm, data[, c("MythAnnotation", "TreeAnnotation", "AnimalAnnotation")])

# Split data
set.seed(123)
trainIndex <- createDataPartition(data_norm$MythAnnotation, p = 0.8, list = FALSE)
trainData <- data_norm[trainIndex, ]
testData <- data_norm[-trainIndex, ]

# SGD Training Function with Loss Tracking
train_sgd <- function(train_features, train_labels, test_features, test_labels, 
                      learning_rate = 0.01, n_epochs = 100, batch_size = 32) {
  
  # Convert data to matrices
  X_train <- as.matrix(train_features)
  y_train <- as.numeric(as.character(train_labels))
  X_test <- as.matrix(test_features)
  y_test <- as.numeric(as.character(test_labels))
  
  # Initialize weights
  n_features <- ncol(X_train)
  weights <- rnorm(n_features, sd = 0.01)
  bias <- 0
  
  # Storage for loss history
  train_loss <- numeric(n_epochs)
  test_loss <- numeric(n_epochs)
  
  # SGD training
  for (epoch in 1:n_epochs) {
    # Shuffle data
    shuffle <- sample(nrow(X_train))
    X_train <- X_train[shuffle, ]
    y_train <- y_train[shuffle]
    
    # Mini-batch processing
    for (i in seq(1, nrow(X_train), by = batch_size)) {
      batch_end <- min(i + batch_size - 1, nrow(X_train))
      X_batch <- X_train[i:batch_end, ]
      y_batch <- y_train[i:batch_end]
      
      # Predictions
      linear_output <- X_batch %*% weights + bias
      predictions <- 1 / (1 + exp(-linear_output))
      
      # Gradient calculation
      error <- predictions - y_batch
      grad_weights <- t(X_batch) %*% error / length(y_batch)
      grad_bias <- mean(error)
      
      # Update parameters
      weights <- weights - learning_rate * grad_weights
      bias <- bias - learning_rate * grad_bias
    }
    
    # Calculate losses
    train_pred <- 1 / (1 + exp(-(X_train %*% weights + bias)))
    train_loss[epoch] <- -mean(y_train * log(train_pred) + (1 - y_train) * log(1 - train_pred))
    
    test_pred <- 1 / (1 + exp(-(X_test %*% weights + bias)))
    test_loss[epoch] <- -mean(y_test * log(test_pred) + (1 - y_test) * log(1 - test_pred))
  }
  
  # Final predictions
  final_pred <- ifelse(1 / (1 + exp(-(X_test %*% weights + bias))) > 0.5, 1, 0)
  accuracy <- mean(final_pred == y_test)
  
  return(list(
    weights = weights,
    bias = bias,
    accuracy = accuracy,
    train_loss = train_loss,
    test_loss = test_loss,
    predictions = final_pred
  ))
}

# Prepare feature matrices
train_features <- trainData[, feature_cols]
test_features <- testData[, feature_cols]

# Train models with SGD
results_myth <- train_sgd(train_features, trainData$MythAnnotation, test_features, testData$MythAnnotation)
results_tree <- train_sgd(train_features, trainData$TreeAnnotation, test_features, testData$TreeAnnotation)
results_animal <- train_sgd(train_features, trainData$AnimalAnnotation, test_features, testData$AnimalAnnotation)

# Print accuracies
print(paste("MythAnnotation Accuracy:", round(results_myth$accuracy, 4)))
print(paste("TreeAnnotation Accuracy:", round(results_tree$accuracy, 4)))
print(paste("AnimalAnnotation Accuracy:", round(results_animal$accuracy, 4)))

# Plot loss curves
plot_loss_curves <- function(results, title) {
  loss_data <- data.frame(
    Epoch = 1:length(results$train_loss),
    Train_Loss = results$train_loss,
    Test_Loss = results$test_loss
  )
  melted_loss <- melt(loss_data, id.vars = "Epoch")
  
  ggplot(melted_loss, aes(x = Epoch, y = value, color = variable)) +
    geom_line() +
    labs(title = paste("Loss Curves -", title),
         x = "Epoch",
         y = "Binary Cross-Entropy Loss") +
    theme_minimal()
}

# Generate plots
plot_loss_curves(results_myth, "Myth Annotations")
plot_loss_curves(results_tree, "Tree Annotations")
plot_loss_curves(results_animal, "Animal Annotations")

# Save predictions
final_predictions <- data.frame(
  MythPrediction = results_myth$predictions,
  TreePrediction = results_tree$predictions,
  AnimalPrediction = results_animal$predictions,
  MythActual = testData$MythAnnotation,
  TreeActual = testData$TreeAnnotation,
  AnimalActual = testData$AnimalAnnotation
)
