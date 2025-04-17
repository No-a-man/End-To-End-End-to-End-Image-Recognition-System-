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

# Load dataset - assuming it now contains MythLabels, TreeLabels, and AnimalLabels
data <- read.csv("~/MGS 662 Project/combinedDataSet.csv")

# Convert all label columns to factors
data$MythAnnotation <- as.factor(data$MythAnnotation)
data$TreeAnnotation <- as.factor(data$TreeAnnotation)  # New column
data$AnimalAnnotation <- as.factor(data$AnimalAnnotation)  # New column

# Normalize the features (excluding target columns)
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
feature_cols <- setdiff(colnames(data), c("MythAnnotation", "TreeAnnotation", "AnimalAnnotation"))
data_norm <- as.data.frame(lapply(data[, feature_cols], normalize))

# Add back target columns
data_norm$MythAnnotation <- data$MythAnnotation
data_norm$TreeAnnotation <- data$TreeAnnotation
data_norm$AnimalAnnotation <- data$AnimalAnnotation

# Split data into training (80%) and testing (20%) sets
set.seed(123)
trainIndex <- createDataPartition(data_norm$MythAnnotation, p = 0.8, list = FALSE)
trainData <- data_norm[trainIndex, ]
testData <- data_norm[-trainIndex, ]

# Function to train and evaluate a model for each label type
train_evaluate <- function(train_features, train_labels, test_features, test_labels, type = 6) {
  # Convert labels to numeric
  train_labels_num <- as.numeric(as.character(train_labels))
  test_labels_num <- as.numeric(as.character(test_labels))
  
  # Train model
  model <- LiblineaR(data = as.matrix(train_features), 
                     target = train_labels_num, 
                     type = type,
                     cost = 1, epsilon = 0.001)
  
  # Make predictions
  pred <- predict(model, as.matrix(test_features))$predictions
  
  # Calculate accuracy
  accuracy <- sum(test_labels_num == pred) / length(test_labels_num)
  
  return(list(model = model, accuracy = accuracy, predictions = pred))
}

# Prepare feature matrices
train_features <- trainData[, feature_cols]
test_features <- testData[, feature_cols]

# Train and evaluate models for each label type
results_myth <- train_evaluate(train_features, trainData$MythAnnotation, test_features, testData$MythAnnotation)
results_tree <- train_evaluate(train_features, trainData$TreeAnnotation, test_features, testData$TreeAnnotation)
results_animal <- train_evaluate(train_features, trainData$AnimalAnnotation, test_features, testData$AnimalAnnotation)

# Print accuracies
print(paste("MythAnnotation Accuracy:", round(results_myth$accuracy, 4)))
print(paste("TreeAnnotation Accuracy:", round(results_tree$accuracy, 4)))
print(paste("AnimalAnnotation Accuracy:", round(results_animal$accuracy, 4)))

# Combine predictions into a data frame
final_predictions <- data.frame(
  MythPrediction = results_myth$predictions,
  TreePrediction = results_tree$predictions,
  AnimalPrediction = results_animal$predictions,
  MythActual = testData$MythAnnotation,
  TreeActual = testData$TreeAnnotation,
  AnimalActual = testData$AnimalAnnotation
)

# Save predictions to CSV
write.csv(final_predictions, "multi_label_predictions.csv", row.names = FALSE)

# Plot accuracies
accuracy_df <- data.frame(
  LabelType = c("Myth", "Tree", "Animal"),
  Accuracy = c(results_myth$accuracy, results_tree$accuracy, results_animal$accuracy)
)

ggplot(accuracy_df, aes(x = LabelType, y = Accuracy, fill = LabelType)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.3) +
  ylim(0, 1) +
  labs(title = "Model Accuracy by Label Type") +
  theme_minimal()