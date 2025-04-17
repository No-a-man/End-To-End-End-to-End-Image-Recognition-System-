# Convert lists to matrices (assuming equal lengths)
predictedVal_matrix1 <- matrix(unlist(treeLabels), nrow = length(treeLabels), byrow = TRUE)
predictedVal_matrix2 <- matrix(unlist(animalLabels), nrow = length(animalLabels), byrow = TRUE)
predictedVal_matrix3 <- matrix(unlist(mythLabels), nrow = length(mythLabels), byrow = TRUE)

# Remove 49th row if needed (from all matrices)
if (nrow(predictedVal_matrix1) >= 49) {
  predictedVal_matrix1 <- predictedVal_matrix1[-49, , drop = FALSE]
  predictedVal_matrix2 <- predictedVal_matrix2[-49, , drop = FALSE]
  predictedVal_matrix3 <- predictedVal_matrix3[-49, , drop = FALSE]
  

}

# Combine all columns at once
updated_matrix <- cbind(
  final_feature_matrix,
  TreeAnnotation = predictedVal_matrix1,
  AnimalAnnotation = predictedVal_matrix2,
  MythAnnotation = predictedVal_matrix3
)

# Verify column names
colnames(updated_matrix)[(ncol(final_feature_matrix)+1):ncol(updated_matrix)] <- 
  c("TreeAnnotation", "AnimalAnnotation", "MythAnnotation")

# Save to CSV
write.csv(updated_matrix, file = "combinedDataSet.csv", row.names = FALSE)

updated_matrix_Tree <- cbind(
  final_feature_matrix,
  TreeAnnotation = predictedVal_matrix1
)

colnames(updated_matrix_Tree)[ncol(updated_matrix_Tree)] <- "TreeAnnotation"


# Save to CSV
write.csv(updated_matrix_Tree, file = "treeDataSet.csv", row.names = FALSE)


updated_matrix_Animal <- cbind(
  final_feature_matrix,
  TreeAnnotation = predictedVal_matrix2
)

colnames(updated_matrix_Animal)[ncol(updated_matrix_Animal)] <- "AnimalAnnotation"


# Save to CSV
write.csv(updated_matrix_Animal, file = "animalDataSet.csv", row.names = FALSE)