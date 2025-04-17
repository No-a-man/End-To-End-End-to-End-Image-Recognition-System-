calculate_annotator_agreement <- function(annotator_data, n_items = 28) {
  # Install and load required packages
  if (!require("combinat")) install.packages("combinat", dependencies = TRUE)
  if (!require("irr")) install.packages("irr", dependencies = TRUE)
  library(combinat)
  library(irr)
  
  # Verify input data
  if (nrow(annotator_data) < n_items) {
    warning(paste("Only", nrow(annotator_data), "rows available, using all rows instead of", n_items))
    n_items <- nrow(annotator_data)
  }
  
  # Get annotator names from column names
  annotators <- colnames(annotator_data)
  
  # Generate all combinations of 2, 3, 4, and 5 annotators
  combinations_list <- unlist(
    lapply(2:min(5, length(annotators)), 
           function(k) combn(annotators, k, simplify = FALSE)),
    recursive = FALSE
  )
  
  # Compute Krippendorff's Alpha for each combination
  alpha_results <- lapply(combinations_list, function(combo) {
    subset_data <- annotator_data[1:n_items, combo, drop = FALSE]
    alpha_value <- tryCatch({
      kripp.alpha(t(as.matrix(subset_data)), method = "nominal")$value
    }, error = function(e) {
      warning(paste("Error calculating alpha for combination:", paste(combo, collapse = ", ")))
      return(NA)
    })
    
    data.frame(
      Combination = paste(combo, collapse = ", "),
      Num_Annotators = length(combo),
      Alpha = alpha_value,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine all results into one dataframe
  alpha_df <- do.call(rbind, alpha_results)
  
  # Sort by Alpha value in descending order and remove NA values
  alpha_df <- alpha_df[order(-alpha_df$Alpha, alpha_df$Num_Annotators), ]
  alpha_df <- na.omit(alpha_df)
  rownames(alpha_df) <- NULL
  
  # Return the complete results
  return(alpha_df)
}

# Example usage:
# 1. Load your data first:
# AnnotationMyth <- read_excel("~/MGS 662 Project/Annotation.xlsx", 
#                             sheet = "Mythological Characters")
# 
# 2. Prepare annotator data:
# annotator_data <- AnnotationMyth[1:28, 3:7]  # Rows 1-28, columns 3-7
#
# 3. Run the function:
# results <- calculate_annotator_agreement(annotator_data)
#
# 4. View top results:
# head(results, 10)

