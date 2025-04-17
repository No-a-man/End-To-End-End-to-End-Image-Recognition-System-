if (!requireNamespace("EBImage", quietly = TRUE)) {
  install.packages("BiocManager")
  BiocManager::install("EBImage")
}
library(EBImage)
compute_entropy <- function(img) {
  img_vector <- as.vector(img)
  img_vector <- (img_vector - min(img_vector)) / (max(img_vector) - min(img_vector))
  hist_vals <- hist(img_vector, breaks = seq(0, 1, by = 0.01), plot = FALSE)$density
  hist_vals <- hist_vals[hist_vals > 0]
  -sum(hist_vals * log(hist_vals))
}

extract_image_features <- function(filepath, scale_size = c(256, 256)) {
  img <- tryCatch({
    readImage(filepath)
  }, error = function(e) {
    message(paste("Error loading:", filepath))
    return(NULL)
  })
  
  if (is.null(img)) return(NULL)
  
  # Convert to grayscale if RGB

  
  img <- resize(img, w = scale_size[1], h = scale_size[2])
  img_thresh <- img > 0.5
  
  shape_feats <- computeFeatures.shape(img_thresh[1:100,1:100,1])
  basic_mean <- mean(img)
  basic_sd   <- sd(img)
  basic_min  <- min(img)
  basic_max  <- max(img)
  basic_entropy <- compute_entropy(img)
  basic_variance <- var(as.vector(img))
  #intensity_features <- computeFeatures.basic(img_thresh[1:100,1:100,1],img)  # Intensity-based features
  #texture_features <- computeFeatures.moment(img_thresh[1:100,1:100,1],img)  # Texture features
  #edge_features <- computeFeatures.haralick(img_thresh[1:100,1:100,1],img)  # Haralick texture features
  #all_features <- cbind(shape_features, intensity_features, texture_features, edge_features)
  
  features <- c(
    shape_feats[1, ],  # just take the first object if multiple found
    intensity_mean = basic_mean,
    intensity_sd = basic_sd,
    intensity_min = basic_min, #check this compare with previous ones
    intensity_max = basic_max, #check this compare with previous ones
    entropy = basic_entropy,
    variance = basic_variance
  )
  
  return(features)
}
process_all_images <- function(base_path) {
  folders <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
  results <- list()
  file_ids <- c()
  
  for (folder in folders) {
    img_path <- file.path(folder, "img")
    if (dir.exists(img_path)) {
      img_files <- list.files(img_path, pattern = "\\.jpg$", full.names = TRUE)
      for (img in img_files) {
        id <- paste0(basename(folder), "_", tools::file_path_sans_ext(basename(img)))
        features <- extract_image_features(img)
        if (!is.null(features)) {
          results[[id]] <- features
          file_ids <- c(file_ids, id)
        }
      }
    }
  }
  

  results <- results[!sapply(results, is.null)]  
  if (length(results) == 0) {
    stop("No valid image features were extracted. Check image paths and EBImage installation.")
  }
  

  max_len <- max(sapply(results, length))
  feature_matrix <- do.call(rbind, lapply(results, function(x) {
    c(x, rep(NA, max_len - length(x)))  # Fill missing values with NA
  }))
  
  rownames(feature_matrix) <- file_ids
  return(feature_matrix)
}

base_folder <- "~/MGS 662 Project/ProjectData_ForClass_v2/ProjectData_ForClass_v2"  # Adjust path
feature_matrix <- process_all_images(base_folder)
print(feature_matrix)
# Optional: Save to Excel
#write.xlsx(as.data.frame(feature_matrix), "image_features_matrix.xlsx")


# Function to extract AND compare full ID structure
natural_sort_ids <- function(ids) {
  # Extract all numeric components
  num_parts <- strsplit(ids, "_|s")
  
  # Convert to numeric matrix for proper comparison
  num_matrix <- sapply(num_parts, function(x) as.numeric(x[nchar(x) > 0]))
  
  # Order by first number, then second number, etc.
  ids[do.call(order, as.data.frame(t(num_matrix)))]
}

# Sort the matrix
sorted_ids <- natural_sort_ids(rownames(feature_matrix))
final_feature_matrix <- feature_matrix[sorted_ids, ]


