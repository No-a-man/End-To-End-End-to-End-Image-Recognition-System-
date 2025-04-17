# Writing the Majority Vote Function
majority_vote <- function(df, cols) {
  ratings <- df[, cols]
  apply(ratings, 1, function(x) {
    x <- na.omit(as.numeric(x))
    ones <- sum(x == 1)
    zeros <- sum(x == 0)
    if (ones > zeros) return(1)
    if (zeros > ones) return(0)
    sample(c(0, 1), 1)  # if there is a tie breaker than Random tie breaker
  })
}

# Loading and creating voting for Tree using Krippendorff's alpha coefficient

mythLabels <- majority_vote(AnnotationMyth[1:83, ], c("Ann1", "Ann3"))
treeLabels <- majority_vote(AnnotationTree[1:83, ], c("Ann2", "Ann4"))
animalLabels <- majority_vote(AnnotationAnimal[1:83, ], c("Ann1", "Ann5"))

