# Install required packages if not installed
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
library(ggplot2)
library(reshape2)

# Simulate loss curves for each model (replace with actual training history if available)
iterations <- seq(1, 100)
set.seed(123)

# Myth model loss (cross-entropy)
loss_myth <- exp(-0.05 * iterations) + rnorm(100, sd = 0.02)

# Tree model loss (logistic)
loss_tree <- exp(-0.07 * iterations) + rnorm(100, sd = 0.02)

# Animal model loss
loss_animal <- exp(-0.06 * iterations) + rnorm(100, sd = 0.02)

# Create data frame
loss_data <- data.frame(
  Iteration = iterations,
  Myth = loss_myth,
  Tree = loss_tree,
  Animal = loss_animal
)

# Melt for ggplot
melted_loss <- melt(loss_data, id.vars = "Iteration", variable.name = "Model", value.name = "Loss")

# Plot loss curves
ggplot(melted_loss, aes(x = Iteration, y = Loss, color = Model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1, alpha = 0.5) +
  labs(title = "Training Loss by Iteration",
       subtitle = "Multi-label Classification Models",
       x = "Training Iteration",
       y = "Loss Value",
       color = "Label Type") +
  scale_color_manual(values = c("Myth" = "#E69F00", 
                                "Tree" = "#56B4E9", 
                                "Animal" = "#009E73")) +
  theme_minimal() +
  theme(legend.position = "bottom")