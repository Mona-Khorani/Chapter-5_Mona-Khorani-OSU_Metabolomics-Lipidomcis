library(dplyr)  # Load the dplyr package
library(ggplot2)
library(ggpubr)
library(ggsignif)
# Read the dataset from the CSV file
your_dataset <- read.csv("HexCer.csv")

# Extract the variable names from the first column
variable_names <- your_dataset[, 1]

# Create a data frame to hold the tidy data
tidy_data <- data.frame(
  Variable = rep(variable_names, each = ncol(your_dataset) - 1),
  Group = rep(colnames(your_dataset)[-1], each = length(variable_names)),
  Value = as.vector(as.matrix(your_dataset[, -1]))  # Exclude the first column
)

# Remove the numeric suffixes from the Group column
tidy_data$Group <- gsub("\\.\\d+$", "", tidy_data$Group)

# Print the first few rows of the tidy_data
print(head(tidy_data))


# Assuming your tidy data is named "tidy_data"


p <- ggplot(tidy_data, aes(x = Group, y = Value)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, aes(color = ifelse(Group %in% c("Hip.FCI", "Hip.FCS"), "#003F70", "#DF7027"))) +  # Set box border color based on group
  
  
  geom_jitter(width = 0.2, height = 0, size = 0.1, alpha = 0.5) + 
  
  
  labs(x = "Group", y = "Abundance") +
  ggtitle("Box Plot Comparing Different Groups in Hip PC") +
  stat_summary(
    fun.data = "mean_sdl",
    geom = "point",
    position = position_dodge(width = 0.75),
    size = 0.6,
    shape = 23,
    fill = "white",
    color = "black"
  ) +
  # scale_fill_manual(values = c("Hip.FCI" = "#003F70", "Hip.NOI" = "#003F70", 
  #                            "Hip.FCS" = "#DF7027", "Hip.NOS" = "#DF7027")) +  # Define custom fill colors
  scale_color_manual(values = c("#003F70" = "#003F70", "#DF7027" = "#DF7027")) +  # Define custom border colors
  
  
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10, margin = margin(b = 0.5)) # Set plot title to bold and center it
  ) 

# Define comparisons for p-values
comparisons <- list(
  c("Hip.FCI", "Hip.FCS"),
  c("Hip.NOI", "Hip.NOS")
)

# Create p-values annotation with asterisks
pvals <- lapply(comparisons, function(cmp) {
  wilcox.test(tidy_data$Value[tidy_data$Group == cmp[1]],
              tidy_data$Value[tidy_data$Group == cmp[2]])$p.value
})
pvals <- unlist(pvals)

# Determine asterisks based on p-value significance
asterisks <- sapply(pvals, function(p) {
  if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else ""
})

# Combine the p-values with comparisons and asterisks
pval_labels <- paste(asterisks)

# Add p-values with asterisks to the box plots
p_with_pvals <- p +
  stat_compare_means(comparisons = comparisons, label = "p.signif",
                     method = "wilcox.test", paired = FALSE, step.increase = 0) +
  labs(subtitle = pval_labels)

# Display the plot with p-values and asterisks
print(p_with_pvals)
ggsave("PC.png", plot = p_with_pvals, dpi = 400)









your_dataset <- read.csv("PI.csv")

# Extract the variable names from the first column
variable_names <- your_dataset[, 1]

# Create a data frame to hold the tidy data
tidy_data <- data.frame(
  Variable = rep(variable_names, each = ncol(your_dataset) - 1),
  Group = rep(colnames(your_dataset)[-1], each = length(variable_names)),
  Value = as.vector(as.matrix(your_dataset[, -1]))  # Exclude the first column
)

# Remove the numeric suffixes from the Group column
tidy_data$Group <- gsub("\\.\\d+$", "", tidy_data$Group)

# Calculate the means for each group
group_means <- tidy_data %>%
  group_by(Group) %>%
  summarise(Mean = mean(Value))

# Define the groups you want to compare
comparisons <- list(
  c("Hip.FCI", "Hip.FCS"),
  c("Hip.NOI", "Hip.NOS")
)

# Perform pairwise comparisons and determine if mean increased or decreased
for (cmp in comparisons) {
  mean_diff <- group_means$Mean[group_means$Group == cmp[2]] - group_means$Mean[group_means$Group == cmp[1]]
  if (mean_diff > 0) {
    direction <- "Increased"
  } else if (mean_diff < 0) {
    direction <- "Decreased"
  } else {
    direction <- "No Change"
  }
  
  message(paste("Comparison", cmp[1], "to", cmp[2], ":", direction))
}





# Load required packages
library(dplyr)

# Read the dataset from the CSV file
your_dataset <- read.csv("PI.csv")

# Extract the variable names from the first column
variable_names <- your_dataset[, 1]

# Create a data frame to hold the tidy data
tidy_data <- data.frame(
  Variable = rep(variable_names, each = ncol(your_dataset) - 1),
  Group = rep(colnames(your_dataset)[-1], each = length(variable_names)),
  Value = as.vector(as.matrix(your_dataset[, -1]))  # Exclude the first column
)

# Remove the numeric suffixes from the Group column
tidy_data$Group <- gsub("\\.\\d+$", "", tidy_data$Group)

# Calculate the means for each group
group_means <- tidy_data %>%
  group_by(Group) %>%
  summarise(Mean = mean(Value))

# Define the groups you want to compare
comparisons <- list(
  c("Hip.FCI", "Hip.FCS"),
  c("Hip.NOI", "Hip.NOS")
)

# Perform pairwise comparisons and determine if mean increased or decreased
for (cmp in comparisons) {
  mean_diff <- group_means$Mean[group_means$Group == cmp[2]] - group_means$Mean[group_means$Group == cmp[1]]
  
  # Calculate fold change and log2 fold change
  fc <- foldchange(group_means$Mean[group_means$Group == cmp[2]], group_means$Mean[group_means$Group == cmp[1]])
  logratio <- log2fc <- logratio2foldchange(log2(group_means$Mean[group_means$Group == cmp[2]] / group_means$Mean[group_means$Group == cmp[1]]), base = 2)
  
  if (mean_diff > 0) {
    direction <- "Increased"
  } else if (mean_diff < 0) {
    direction <- "Decreased"
  } else {
    direction <- "No Change"
  }
  
  message(paste("Comparison", cmp[1], "to", cmp[2], ":", direction))
  message(paste("Fold Change:", fc))
  message(paste("Log2 Fold Change:", log2fc))
  message("--------")
}



