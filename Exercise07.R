# Section 00
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork)# optional, helps assembling plots into a single figure
# Load the yeast data
# We perform some reshapes to make the data easy to use
growth_analysis <- fread("./extdata/eqtl/growth_analysis.txt") %>%
  select(yeast_ID = strain, growth_rate, gene_A = gene_5211,
         gene_B = gene_5091, gene_C = gene_1653)
recode_values <- c("LAB", "WILD")
growth_analysis <- melt(growth_analysis,id=c("yeast_ID","growth_rate"),
                        variable.name = "gene", value.name = "variant")
growth_analysis[, variant := recode_values[variant + 1]]
# Section 01
# Function to plot growth vs genotype for one gene
plot_growth_one_gene <- function(gene_name) {
  ggplot(growth_analysis[gene == gene_name],
         aes_string(x = "variant", y = "growth_rate")) +
    geom_boxplot() +
    labs(title = paste("Growth Rate by", gene_name), x = gene_name, y = "Growth Rate") +
    scale_x_discrete(labels = c("LAB" = "Lab variant", "WILD" = "Wild variant")) +
    theme_bw(base_size = 16)
}
plot_growth_one_gene("gene_A")
plot_growth_one_gene("gene_B")
plot_growth_one_gene("gene_C")
# Function to calculate the difference of the medians of two variants
median_diff <- function(dt){
  dt[variant == "WILD", median(growth_rate, na.rm = TRUE)] -
    dt[variant == "LAB", median(growth_rate, na.rm = TRUE)]
}
# Function to permute the table, plot the resulting histogram
# and compute a p-value
set.seed(0)
p_val_medians <- function(gene_name, N_permu = 1000){
  # It will return both a pvalue and plot a histogram of T_star
  # reduce to gene of interest
  dt <- growth_analysis[gene == gene_name]
  # Compute median difference for actual data
  T_ref <- median_diff(dt)
  # Permute
  T_star <- sapply(1:N_permu, function(x){
    median_diff(dt[, variant := sample(variant)]) })
  # Plot
  g <- ggplot(data = data.table(T_star = T_star), aes(T_star)) + geom_histogram() +
    geom_vline(aes(xintercept=T_ref, color="T_ref")) + xlim(-3,3)
  print(g) # Needed to render plot inside function call
  # Compute and return the p value
  # First compute each tail seperately
  p_val_right <- (sum(T_star >= T_ref) + 1) / (N_permu + 1)
  p_val_left <- (sum(T_star <= T_ref) + 1) / (N_permu + 1)
  # Then combine the above to obtain the double sided p-value.
  p_val <- 2 * min(p_val_right, p_val_left)
  return(p_val)
}
# Calling the function:
p_val_medians("gene_A")
p_val_medians("gene_B")
p_val_medians("gene_C")

# Section 02
growth_analysis_cast <- dcast(growth_analysis, ... ~ gene, value.var= "variant")

ggplot(growth_analysis_cast, aes(x = gene_B, fill = gene_A)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Dependency of Gene A on Gene B",
    x = "Gene B Variant",
    y = "Frequency",
    fill = "Gene A Variant"
  )

table_A_B <- table(growth_analysis_cast$gene_A, growth_analysis_cast$gene_B)
observed_stat <- chisq.test(table_A_B, correct = FALSE)$statistic
observed_stat
# Set the number of permutations
n_permutations <- 1000

# Initialize a vector to store permuted statistics
null_distribution <- numeric(n_permutations)

# Perform permutations
set.seed(0)  # For reproducibility
for (i in 1:n_permutations) {
  # Shuffle gene_A
  permuted_gene_A <- sample(growth_analysis_cast$gene_A)

  # Create a contingency table with permuted data
  permuted_table <- table(permuted_gene_A, growth_analysis_cast$gene_B)

  # Compute the chi-squared statistic for permuted data
  null_distribution[i] <- chisq.test(permuted_table, correct = FALSE)$statistic
}
p_value <- mean(null_distribution >= observed_stat)
print(paste("P-value:", p_value))
# Plot histogram of the null distribution
hist(null_distribution, breaks = 30, col = "lightblue",
     main = "Null Distribution of Chi-squared Statistic",
     xlab = "Chi-squared Statistic", xlim = range(c(null_distribution, observed_stat)))
abline(v = observed_stat, col = "red", lwd = 2)
legend("topright", legend = c("Observed Statistic"), col = "red", lwd = 2)

# Section 03
