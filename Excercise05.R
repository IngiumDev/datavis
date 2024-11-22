# Section 00
library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator
library(tidyr)
library(GGally)
library(pheatmap)
library(mclust)

# Section 01
expr <- readRDS("extdata/lec05_data/exercise/cancer_data.rds") %>% as.data.table(keep.rownames="tumor_type")
head(expr[, 1:6])
ggcorr(expr)

pheatmap(as.matrix(expr[,-"tumor_type"]), cluster_rows = TRUE, scale='column',cluster_cols = FALSE)


expr_long <- melt(expr, id.vars = "tumor_type", variable.name = "Gene", value.name = "Expression")
ggplot(expr_long, aes(x = Gene, y = Expression)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Remove outlier Val above 50 in FUK and UGP2, replace with na
expr[FUK > 50, FUK := NA]
expr[UGP2 > 50, UGP2 := NA]
expr_long <- melt(expr, id.vars = "tumor_type", variable.name = "Gene", value.name = "Expression")
ggplot(expr_long, aes(x = Gene, y = Expression)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggcorr(expr)

pheatmap(as.matrix(expr[,-"tumor_type"]), cluster_rows = TRUE, scale='column',cluster_cols = FALSE)

# Section 02
iris_dt <- as.data.table(iris)

pheatmap(as.matrix(iris_dt[, -"Species"]), cluster_rows = FALSE, scale='column', cluster_cols = FALSE)

pheatmap(as.matrix(iris_dt[, -"Species"]), cluster_rows = TRUE, scale='column', cluster_cols = FALSE, clustering_method="complete")

d <- dist(iris_dt)
hc <- hclust(d, method = "complete")
plot(hc)
hc <- cutree(hc, k = 3)
pheatmap(as.matrix(iris_dt[, -"Species"]), cluster_rows = TRUE, scale='column', cluster_cols = FALSE, clustering_method="complete", cutree_rows = 3,show_rownames = TRUE, show_colnames = TRUE)