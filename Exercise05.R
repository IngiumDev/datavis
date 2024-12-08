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
ggcorr(expr[,-"tumor_type"])

expr_mat <- as.matrix(expr[,-"tumor_type"])
rownames(expr_mat) <- expr[,tumor_type]
pheatmap(expr_mat, cluster_rows = F,cluster_cols = F)
expr_melt <- melt(expr, id.vars='tumor_type')
expr_melt[order(-value)]
ggplot(expr, aes(FUK, UGP2)) + geom_point()
expr[tumor_type == "DOHH2", FUK := NA]
expr[tumor_type == "DOHH2", UGP2 := NA]

ggcorr(expr[, !"tumor_type"])
expr_mat <- as.matrix(expr[,-"tumor_type"])
rownames(expr_mat) <- expr[,tumor_type]
pheatmap(expr_mat, cluster_rows = F,cluster_cols = F)

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
plot.data <- as.matrix(iris[, -5])
pheatmap(plot.data, show_rownames=F, scale='column',
         clustering_method = "complete")
## pheatmap() returns an object with dendrograms
h_complete <- pheatmap(plot.data, show_rownames=F,
                       scale='column', clustering_method = "complete", silent=T)
# silent=T prevents heatmap to be displayed again
complete <- cutree(h_complete$tree_row, k = 3)

## label the row names to be able to annotate rows
rownames(plot.data) <- seq_len(nrow(plot.data))
## create a data.frame for the row annotations
row.ann <- data.table(Species = iris$Species)
row.ann[, complete:=factor(complete)] # the clusters need to be factors
## plot the heatmap with complete linkage clustering
pheatmap(plot.data, annotation_row = row.ann, show_rownames=F,
         scale='column', clustering_method = "complete")


d <- dist(iris_dt)
hc <- hclust(d, method = "complete")
plot(hc)
hc <- cutree(hc, k = 3)
pheatmap(as.matrix(iris_dt[, -"Species"]), cluster_rows = TRUE, scale='column', cluster_cols = FALSE, clustering_method="complete", cutree_rows = 3,show_rownames = TRUE, show_colnames = TRUE)

# Section 03
# a, the number of pairs of elements in S that are in the same subset in X and in the same subset in Y
# b, the number of pairs of elements in S that are in different subsets in X and in different subsets in Y
# a: 1=1
# b: 1+1+1+1+1+1+1+1+1+1=10
# Rand index= 0.7333

# Section 04
# Assignment:
# X1: D,B,A
# X2: C, E
# X1= 1/3(5+10+8,8+7+5) =(7.67,6.67)
# X2 = 1/2(4+6,6+4) = (5,5)

scaled.data <- scale(plot.data) # it is important to scale your data for k-means
km <- kmeans(scaled.data, 3)
km

# The table allows us to see in how many observations the methods coincided
# for each of the 3 clusters
table(complete, km$cluster)
row.ann[, kmeans := factor(km$cluster)]
pheatmap(scaled.data, annotation_row=row.ann, show_rownames=F,
         clustering_method = "complete")

# Section 05
library(fossil)
class(complete)
rand.index(complete, complete)
row.ann[, Species:= as.numeric(Species)]

rand <- apply(row.ann, 2, function(i)
  apply(row.ann, 2, function(j) rand.index(as.numeric(i), as.numeric(j))))
rand

pheatmap(rand)
rand_dt <- data.table(rand, keep.rownames = 'Clustering1') %>% melt(id.vars='Clustering1',
                                                                    value.name='rand_index',
                                                                    variable.name='Clustering2')
rand_dt[rand_index<1 & Clustering1=='Species'][which.max(rand_index)]

# Section 06
# B is liklely complete, as the axis is much larger and the complete linkage criteria uses the maximum dissimilarities between the cluster contents leading to larger dissimilarity

# Section 07
iris_dt <- as.data.table(iris)
X <- iris_dt[Species == "setosa", -"Species"]
pca_res <- prcomp(X, scale. = TRUE, center = TRUE)
pca_res
summary(pca_res)
plot(pca_res, type = "l")

prediction <- predict(pca_res)
ggplot(data = as.data.table(prediction), aes(x = PC1, y = PC2,)) +
  geom_point()
biplot(pca_res)
# All variables contribute negatively to PC1, suggesting that increasing the flower size decreases the PC1 value which probably means that PC1 describes the size of the flower in some way

pc_iris <- cbind(iris_dt[Species == "setosa"], as.data.table(prediction))
pc_iris <- melt(pc_iris, id.vars = c("Species", 'PC1', 'PC2', 'PC3', 'PC4'))
ggplot(pc_iris, aes(value, PC1)) + geom_point() + facet_wrap(~variable, scales = 'free')
#Supports our pervious observation
#With all
iris_dt <- as.data.table(iris)
X <- iris_dt[, -"Species"]
pca_res <- prcomp(X, scale. = TRUE, center = TRUE)
pca_res
summary(pca_res)
plot(pca_res, type = "l")

prediction <- predict(pca_res)
ggplot(data = as.data.table(prediction), aes(x = PC1, y = PC2,)) +
  geom_point(aes(color = iris_dt$Species))
biplot(pca_res)

pc_iris <- cbind(iris_dt, as.data.table(prediction))
pc_iris <- melt(pc_iris, id.vars = c("Species", 'PC1', 'PC2', 'PC3', 'PC4'))
ggplot(pc_iris, aes(value, PC1)) + geom_point(aes(color = pc_iris$Species)) + facet_wrap(~variable, scales = 'free')

# It looks like PC1 is separating the species from another