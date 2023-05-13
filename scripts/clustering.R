# załadowanie bibliotek
library(proxy)
library(dendextend)
library(corrplot)
library(flexclust)

# załadowanie skryptu z macierzą częstości
source_file <- "./scripts/frequency_matrix.R"
source(source_file)

# utworzenie katalogu na wyniki
clusters_dir <- "./clusters"
dir.create(clusters_dir)

# analiza skupień

clusters_pattern <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3)
cols = c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink")
cols_pattern <- cols[clusters_pattern]

frequency_matrix <- dtm_tfidf_bounds_m
measure <- "cosine"
method <- "ward.D2"
name <- "tfidf_bounds"

doc_names <- rownames(frequency_matrix)
doc_count <- length(doc_names)
names(clusters_pattern) <- doc_names
names(cols_pattern) <- doc_names

dist_matrix <- dist(frequency_matrix, method = measure)
hierarch_clust <- hclust(dist_matrix, method = method)
plot_file <- paste(
  clusters_dir,
  paste("dend_",name,"_base.png", sep = ""),
  sep = "/"
)
png(plot_file)
plot(hierarch_clust)
dev.off()
dend <- as.dendrogram(hierarch_clust)
clusters_count <- find_k(dend)$k
plot_file <- paste(
  clusters_dir,
  paste("dend_",name,"_color.png", sep = ""),
  sep = "/"
)
dend_colored <- color_branches(
  dend,
  k = clusters_count,
  col = cols
)
png(plot_file, width = 600)
par(mai = c(1,1,1,4))
plot(dend_colored, horiz = T)
dev.off()
plot_file <- paste(
  clusters_dir,
  paste("dend_",name,"_pattern.png", sep = ""),
  sep = "/"
)
dend_colored <- color_branches(
  dend,
  col = cols_pattern[dend %>% labels]
)
png(plot_file, width = 600)
par(mai = c(1,1,1,4))
plot(dend_colored, horiz = T)
dev.off()  
clusters <- cutree(hierarch_clust, k = clusters_count)
clusters_matrix = matrix(0, doc_count, clusters_count)
rownames(clusters_matrix) <- doc_names
for (doc in 1:doc_count) {
  clusters_matrix[doc, clusters[doc]] <- 1
}
plot_file <- paste(
  clusters_dir,
  paste("matrix_",name,".png", sep = ""),
  sep = "/"
)
png(plot_file)
# par(mai = c(1,1,1,4))
corrplot(clusters_matrix)
dev.off()  

rand_pattern_experiment <- comPart(clusters, clusters_pattern)
rand_experiment1_experiment2 <- comPart(clusters_1, clusters_2)

plot_file <- paste(
  clusters_dir,
  "FM_index.png",
  sep = "/"
)
png(plot_file)
Bk_plot(
  dend_1,
  dend_2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a - Mallows'a"
)
dev.off()

