library(lsa)

# załadowanie skryptu z macierzą częstości
source_file <- "./scripts/frequency_matrix.R"
source(source_file)

# utworzenie katalogu na wyniki
reduction_dir <- "./reduction"
dir.create(reduction_dir)

# legenda
doc_names <- rownames(dtm_tfidf_bounds)
doc_count <- length(doc_names)
legend <- paste(
  paste(
    "d",
    1:doc_count,
    sep = ""
  ),
  doc_names,
  sep = " -> "
)
options(scipen = 5)

# kolory
clusters_pattern <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3)
cols = c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink")
cols_pattern <- cols[clusters_pattern]
names(clusters_pattern) <- doc_names
names(cols_pattern) <- doc_names

# analiza głównych składowych
pca_model <- prcomp(dtm_tfidf_bounds)

x <- pca_model$x[,1]
y <- pca_model$x[,2]

plot_file <- paste(
  reduction_dir,
  "pca.png",
  sep = "/"
)
png(plot_file, width = 800)
par(mar = c(4, 4, 4, 25), xpd = TRUE)
plot(
  x,
  y,
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2",
  col = cols_pattern,
  pch = 16
)
text(
  x,
  y,
  paste(
    "d",
    1:doc_count,
    sep = ""
  ),
  col = cols_pattern,
  pos = 1
)
legend(
  "topright",
  inset = c(-0.9, 0.1),
  legend,
  text.col = cols_pattern
)
dev.off()
  
# analiza ukrytych wymiarów semantycznych
# dekompozycja według wartości osobliwych
lsa_model <- lsa(tdm_tf_bounds_m)

coord_docs <- lsa_model$dk%*%diag(lsa_model$sk)
coord_terms <- lsa_model$tk%*%diag(lsa_model$sk)

terms_importance <- diag(
  lsa_model$tk%*%diag(lsa_model$sk)%*%t(diag(lsa_model$sk))%*%t(lsa_model$tk)
)
important_terms <- names(
  tail(
    sort(terms_importance),
    30
  )
)
own_terms <- c("harry", "hermiona", "edmund", "zuzanna", "edward", "bell")
current_terms <- own_terms

x1 <- coord_docs[,1]
y1 <- coord_docs[,2]
x2 <- coord_terms[current_terms,1]
y2 <- coord_terms[current_terms,2]

plot_file <- paste(
  reduction_dir,
  "lsa.png",
  sep = "/"
)
png(plot_file, width = 800)
par(mar = c(4, 4, 4, 25), xpd = TRUE)
plot(
  x1,
  y1,
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "SD2",
  col = cols_pattern,
  pch = 16
)
text(
  x1,
  y1,
  paste(
    "d",
    1:doc_count,
    sep = ""
  ),
  col = cols_pattern,
  pos = 1
)
points(
  x2, 
  y2,
  pch = 15,
  col = "magenta"
)
text(
  x2,
  y2,
  rownames(coord_terms[current_terms,]),
  col = "magenta",
  pos = 2
)
legend(
  "topright",
  inset = c(-0.9, 0.1),
  legend,
  text.col = cols_pattern
)
dev.off()





