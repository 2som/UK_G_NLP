# załadowanie bibliotek
library(wordcloud)

# załadowanie skryptu z macierzą częstości
source_file <- "./scripts/frequency_matrix.R"
source(source_file)

# utworzenie katalogu na wyniki
clouds_dir <- "./clouds"
dir.create(clouds_dir)

# waga tf jako miara ważności słów w dokumentach
for (doc_no in 1:length(corpus)) {
  print(rownames(dtm_tf_all_m)[doc_no])
  print(head(sort(dtm_tf_all_m[doc_no,], decreasing = T)))
}

# waga tfidf jako miara ważności słów w dokumentach
for (doc_no in 1:length(corpus)) {
  print(rownames(dtm_tfidf_all_m)[doc_no])
  print(head(sort(dtm_tfidf_all_m[doc_no,], decreasing = T)))
}

# prawdopodobieństwo w lda jako miara ważnosci słów


#chmury tagów
for (doc_no in 1:length(corpus)) {
  cloud_file <- paste(
    clouds_dir,
    paste(corpus[[doc_no]]$meta$id, ".png", sep = ""),
    sep = "/"
  )
  png(cloud_file)
  wordcloud(
    corpus[doc_no],
    max.words = 200,
    colors = brewer.pal(8,"PuOr")
  )
  dev.off()
}
