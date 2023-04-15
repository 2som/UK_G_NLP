# załadowanie bibliotek
library(tm)

# utworzenie korpusu dokumentów
corpus_dir <- "./Literatura - streszczenia - przetworzone"
corpus <- VCorpus(
  DirSource(
    corpus_dir,
    "UTF-8",
    "*.txt"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

# dodatkowe funkcje transformujące
cut_extension <- function(document){
  meta(document, "id") <- gsub("\\.txt$", "", meta(document, "id"))
  return(document)
}

# wstępne przetwarzanie
corpus <- tm_map(corpus, cut_extension)

# tworzenie macierzy częstości
tdm_tf_all <- TermDocumentMatrix(corpus)
dtm_tf_all <- DocumentTermMatrix(corpus)
dtm_tfidf_all <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
dtm_tf_bounds <- DocumentTermMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
dtm_tfidf_bounds <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
tdm_tf_bounds <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
dtm_tf_all_m <- as.matrix(dtm_tf_all)
dtm_tfidf_all_m <- as.matrix(dtm_tfidf_all)
dtm_tfidf_bounds_m <- as.matrix(dtm_tfidf_bounds)
tdm_tf_bounds_m <- as.matrix(tdm_tf_bounds)


