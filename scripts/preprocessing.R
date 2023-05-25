library(tm)
library(hunspell)

load_corpus <- function(corpus_dir) {
  corpus <- VCorpus(
    DirSource(corpus_dir, "UTF-8", "*.txt"),
    readerControl = list(language = "pl_PL")
  )
  return(corpus)
}

load_stop_list_contents <- function (path) {
  stoplist <- readLines(
      path,
      encoding = "utf-8"
  )
  return(stoplist)
}

preprocess_document <- function(corpus, stop_list) {
  cut_extension <- function(corpus){
  meta(corpus, "id") <- gsub("\\.txt$", "", meta(corpus, "id"))
  return(corpus)
}
  paste_paragraphs <- content_transformer(function(text) paste(text, collapse = " "))
  remove_numbers <- content_transformer(function(text) gsub("\\d+", "", text))
  remove_punctuation <- content_transformer(function(text) gsub("[[:punct:]]", "", text))
  lowercase <- content_transformer(tolower)
  remove_char <- content_transformer(function(text, char) gsub(char, "", text))
  trim <- content_transformer(trimws)
  
  corpus <- tm_map(corpus, cut_extension)
  corpus <- tm_map(corpus, paste_paragraphs)
  corpus <- tm_map(corpus, remove_numbers)
  corpus <- tm_map(corpus, remove_punctuation)
  corpus <- tm_map(corpus, lowercase)

  corpus <- tm_map(corpus, removeWords, stop_list)
  corpus <- tm_map(corpus, remove_char, intToUtf8(8722))
  corpus <- tm_map(corpus, remove_char, intToUtf8(190))
  corpus <- tm_map(corpus, trim)
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

lemmatize_document <- function(document) {
  polish <- dictionary("pl_PL")
  
  lemmatize <- function(text) {
    parsed_text_vec <- unlist(hunspell_parse(text, dict = polish))
    lemmatized_text_vec <- hunspell_stem(parsed_text_vec, dict = polish)
    
    for (t in 1:length(lemmatized_text_vec)) {preprocessed_dir
      if (length(lemmatized_text_vec[[t]]) == 0) {
        lemmatized_text_vec[t] <- parsed_text_vec[t]
      }
      if (length(lemmatized_text_vec[[t]]) > 1) {
        lemmatized_text_vec[t] <- lemmatized_text_vec[[t]][1]
      }
    }
    
    lemmatized_text <- paste(lemmatized_text_vec, collapse = " ")
    return(lemmatized_text)
  }
  
  document <- tm_map(document, content_transformer(lemmatize))
  return(document)
}

export_corpus <- function(corpus, output_dir) {
  dir.create(output_dir, showWarnings = FALSE)
  writeCorpus(corpus, output_dir)
}


corpus_dir <- "./documents"
preprocessed_dir <- "./processed_documents"

stop_list = load_stop_list_contents("./stopwords_pl.txt")

corpus <- load_corpus(corpus_dir)

corpus <- preprocess_document(corpus, stop_list)
corpus <- lemmatize_document(corpus)

export_corpus(corpus, preprocessed_dir)
export_corpus(corpus, preprocessed_dir)


