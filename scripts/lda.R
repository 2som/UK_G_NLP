# załadowanie bibliotek
library(topicmodels)

# załadowanie skryptu z macierzą częstości
source_file <- "./scripts/frequency_matrix.R"
source(source_file)

# utworzenie katalogu na wyniki
topics_dir <- "./topics"
dir.create(topics_dir)

# Analiza Ukrytej Alokacji Dirichlet'a (Latent Dirichlet Allocation method)
words_count <- ncol(dtm_tf_all)
topics_count <- 6
lda_model <- LDA(
  dtm_tf_all,
  topics_count,
  method = "Gibbs",
  control = list(
    burnin = 2000,
    thin = 100,
    iter = 3000
  )
)
results <- posterior(lda_model)
cols = c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink")

# prezentacja tematów
for (topic_no in 1:topics_count) {
  topic_file <- paste(
    topics_dir,
    paste("Temat", topic_no, ".png"),
    sep = "/"
  )
  png(topic_file)
  par(mai = c(1,2,1,1))
  topic <- tail(sort(results$terms[topic_no,]),20)
  barplot(
    topic,
    horiz = TRUE,
    las = 1,
    main = paste("Temat", topic_no),
    xlab = "Prawdopodobieństwo",
    col = cols[topic_no]
  )
  dev.off()
}

#prezentacja dokumentów
plot_file <- paste(topics_dir, "Dokumenty.png",sep = "/")
png(plot_file)
par(mai = c(1,4,1,1))
barplot(
  t(results$topics),
  horiz = TRUE,
  las = 1,
  main = "Dokumenty",
  xlab = "Prawdopodobieństwo",
  col = cols
)
dev.off()
