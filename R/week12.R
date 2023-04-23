# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(wordcloud)
library(tidytext)
library(topicmodels)

# Data Import and Cleaning
week12_tbl <- readRDS("week12.RDS")  

# Create a corpus called io_corpus_original from the titles in week12_tbl 
io_corpus_original <- VCorpus(VectorSource(week12_tbl$title))
# Create a new lemmatized pre-processed corpus from io_corpus_original called io_corpus


io_corpus <- io_corpus_original %>% 
  # Convert all to lowercase
  tm_map(content_transformer(str_to_lower)) %>% 
  # Remove references to IO Psychology
  tm_map(content_transformer(function(x) {
    x <- gsub("io psychology|industrial-organizational psychology|industrial and organizational psychology|i/o psychology|i-o psychology|i-o psych|i-o|i/o|io", "", x)})) %>% 
  # Remove extra whitespace
  tm_map(stripWhitespace) %>% 
  # Remove English stop words
  tm_map(removeWords, stopwords("en")) %>% 
  # Remove punctuation 
  tm_map(removePunctuation) 

# Write function compare_them 

compare_them <- function(corpus1, corpus2) {
  # Select a random row from each corpus
  random_row <- sample(1:min(length(corpus1), length(corpus2)), 1) 
  # Get the content of the selected row from each corpus
  corpus1_random <- corpus1[[random_row]]$content
  corpus2_random <- corpus2[[random_row]]$content
  
  # Print the content of both corpora
  cat("Corpus 1:\n", corpus1_random, "\n\n")
  cat("Corpus 2:\n", corpus2_random, "\n\n")
}

# Compare io_corpus and io_corpus_original
compare_them(io_corpus, io_corpus_original)

# Create a bigram DTM called io_dtm
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}
io_dtm <- DocumentTermMatrix(
  io_corpus, 
  control = list(tokenize = tokenizer)
)
# Create slim DTM
io_slim_dtm <- removeSparseTerms(io_dtm, 1/3)
io_slim_dtm

# Use latent Dirichlet allocation to categorize posts into topics from io_dtm
# Convert DTM to tidy format
io_tidy <- tidy(io_dtm)

dtm_io <- io_tidy %>% 
  cast_dtm(document, term, count) 


# Create model
io_lda <- topicmodels::LDA(dtm_io,
                           k = 2,
                           method = "Gibbs",
                           control = list(seed = 42)
)

# Glimpse the topic model output
glimpse(io_lda)

# Tidy the matrix of word probabilities
lda_topics <- io_lda %>% 
  tidy(matrix = "gamma") %>% 
  # Arrange the topics by document probabilities in descending order
  arrange(desc(gamma))

# Convert the VCorpus object to a tidy format
io_corpus_tidy <- tidy(io_corpus) %>% 
  rename("document" = "id")

# Join with original data to get post titles
topics_tbl <-  left_join(lda_topics, io_corpus_tidy, by= "document") %>% 
  select(doc_id=document, original=text, topic, probability=gamma  )

topics_tbl


# 1
# 2

# Create a wordcloud of io_dtm
wordcloud(io_tidy$term, io_tidy$count, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Interpret


