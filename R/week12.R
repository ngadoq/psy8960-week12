# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(wordcloud)
library(tidytext)

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

io_slim_dtm <- removeSparseTerms(io_dtm, 1/2, 1/3)
io_slim_dtm



  
  

