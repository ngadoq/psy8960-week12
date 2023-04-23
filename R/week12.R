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
  # Remove stop words
  tm_map(removeWords, stopwords("en"))
  
  

