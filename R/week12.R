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
library(ldatuning)

# Data Import and Cleaning
week12_tbl <- readRDS("week12.RDS")  

# Create a corpus called io_corpus_original from the titles in week12_tbl 
io_corpus_original <- VCorpus(VectorSource(week12_tbl$title))
# Create a new lemmatized pre-processed corpus from io_corpus_original called io_corpus

io_corpus <- io_corpus_original %>% 
  # Convert all to lowercase for easy process later
  tm_map(content_transformer(tolower)) %>% 
  # Remove numbers since we're only intersted in words
  tm_map(removeNumbers) %>% 
  # Remove references to IO Psychology
  tm_map(removeWords, c("io psychology", "industrial-organizational psychology", "industrial and organizational psychology", "i/o psychology", "i-o psychology", "i-o psych", "i-o", "i/o", "io", "industrial/organizational psychology", "IO psychology")) %>% 
  # Remove extra white space
  tm_map(stripWhitespace) %>% 
  # Remove English stop words
  tm_map(removeWords, stopwords("en")) %>% 
  # Remove punctuation 
  tm_map(removePunctuation) %>% 
  # Remove words that don't add meaning
  tm_map(removeWords, c("discussion", "riopsychology", "biweekly", "reading"))

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

# Analysis
# Create a bigram DTM called io_dtm
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}
io_dtm <- DocumentTermMatrix(
  io_corpus, 
  control = list(tokenize = tokenizer)
)
# Create slim DTM
io_slim_dtm <- removeSparseTerms(io_dtm, sparse = .998)
io_slim_dtm

# Filter out rows with all zeros
dtm_new <- DTM_tbl[rowSums(DTM_tbl) > 0, ]


tuning <- FindTopicsNumber(
    io_slim_dtm,
    topics = seq(2,7,1), 
    metrics = c("Griffiths2004",
                "CaoJuan2009",
                "Arun2010",
                "Deveaud2014"), verbose = T
  )

FindTopicsNumber_plot(tuning)
# From the plot it seems like 4 is a good number of topics

# Use latent Dirichlet allocation to categorize posts into topics from io_dtm

# Convert DTM to tidy format
io_tidy <- tidy(io_dtm)

dtm_io <- io_tidy %>% 
  cast_dtm(document, term, count) 

# Create model
io_lda <- topicmodels::LDA(dtm_io,
                           k = 4,
                           method = "Gibbs",
                           control = list(seed = 42)
)

# Glimpse the topic model output
glimpse(io_lda)

# Tidy the matrix of word probabilities
lda_topics <- io_lda %>% 
  tidy(matrix = "gamma") 

lda_gammas <- lda_topics %>% 
  group_by(document) %>% 
  top_n(1, gamma) %>% 
  slice(1) %>% 
  ungroup %>% 
  mutate(document = as.numeric(document)) %>% 
  arrange(document)

# Convert the VCorpus object to a tidy format
io_corpus_tidy <- tidy(io_corpus) %>% 
  rename("document" = "id") %>% 
  mutate(document = as.numeric(document))

# Join with original data to get post titles
topics_tbl <-  left_join(lda_gammas, io_corpus_tidy, by= "document") %>% 
  select(doc_id=document, original=text, topic, probability=gamma)

# beta matrix
io_lda %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(desc(beta)) %>% View()

# Based on the beta matrix, I'd choose my final topic list: discussion for topic 2, academia/school for topic 3, and career for topic 1.
# My topics derived from beta matrix seem match with the original posts. However, they seem to blend together. For example, some posts that I categorize as topic 3/academic also talked about career. I assume this is related to construct validity. 

# Visualization

# Calculate the col sums of DTM_tbl
# Create DTM_tbl
DTM_tbl <- as_tibble(as.matrix(io_dtm))
# Create values for word cloud
term_frequency <- colSums(DTM_tbl)
terms_vec = names(DTM_tbl)

# Create a wordcloud of io_dtm
wordcloud(words=terms_vec, freq = term_frequency, scale=c(1, .5), random.order=FALSE, max.words = 50, colors=brewer.pal(8, "Dark2"))

# It seems like the most common topics are SIOP conference, people analytics and grad school. 

# Create a dataset called final_tbl that contains the contents of topics_tbl plus the upvote count
week12_tbl$doc_id <- as.numeric(rownames(week12_tbl))

final_tbl <- left_join(topics_tbl, select(week12_tbl, c(doc_id, upvotes)), by= "doc_id") %>% 
  mutate(topic =as.factor(topic))

# Run ANOVA to determine if upvotes differs by topic
anova_model <- aov(upvotes ~ topic, data = final_tbl)
summary(anova_model)

# Based on ANOVA results, there is significant difference in upvotes between topics, F(3,668) = 3.09, p <.05.

                       