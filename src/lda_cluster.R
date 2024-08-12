# Load necessary libraries
library(dplyr)
library(tidytext)
library(topicmodels)
library(tm)
library(tidyr)
library(reshape2)

# Load your data
data <- read.csv('data/libinsights-full21-24-dataframe.csv')
data <- clean_names(data)

# Preprocess the text data
data_clean <- data %>%
  mutate(text_clean = tolower(response_3)) %>%
  mutate(text_clean = removePunctuation(text_clean)) %>%
  mutate(text_clean = removeNumbers(text_clean)) %>%
  mutate(text_clean = removeWords(text_clean, stopwords("en")))

# Create a Document-Term Matrix
dtm <- DocumentTermMatrix(Corpus(VectorSource(data_clean$text_clean)))

# Perform LDA
k <- 5 # Set the number of topics
lda_result <- LDA(dtm, k = k, control = list(seed = 123))

# Get the topic assignments for each document
data$Topic <- topics(lda_result)

# Get the top terms for each topic
top_terms <- tidy(lda_result, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# View the data with topic assignments and top terms
print(data)
print(top_terms)
