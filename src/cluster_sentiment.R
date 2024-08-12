# Load necessary libraries
library(dplyr)
library(tidytext)
library(tm)
library(SnowballC)
library(topicmodels)
library(tidyr)
library(textstem)
library(janitor)

# Read the CSV file and clean column names
data <- read.csv('data/libinsights-full21-24-dataframe.csv')
data <- clean_names(data)

# Define the column containing the text data
text_column <- 'response_3'

# Define custom stop words
c_stop_words <- c("jamie", "tim", "doug", "ibraheem", "leig", "zhiyuan", 
                  "gillian", "kristian", "ucla")

custom_stop_words <- data.frame(word = c_stop_words, stringsAsFactors = FALSE)
all_stop_words <- bind_rows(stop_words, custom_stop_words) %>%
  filter(word != "r")

# Preprocess the text data
data <- data %>%
  filter(!is.na(response_3)) %>%
  mutate(response_3 = str_replace_all(response_3, "\\|", " "),
         response_3 = str_replace_all(response_3, "[^a-zA-Z\\s]", " "),
         response_3 = tolower(response_3))

# Tokenize the text data into single words and bi-grams
single_words <- data %>%
  mutate(document_id = as.character(row_number())) %>%
  unnest_tokens(word, response_3) %>%
  filter(!word %in% all_stop_words$word)

bi_grams <- data %>%
  mutate(document_id = as.character(row_number())) %>%
  unnest_tokens(bigram, response_3, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% all_stop_words$word, !word2 %in% all_stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(!is.na(bigram))

combined_tokens <- bind_rows(
  single_words %>% mutate(bigram = word) %>% select(document_id, bigram),
  bi_grams
)

# Create term-document matrix
dtm <- combined_tokens %>%
  count(document_id, bigram) %>%
  cast_dtm(document_id, bigram, n)

# Apply K-means clustering
set.seed(123)
k <- 5  # Define the number of clusters
km_result <- kmeans(as.matrix(dtm), centers = k, nstart = 20)

# Ensure document IDs match between data and clustering results
data <- data %>%
  mutate(document_id = as.character(row_number()))

# Assign clusters to each document
data <- data %>%
  filter(document_id %in% names(km_result$cluster)) %>%
  mutate(cluster = km_result$cluster[document_id])

# Apply LDA topic modeling
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# Get the topics for each document
document_topics <- tidy(lda_model, matrix = "gamma")

# Extract the most likely topic for each document
document_topics <- document_topics %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  rename(document_id = document, topic = topic)

# Assign topics to each document
data <- data %>%
  left_join(document_topics, by = "document_id")

# Replace the "response_3" column with the cluster or topic labels
# For K-means
data <- data %>%
  mutate(response_3_clustered = paste(response_3, "Cluster", cluster))

# For LDA
data <- data %>%
  mutate(response_3_topic = paste(response_3, "Topic", topic))

# Save the updated dataframe
write.csv(data, "data/libinsights_with_topics_clusters.csv", row.names = FALSE)
