# text_analysis.R

# Load necessary libraries
library(dplyr)
library(tidytext)
library(stringr)
library(readr)

# Load the cleaned and merged data
dsc_consult <- readRDS("data/dsc_consult_merged.rds")

# Load the Trello data
trello_data <- read_csv("data/ucla-datasquad-projects-trello.csv")
trello_data <- clean_names(trello_data)
trello_data$appointment_reason <- paste(trello_data$card_name, trello_data$card_description, sep = " ")

# Define custom stop words
## todo, add a bunch more that is in the qmd 
custom_stop_words <- data.frame(word = c("ucla"), stringsAsFactors = FALSE)

# Combine with existing stop words from tidytext and remove "r"
all_stop_words <- bind_rows(stop_words, custom_stop_words) %>%
  filter(word != "r")

# Preprocess the text data
dsc_consult <- dsc_consult %>%
  filter(!is.na(appointment_reason)) %>%
  mutate(appointment_reason = str_replace_all(appointment_reason, "\\|", " "),
         appointment_reason = str_replace_all(appointment_reason, "[^a-zA-Z\\s]", " "),
         appointment_reason = tolower(appointment_reason))

# Tokenize the text data into single words and bi-grams while preserving the document identifier
single_words <- dsc_consult %>%
  mutate(document_id = as.character(row_number())) %>%
  unnest_tokens(word, appointment_reason) %>%
  filter(!word %in% all_stop_words$word)

bi_grams <- dsc_consult %>%
  mutate(document_id = as.character(row_number())) %>%
  unnest_tokens(bigram, appointment_reason, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% all_stop_words$word, !word2 %in% all_stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(!is.na(bigram))

# Combine single words and bi-grams
combined_tokens <- bind_rows(
  single_words %>% mutate(bigram = word) %>% select(document_id, bigram),
  bi_grams
)
# Preprocess the text data
trello_data <- trello_data %>%
  filter(!is.na(appointment_reason)) %>%
  mutate(appointment_reason = str_replace_all(appointment_reason, "\\|", " "),
         appointment_reason = str_replace_all(appointment_reason, "[^a-zA-Z\\s]", " "),
         appointment_reason = tolower(appointment_reason))

# Tokenize the text data into single words and bi-grams while preserving the document identifier
single_words <- trello_data %>%
  mutate(document_id = as.character(row_number())) %>%
  unnest_tokens(word, appointment_reason) %>%
  filter(!word %in% all_stop_words$word)

bi_grams <- trello_data %>%
  mutate(document_id = as.character(row_number())) %>%
  unnest_tokens(bigram, appointment_reason, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% all_stop_words$word, !word2 %in% all_stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(!is.na(bigram))

# Combine single words and bi-grams
combined_tokens <- bind_rows(
  single_words %>% mutate(bigram = word) %>% select(document_id, bigram),
  bi_grams
)
# Save combined tokens for use in R Markdown
saveRDS(combined_tokens, "data/trello_combined_tokens.rds")

# Save combined tokens for use in R Markdown
saveRDS(combined_tokens, "data/combined_tokens.rds")
