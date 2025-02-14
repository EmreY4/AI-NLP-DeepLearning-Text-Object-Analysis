# Load necessary libraries
library(dplyr)
library(tidytext)
library(janeaustenr)
library(ggplot2)
library(ggraph)
library(tidyr)
library(forcats)

# Create a data frame of bigrams (pairs of consecutive words) from Austen's books
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# View the bigrams data frame
austen_bigrams

# Count the frequency of each bigram and sort by frequency
bigram_counts <- austen_bigrams %>%
  count(bigram, sort = TRUE)

# Print the bigram counts
bigram_counts

# Separate the bigrams into two columns: word1 and word2
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Remove stop words from both columns
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Count the filtered bigrams
bigram_counts_filtered <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Print the filtered bigram counts
bigram_counts_filtered

# Unite the filtered bigrams back into a single column
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Create and count trigrams (triplets of consecutive words)
austen_trigrams <- austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Print the trigram counts
austen_trigrams

# Find the most frequent bigrams where the second word is "street"
bigrams_with_street <- bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# Print the bigrams with "street"
bigrams_with_street

# Calculate tf-idf for bigrams to find those most important to each book
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

# Print the tf-idf weighted bigrams
bigram_tf_idf

# Visualize the top 10 tf-idf bigrams for each book
bigram_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL, title = "Top 15 TF-IDF Bigrams in Jane Austen's Books")
