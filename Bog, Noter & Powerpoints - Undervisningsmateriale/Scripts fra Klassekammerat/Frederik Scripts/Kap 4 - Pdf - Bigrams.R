# Load necessary libraries
library(dplyr)
library(tidytext)
library(tidyr)
library(pdftools)

# Read the text of your book from a PDF file
pdf_text <- pdf_text("EU_lov_FAV.pdf")

# Combine all pages into a single text string
pdf_text_combined <- paste(pdf_text, collapse = "\n")

# Convert the combined text into a data frame with a single column named 'text'
Pdf1 <- data.frame(text = pdf_text_combined, stringsAsFactors = FALSE)

# Tokenize the text into words and remove stop words
tidy_pdf <- Pdf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Create bigrams
tidy_bigrams <- Pdf1 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ")

# View the first few bigrams
head(tidy_bigrams)

# Optionally, you might want to filter out bigrams with stop words
bigrams_separated <- tidy_bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

# Remove stop words from bigrams
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Reunite the words to get the final list of meaningful bigrams
tidy_bigrams_final <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# View the first few bigrams
head(tidy_bigrams_final)

#################################################

# Count bigrams
bigram_counts <- tidy_bigrams_final %>%
  count(gutenberg_id = 35, bigram, sort = TRUE)

bigram_counts

# Calculate tf-idf for each bigram
bigram_tf_idf <- bigram_counts %>%
  bind_tf_idf(bigram, gutenberg_id, n) %>%
  arrange(desc(tf_idf))


# View the first few tf-idf results
head(bigram_tf_idf)

# Start Trigrams
trigram_counts <- hgwells %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# View the first few trigrams
head(trigram_counts)

# Most frequent bigrams where "street" is the second word
bigrams_with_street <- bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(gutenberg_id = 35, word1, sort = TRUE)

# View the first few bigrams with "street"
head(bigrams_with_street)
