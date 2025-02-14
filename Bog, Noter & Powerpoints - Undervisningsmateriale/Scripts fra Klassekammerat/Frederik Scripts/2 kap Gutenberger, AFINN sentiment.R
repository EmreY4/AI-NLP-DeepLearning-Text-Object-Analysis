# Load necessary libraries
library(dplyr)
library(tidytext)
library(tidyr)
library(gutenbergr)
library(ggplot2)
library(stringr)
library(pdftools)

# Load sentiment lexicon
afinn <- get_sentiments("afinn")

# Download texts of H.G. Wells
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# Tokenize the text into words and remove stop words
tidy_books <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


# Perform sentiment analysis using the AFINN sentiment lexicon
afinn_sentiment <- tidy_books %>%
  inner_join(afinn, by = "word") %>%
  group_by(gutenberg_id = 35) %>%
  mutate(method = "AFINN")

afinn_sentiment %>% 
  summarize(sentiment = sum(value))
  
  
# Plot the sentiment over the course of the text
ggplot(afinn_sentiment, aes(index, sentiment, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free_x") +
  labs(title = "AFINN Sentiment Scores for H.G. Wells' Texts",
       x = "Index",
       y = "Sentiment Score")
