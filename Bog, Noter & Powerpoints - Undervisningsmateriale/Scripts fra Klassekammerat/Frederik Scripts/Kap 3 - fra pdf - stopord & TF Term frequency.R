# Load required libraries
library(dplyr)
library(tidytext)
library(tidyverse)
library(pdftools)

# Read the text of your book from a PDF file
pdf_text <- pdf_text("EU_lov_FAV.pdf")

# Tokenize the text into words
Extract_words <- tibble(text = pdf_text) %>%
  unnest_tokens(word, text)%>%
  count(word, sort = TRUE)

# Calculate the total number of words in the document
total_words <- sum(Extract_words$n)

# Read the lines of the text file
stopord_DK <- readLines("stopord.txt")

# Print the stopwords
print(stopord_DK)

# Filter out stopwords
Extract_words_filtered <- Extract_words %>%
  anti_join(data.frame(word = stopord_DK), by = "word")

# Calculate term frequency and rank
freq_by_rank <- Extract_words %>% 
  mutate(rank = row_number(), 
         term_frequency = n / total_words)

#Create plot showing frequency and rank
freq_by_rank %>% 
  ggplot(aes(rank, term_frequency)) + 
  geom_line(linewidth = 1.1, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Rank (log scale)", y = "Term Frequency (log scale)")

# Forklering af graf / figur
# Rank 1 er det ord som optræder flest gange i teksten. Term frequency er hvor meget det optræder reletivt 
# relativt til de andre ord i teksten

##### Efter stopword filtrering #####

# Calculate term frequency and rank
freq_by_rank_filtered <- Extract_words_filtered %>% 
  mutate(rank = row_number(), 
         term_frequency = n / total_words)

#Create plot showing frequency and rank
freq_by_rank_filtered %>% 
  ggplot(aes(rank, term_frequency)) + 
  geom_line(linewidth = 1.1, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Rank (log scale)", y = "Term Frequency (log scale)")



