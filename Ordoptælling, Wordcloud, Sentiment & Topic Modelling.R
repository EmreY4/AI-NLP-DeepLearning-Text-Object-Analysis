#### ----------------------------------------------- Ordoptælling, Wordcloud, Sentiment & Topic Modelling ------------------------------------------------- ####  
# Emne: Analyse af tekst fra PDF og sentimentanalyse med ordtælling og topic modelling  
# Dette script demonstrerer, hvordan man:  
#   1. Læser tekst fra en PDF-fil og konverterer den til et samlet tekstformat  
#   2. Tokeniserer teksten til individuelle ord  
#   3. Fjerner stopord, tal, tegnsætning og kortere ord  
#   4. Udfører sentimentanalyse ved hjælp af "bing"-lexikonet for at beregne positive og negative ord  
#   5. Visualiserer sentimentanalysen ved hjælp af barplots  
#   6. Skaber en ordsky baseret på hyppigheden af ord i teksten  
#   7. Udfører topic modeling med Latent Dirichlet Allocation (LDA) for at identificere de vigtigste emner i teksten  
#   8. Visualiserer de vigtigste ord for hvert emne fundet i topic modeling  

# Libraries  
library(pdftools)      # For extracting text from PDF
library(dplyr)         # For data manipulation
library(tidytext)      # For text mining
library(tidyr)         # For data tidying
library(tm)            # For text mining and preprocessing
library(stringr)       # For string manipulation
library(ggplot2)       # For data visualization
library(wordcloud)     # For creating word clouds
library(topicmodels)   # For topic modeling

# ------------------------------------------------------------- Data Preparation - Stop Words --------------------------------------------------------------- #
# Load PDF document
nvidia_pdf_text <- pdf_text("Nvidia Commentary.pdf")
# Convert PDF text to a single character vector
nvidia_text_combined <- paste(nvidia_pdf_text, collapse = " ")
# Create a data frame with the text
nvidia_text_df <- data.frame(text = nvidia_text_combined, stringsAsFactors = FALSE)

# Tokenize the text into individual words
nvidia_word_tokens <- nvidia_text_df %>%
  unnest_tokens(word, text)

# Define stop words using built-in data
data("stop_words")
# Add custom stop words
additional_stop_words <- c("gaap", "nvidia")
custom_stop_words <- bind_rows(stop_words, data_frame(word = additional_stop_words))
#custom_stop_words <- bind_rows(stop_words, tibble(word = c("gaap", "nvidia")))
# Remove stop words, numbers, punctuation, and short words
nvidia_cleaned_tokens <- nvidia_word_tokens %>%
  anti_join(custom_stop_words) %>% # Remove common stop words and additional stop words
  filter(
    !str_detect(word, "^[0-9]+$"), # Remove numbers
    !str_detect(word, "[:punct:]"), # Remove punctuation
    nchar(word) >= 3                # Keep words with 3 or more characters
  )

# -------------------------------------------------------------------- Sentiment Analysis ------------------------------------------------------------------- #
# Perform sentiment analysis using the "bing" lexicon
nvidia_bing_sentiment <- nvidia_cleaned_tokens %>%
  inner_join(get_sentiments("bing")) %>% # Join with bing sentiment lexicon
  count(word, sentiment, sort = TRUE)    # Count occurrences of each sentiment word
# Calculate overall sentiment by subtracting negative counts from positive counts
nvidia_overall_sentiment <- nvidia_bing_sentiment %>%
  count(sentiment) %>%                         # Count number of positive and negative words
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(sentiment = positive - negative)      # Calculate overall sentiment score
# Print overall sentiment
print(nvidia_overall_sentiment)
# Visualize the sentiment analysis results
nvidia_bing_sentiment %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%             # Select top 10 words for each sentiment
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%  # Reorder words by count
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +      # Create bar plot
  facet_wrap(~sentiment, scales = "free_y") + # Separate plots for positive and negative sentiments
  labs(title = "Sentiment Analysis of 'Nvidia Commentary'",
       y = "Contribution to Sentiment",
       x = "Words") +
  coord_flip()                         # Flip coordinates for better readability

# ---------------------------------------------------------------------- Word Cloud ------------------------------------------------------------------------- #
# Tokenize the text and count words for word cloud
nvidia_word_counts <- nvidia_text_df %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)  # Count occurrences of each word
# Remove stop words, numbers, punctuation, and short words
nvidia_cleaned_word_counts <- nvidia_word_counts %>%
  anti_join(custom_stop_words) %>%
  filter(
    !str_detect(word, "^[0-9]+$"), # Remove numbers
    !str_detect(word, "[:punct:]"), # Remove punctuation
    nchar(word) >= 3                # Keep words with 3 or more characters
  )
# Display cleaned word counts dataframe
print(nvidia_cleaned_word_counts)
# Create word cloud
# Create word cloud with adjusted layout settings
set.seed(1234)
wordcloud(words = nvidia_cleaned_word_counts$word, 
          freq = nvidia_cleaned_word_counts$n, 
          min.freq = 1, 
          max.words = 200, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))
# Plot the top words using ggplot2
nvidia_top_words <- nvidia_cleaned_word_counts %>%
  top_n(20, n)  # Select top 20 words

ggplot(nvidia_top_words, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 20 Words in 'Nvidia Commentary'", 
       x = "Words", y = "Count")

# -------------------------------------------------------------------- Topic Modeling ----------------------------------------------------------------------- #
# Topic modeling
# Create a Document-Term Matrix (DTM)
nvidia_dtm <- nvidia_cleaned_tokens %>%
  count(document = nvidia_text_df$text, word) %>%  # Count word occurrences in each document
  cast_dtm(document, word, n)               # Cast to Document-Term Matrix
# Perform LDA (Latent Dirichlet Allocation) for topic modeling
nvidia_lda_model <- LDA(nvidia_dtm, k = 2, control = list(seed = 1234)) # Adjust k for number of topics
# Tidy the LDA model results
nvidia_lda_topics <- tidy(nvidia_lda_model, matrix = "beta")  # Extract topic-term probabilities
# Find the top terms in each topic
nvidia_top_terms <- nvidia_lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%     # Select top 10 terms for each topic
  ungroup() %>%
  arrange(topic, -beta)   # Arrange terms by topic and beta value
# Plot the top terms in each topic
ggplot(nvidia_top_terms %>% 
         mutate(topic = paste("Topic", topic)), # Add descriptive titles to each topic
       aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") + # Separate plots for each topic
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms in Each Topic",
       x = "Term", y = "Beta (Probability)",
       caption = "Top terms within each topic discovered by LDA") +
  theme(strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

