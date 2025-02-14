library(dplyr)
library(tidytext)
library(janeaustenr)


# Data frame med bigrams dvs. 2 sammenhængende ord
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

austen_bigrams

# Count bigrams
austen_bigrams %>%
  count(bigram, sort = TRUE)

library(tidyr)

# laver 2 kolonner af et bigrams
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtrere stopwords fra
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

#####

# samler bigrams fra de 2 kolonner i 1 samlet kolonne
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

### Start Trigrams
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

#### end of trigrams

# De mest hyppige bigrams hvor ordet "street" indgår som word nr. 2. 
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# Tf_idf vægtet (dvs. ord der væsentlige for bogen)
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# Female - Hvilke bigrams er de mest hyppige når "She" er word1?
F <- bigrams_separated %>%
  filter(word1 == "she") %>%
  count(word1, word2, sort = TRUE)


# Male - Hvilke bigrams er de mest hyppige når "He" er word1?
M <- bigrams_separated %>%
  filter(word1 == "he") %>%
  count(word1, word2, sort = TRUE)

# Load sentiment score
AFINN <- get_sentiments("afinn")



# Filtrer word1 til "not" og find de mest hyppige
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words

library(ggplot2)

# Lav en figur der visualisere det
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

# Lav data frame med ord som skal fjernes
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

library(igraph)

# original counts
bigram_counts


# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

# Visualisere
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Visualisere markov chain
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

