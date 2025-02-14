library(tidytext)
library(janeaustenr)
library(tidyverse)
library(igraph)
library(ggraph)
library(spacyr)

# Indhenter engelsk spacy sprogmodel
spacy_initialize(model = "en_core_web_sm")
#spacy_initialize(model = "da_core_web_sm")

# Deler i 2 ord - ændrer til 3 for trigrams
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# Ser liste af mest almindelige to-ords kombinationer
austen_bigrams %>%
  count(bigram, sort = TRUE)

# Sepererer ord
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

gender_words <- c("he", "she")

# Filter out stop words, but retain "he" and "she"
bigrams_filtered <- bigrams_separated %>%
  filter(!(word1 %in% stop_words$word & !word1 %in% gender_words)) %>%
  filter(!(word2 %in% stop_words$word & !word2 %in% gender_words))

# Indhenter affin for sentiment
afinn <- get_sentiments("afinn")

# new bigram counts: med he og she
bigram_counts_hf <- bigrams_filtered %>%
  filter(word1 %in% gender_words) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)


# Ny liste af mest almindelige to-ords kombinationer
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# Forbinder de to ord-kolonner igen 
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")


# ti-idf - Term Frequency-Inverse Document Frequency 
# importance of a term in a document relative to a collection of documents (corpus).
# term Frequency (TF) measures how frequently a term appears in a document
# inverse Document Frequency (IDF) measures how important a term is across the entire corpus
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

# Plotter top 10 ord fra hver bog
top_10_df <- bigram_tf_idf %>% 
  group_by(book) %>% 
  top_n(n = 10, wt = tf_idf)

ggplot(data = top_10_df, aes(x = tf_idf, y = fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(facets = ~book, ncol = 2, scales = "free")

# Finder top ord som følger "not"
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# Henter sentiment liste
afinn <- get_sentiments("afinn")

# Mest almindelige ord som følger "not" sammen med sentiment score
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

# Top ord som forekommer mest i den forkerte retning
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

# Laver det samme på flere ord
# Finder ord som har omvendt betydning
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  group_by(word1) %>%  
  count(word1, word2, value, sort = TRUE) %>% 
  slice(1:10) %>% 
  ungroup()

negated_words$contribution <- negated_words$n * negated_words$value

ggplot(data = negated_words, aes(contribution, fct_reorder(word2, contribution), fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"") +
  facet_wrap(facets = ~word1, ncol = 2, scales = "free")

## Visuallising relationships
# Finder mest forekommende bigrams
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

# Relationship plot 1 
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Relationship plot 2
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Laver funktioner til at lave visualiseringer af bigrams 
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# Spacy
austen_text <- austen_books() %>% 
  filter(book == "Sense & Sensibility")
austen_text <- austen_text %>% 
  mutate(doc_id = row_number()) %>% 
  select(-book)

# Kører spacy
JASS=spacy_parse(austen_text)

# Bruger space til kun at finde adjektiver tillægsord
JASS_verbs=JASS %>% 
  filter(pos=="ADJ") %>% 
  select(token) %>% 
  unique() %>% 
  rename('w2'='token')


austeks <- austen_books() %>% 
  group_by(book) %>% 
  filter(str_detect(book,"Sense")) %>% 
  ungroup()

sense_sentences <- austeks %>% 
  unnest_tokens(sentence, text, token = "sentences")

sense_sentences['doc_id']=1:nrow(sense_sentences)

sense_sentences <- sense_sentences %>% 
  rename('text'='sentence')

ausPOSparse <- spacy_parse(sense_sentences, pos = TRUE, entity = TRUE, dependency = TRUE)

# Filtrerer på navneord
ausPOSEx = ausPOSparse %>%
  filter(str_detect(entity,"PERSON") |  pos=="VERB")

ausPOSExP = ausPOSEx %>% filter(str_detect(entity,"PERSON")) %>% group_by(doc_id) %>%
  summarise(pc=n()) %>% ungroup()
  
## forstår ingenting ^^ ??

