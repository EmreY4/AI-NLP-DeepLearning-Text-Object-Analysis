#### ------------------------------------------------ Tekstanalyse: Tokenizing af n-grammer i Jane Austens bøger ------------------------------------------ ####  
# Emne: Analyse af Jane Austens værker - Tokenisering af n-grammer og analyse af ordkombinationer  
# Dette script demonstrerer, hvordan man:  
#   1. Læsser og forbereder Jane Austens bøger fra Project Gutenberg  
#   2. Tokeniserer teksten til n-grammer (bigrammer og trigrammer)  
#   3. Filtrerer og optæller de mest almindelige n-grammer  
#   4. Analyserer n-grammer baseret på køn (fokus på "he" og "she")  
#   5. Beregner TF-IDF for bigrammer for at identificere vigtige ordkombinationer  
#   6. Undersøger forholdet mellem ord og negationer (f.eks. "not") ved hjælp af AFINN-ordbogen  
#   7. Visualiserer de mest hyppige n-grammer og deres relationer via grafteori og netværksanalyse  

# Libraries  
library(dplyr)      # Data manipulation  
library(tidytext)   # Tekstanalyse  
library(janeaustenr) # Jane Austen-tekster  
library(tidyr)      # Data transformation  
library(ggplot2)    # Datavisualisering  
library(igraph)     # Grafteori og netværksanalyse  
library(ggraph)     # Graftegning  
library(gutenbergr) # Adgang til Project Gutenberg  
library(stringr)    # String manipulation  
library(widyr)      # Beregning af parvise korrelationer

# --------------------------------------------------------------- 4.1 Tokenizing by n-gram ------------------------------------------------------------------ #
# He or She
# Tokenizing af tekster ved brug af n-grammer fra Jane Austens bøger
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) # Udpakning af n-grammer og fjernelse af manglende værdier
# Udskrivning af udtrukne n-grammer
austen_bigrams 
# Optælling af n-grammer og sorteret efter hyppighed
austen_bigrams %>%
  count(bigram, sort = TRUE) 

# Separering af n-grammer i to separate kolonner
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") 
# Filtrering af n-grammer med "he" og "she" som ord1, og fjernelse af stopord fra ord2
bigrams_filtered <- bigrams_separated %>%
  filter(word1 %in% c("he", "she", "He", "She")) %>%  
  filter(!word2 %in% stop_words$word) 
# Ny optælling af n-grammer
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
# Sammenlægning af ord1 og ord2 til ét n-gram
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") 

# Tokenizing af tekster ved brug af trigrammer
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Analyse af bigrammer
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)
# Beregning af TF-IDF for bigrammer
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

# Analyse af bigrammer baseret på køn
pnlist <- c("he", "she", "He", "She")
austen_bigramsGender <- bigrams_filtered %>% filter(word1 %in% pnlist)
austen_bigramsGenderVerbs <- austen_bigramsGender %>% group_by(word1, word2) %>% count(word2, sort = TRUE) %>% ungroup()
dfm <- austen_bigramsGenderVerbs %>% filter(word1 %in% c("he", "He"))
dff <- austen_bigramsGenderVerbs %>% filter(word1 %in% c("she", "She"))
commonw <- inner_join(dfm, dff, by = "word2")
commonw <- commonw %>% mutate(hebyher = (n.x / n.y) - 1)
commonw <- commonw %>% filter(n.x > 20) %>% mutate(gender = ifelse(hebyher > 0, "F", "M"))

# Plot af resultater baseret på køn
ggplot(commonw, aes(x = reorder(word2, hebyher), y = hebyher, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Common words by gender", x = "weight according to gender", y = "common word")

# Filtrering af bigrammer med "not" som ord1
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# Hent AFINN-ordbogen
AFINN <- get_sentiments("afinn")
# Print
AFINN

# Filtrering og optælling af ord med "not" som ord1 baseret på AFINN-ordbogen
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)
# Print
not_words
# Plot af sentimentværdi for ord efterfulgt af "not"
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

# Filtrering og optælling af ord efterfulgt af negationsord
negation_words <- c("not", "no", "never", "without")
# Filtrering
negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

# Oprindelige optællinger af bigrammer
bigram_counts
# Filtrering af kun relativt almindelige kombinationer og oprettelse af en graf
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
# Print
bigram_graph

# Plot af grafen
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Plot af grafen med justeringer
set.seed(2020)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Funktion til optælling af bigrammer
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}
# Funktion til visualisering af bigrammer
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

# King James-versionen er bog 10 på Project Gutenberg:
kjv <- gutenberg_download(10)
# Optælling af bigrammer for King James-versionen
kjv_bigrams <- kjv %>%
  count_bigrams()
# Filtrering af sjældne kombinationer og tal og visualisering af bigrammer
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()

# ----------------------------------------------- 4.2 Counting and correlating pairs of words with the widyr package ---------------------------------------- #
# 4.2.1 - Counting and correlating among sections
austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
# Print
austen_section_words

# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)
# Pring
word_pairs
# Filtrering
word_pairs %>%
  filter(item1 == "darcy")

#
# 4.2.2 - Pairwise correlation
# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)
# Print
word_cors
# Filtrering
word_cors %>%
  filter(item1 == "pounds")

# Plot
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# Plot
set.seed(2016)
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
