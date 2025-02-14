#### ---------------------------- Tekstanalyse: Tidy Text og Topic Modeling - Converting to and from non-tidy formats ------------------------------------- ####  
# Emne: Tekstanalyse med tidy text og emnemodellering  
# Dette script demonstrerer, hvordan man:  
#   1. Konverterer en dokument-term-matrix til et tidy-format for videre analyse  
#   2. Udfører sentimentanalyse ved at matche ord mod en sentiment-ordbog  
#   3. Visualiserer sentiment-fordelingen ved hjælp af søjlediagrammer  
#   4. Anvender TF-IDF til at identificere vigtige ord i et korpus  
#   5. Udfører topic modeling ved hjælp af Latent Dirichlet Allocation (LDA) for at identificere de vigtigste emner i teksten  
#   6. Visualiserer de vigtigste ord for hvert emne fundet i topic modeling  
#   7. Analyserer forfattere fra Gutenberg-projektet og bearbejder tekster til videre tekstanalyse  

# Libraries  
library(tm)            # Tekstmining og pre-processing  
library(dplyr)         # Data manipulation  
library(tidytext)      # Tidy text mining  
library(ggplot2)       # Visualisering  
library(quanteda)      # Korpus-analyse og feature-matrix  
library(topicmodels)   # Topic modeling (LDA)  
library(gutenbergr)    # Data fra Project Gutenberg  
library(stringr)       # Tekstmanipulation  
library(tidyr)        # Datatransformation  

# ----------------------------------------------------------- 5.1 Tidying a document-term matrix ------------------------------------------------------------ #
library(tm)
data("AssociatedPress", package = "topicmodels")
AssociatedPress
#
terms <- Terms(AssociatedPress)
head(terms)
#
library(dplyr)
library(tidytext)
ap_td <- tidy(AssociatedPress)
ap_td
#
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
ap_sentiments
#
library(ggplot2)
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col() +
  labs(x = "Contribution to sentiment", y = NULL)
#
library(quanteda)
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- data_corpus_inaugural %>%
  quanteda::tokens() %>%
  quanteda::dfm(verbose = FALSE)
inaug_dfm
#
inaug_td <- tidy(inaug_dfm)
inaug_td
#
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))
inaug_tf_idf
#
library(tidyr)
year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))
year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "% frequency of word in inaugural address")

# --------------------------------------------------------- 5.2 Casting tidy text data into a matrix -------------------------------------------------------- #
# -------------------------------------------------------------------- Find en forfatter -------------------------------------------------------------------- #
# Opgave fra klassen:
# Find en kvindelig forfatter fra gutenbergr:: # som har ca 20 bøger i sit forfatterskab

library(stm)
library(gutenbergr)
library(tm)
library(stringr)
# Load Gutenberg metadata
metagut = gutenberg_metadata
metagut
# Find forfatter med navnet Mary og sprog engelsk
auth = metagut %>% filter(str_detect(author, "Mary") & language == "en")
head(auth)
# Identificer Mary Wollstonecraft gennem hendes author id
marys = metagut %>% filter(gutenberg_author_id == 61 & language == "en")
# Vælg bøger af en anden Mary med author id 83 og vælg en sample på 4 bøger
marysbooks = metagut %>% filter(gutenberg_author_id == 83) %>% select(gutenberg_id)
maryssample = sample_n(marysbooks, 4)
maryssample$gutenberg_id
gid = unlist(maryssample$gutenberg_id)
# Download bøgerne
mjbooks = gutenberg_download(gid)
# Tokenize teksten og tæl ord
mjbooksct = mjbooks %>% unnest_tokens(word, text) %>% count(gutenberg_id, word)
# Definer stopord
dkstopord = stopwords("en")
dkstopord <- c(dkstopord, "Mary") # Tilføj egen stopord
# Rens mjbooksct for stopord, tal, tegnsætning, ord under 3 karakter og trim whitespaces
mjbooksct <- mjbooksct %>%
  filter(
    !word %in% dkstopord, 
    !str_detect(word, "^[0-9]+$"), 
    !str_detect(word, "[:punct:]"), 
    nchar(word) >= 3
  ) %>%
  mutate(word = str_trim(word))

# Konverter mjbooksct_cleaned til Document-Feature Matrix (DFM)
mjbdfm = mjbooksct %>% cast_dfm(gutenberg_id, word, n)
# Konverter mjbooksct_cleaned til Document-Term Matrix (DTM)
mjbdtm = mjbooksct %>% cast_dtm(gutenberg_id, word, n)
# Resultaterne af DFM og DTM
mjbdfm
mjbdtm
# Inspektion af DTM
inspect(mjbdtm)

library(topicmodels)
# Emne modellering (Topic modeling) ved brug af Latent Dirichlet Allocation (LDA)
topic_model = LDA(mjbdtm, k = 2, control = list(seed = 1234)) # 2 emner
# Konverter LDA resultaterne til et tidy format
tp_tidy = tidy(topic_model, matrix = "beta")
# Filtrer for ord med beta-værdier over 0.1 for at se de mest signifikante ord pr. emne
tp_tidy %>% filter(beta > 0.001)
# Plot de top ord pr. emne baseret på beta-værdier
tpplot = tp_tidy %>% group_by(topic) %>% top_n(10, beta)
# Plot top 10 ord pr. emne
tpplot

# Plot top 10 ord pr. emne
ggplot(tpplot, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 10 ord pr. emne baseret på beta-værdier", x = "Ord", y = "Beta")


# ------------------------------------------------------------ Quanteda - M. Frederiksen 1. maj tale -------------------------------------------------------- #
# Indlæs nødvendige pakker
library(quanteda)
library(topicmodels)
library(ggplot2)
library(dplyr)
library(quanteda.textplot)
library(quanteda.textstats)

# Indlæs RDS-filen med 1. maj taler
tale_1maj <- readRDS("1.maj.taler.rds")
# Opret en corpus fra data frame
corpus_tale <- corpus(tale_1maj, text_field = "value")
# Rens dataene: lav alt til små bogstaver, fjern tegnsætning, fjern stopord osv.
tokens_tale <- corpus_tale %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("da"))  # Fjern danske stopord

# Opret Document-Feature Matrix (DFM)
dfm_tale <- dfm(tokens_tale)
# Konverter DFM til en matrix som kan bruges i topicmodels
dtm_tale <- convert(dfm_tale, to = "topicmodels")

# Udfør emnemodellering (LDA) med f.eks. 2 emner
lda_tale <- LDA(dtm_tale, k = 2, control = list(seed = 1234))
# Konverter LDA resultaterne til et tidy format
lda_tidy <- tidy(lda_tale, matrix = "beta")

# Filtrer for de top 10 ord pr. emne
top_terms <- lda_tidy %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Plot top 10 ord pr. emne
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 10 ord pr. emne i Mette Frederiksens 1. maj-tale", x = "Ord", y = "Beta")

#
tale_1maj <- tale_1maj %>%
  mutate(speaker = str_replace_all(name, "https://www.dansketaler.dk/tale/|-1-maj-tale", ""))


