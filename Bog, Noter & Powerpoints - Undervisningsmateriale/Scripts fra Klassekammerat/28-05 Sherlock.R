# Libraries
library(tidyverse)
library(gutenbergr)
library(stringr)
library(tidytext)
library(tm)
library(stm)
library(topicmodels)
library(stopwords)
library(wordcloud)
library(ggthemes)
library(igraph)
library(ggraph)

# Indlæser data
sherlock <- as.data.frame(readLines("/Users/marius/Documents/DataAnalyseProjekter/NLP & Deep-Learning/NLP & Deep-Leaning R/28-05_sherlock/pg1661.txt"))

# Transformerer data
# Ændrer kolonnenavn
colnames(sherlock) <- "text"

# Laver ny kolonne for length
sherlock <- sherlock %>% 
  mutate(length = nchar(text))

# Laver ny kolonne for story
sherlock <- sherlock %>% 
  mutate(story = str_extract(text, "^[IXV\\.]+ [A-Z\\. ]+$")) %>% 
  fill(story)

# Fjerner na og beholder alle ord med over 60 tegn
sherlock <- sherlock %>% 
  filter(!is.na(story)) %>% 
  filter(length > 60)

# Linjenummer
sherlock <- sherlock %>% 
  mutate(idx = row_number())

# Lille plot
hist(sherlock$length, breaks = 30)

# To tidy
sherlock_tibble <- sherlock %>% 
  unnest_tokens(word, text)

# Indlæser sentiment på engelsk
afinn <- as.data.frame(get_sentiments("afinn"))
bing <- as.data.frame(get_sentiments("bing"))
nrc <- as.data.frame(get_sentiments("nrc"))

# Sentimentscore
sherlock_score <- inner_join(sherlock_tibble, afinn, by = "word")
sherlock_score_bing <- inner_join(sherlock_tibble, bing, by = "word")

# afinn 
sherlock_score2 <- sherlock_score %>% 
  group_by(story, idx) %>% 
  mutate(ts = sum(value)) %>% 
  select(-word, value) %>% 
  unique()

# bing
sherlock_score2_bing <- sherlock_score_bing %>% 
  mutate(sentiment_score = ifelse(sentiment == "positive", 1, ifelse(sentiment == "negative", -1, 0))) %>%
  group_by(story, idx) %>% 
  summarise(ts_bing = sum(sentiment_score)) %>%
  ungroup()

# Plot 

# Husk at ændre y-variablen til en anden afhængig om man bruger afinn eller bing :)
ggplot(sherlock_score2, aes(x = idx, y = ts)) +
  geom_line(alpha = 0.2) + 
  geom_smooth(method = "loess", span = 0.05) +
  facet_wrap(~story, scales = "free")

# Udvælger 10 historier laver topic modelling
sherlock_texts <- sherlock_score %>%
  group_by(story) %>%
  summarize(score = sum(value), text = paste(word, collapse = " ")) 

# Udtrækker den unikke sentimentscore for hver historie
scoresum <- sherlock_texts$score

# Count words within each story
sherlock_count <- sherlock_texts %>%
  unnest_tokens(word, text) %>%
  group_by(story) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))

# Aggrergerer data 
df <- sherlock_score %>% 
  group_by(story) %>% 
  mutate(scoresum = sum(value)) %>% 
  select(story, scoresum) %>% 
  unique()

# Fjerner stopord
stopord <- c(stop_words$word, "matter", "good", "miss")
stopord <- tibble(word = stopord)
sherlock_count <- anti_join(sherlock_count, stopord, by = "word")

# Fjerner alt som ikke er ord
sherlock_count <- sherlock_count[grepl("^[A-Za-z]+$", sherlock_count$word), ]

# Laver tf_idf
sherlock_count2 <- sherlock_count %>%
  bind_tf_idf(word, story, n)

# Visualiserer tf_idf
top_10_df <- sherlock_count2 %>% 
  group_by(story) %>% 
  top_n(n = 10, wt = tf_idf)

ggplot(data = top_10_df, aes(x = tf_idf, y = fct_reorder(word, tf_idf), fill = story)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(facets = ~story, ncol = 2, scales = "free")

# DFM og LDA
# Laver DFM
sherlock_tm <- sherlock_count %>% 
  cast_dfm(story, word, n)

topic_model <- stm(sherlock_tm, K = 10, init.type = "Spectral")
summary(topic_model)

td_beta <- tidy(topic_model)

td_beta <- td_beta %>% 
  group_by(topic) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(term = reorder(term, beta))

ggplot(data = td_beta, aes(x = beta, y = term, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free")


# Laver LDA
story_dtm <- sherlock_count %>% 
  cast_dtm(story, word, n)

apLDA <- LDA(story_dtm, k = 10, control = list(seed = 1234))

ap_topics <- tidy(apLDA, matrix = "beta")

ap_topics <- ap_topics %>%
  mutate(term = str_remove_all(term, "[^[:alpha:]]")) %>%
  filter(term != "")

# test på topic 1
ap1 <- ap_topics %>%
  filter(topic == 1) %>%
  arrange(desc(beta))

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Finder gamma
td_gamma <- tidy(apLDA, matrix = "gamma",
                 document_names = rownames(story_dtm))

# Laver til faktor
td_gamma$topic <- as.factor(td_gamma$topic)
td_gamma$document <- as.factor(td_gamma$document)

# Plotter gamma
ggplot(td_gamma, aes(gamma, fill = topic)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~topic, ncol = 3)

# Plot TM
ggplot(data = td_gamma, aes(x = topic, y = document, fill = gamma)) +
  geom_tile() +
  scale_fill_gradient(low = "#ffffff", high = "#ff0000")

# Plot 2 TM
td_gamma2 <- td_gamma
td_gamma$topic <- as.integer(td_gamma$topic)
top_gamma2 <- left_join(td_gamma, ap_top_terms, by = "topic")
td_gamma$topic <- as.factor(td_gamma$topic)

ggplot(data = top_gamma2, aes(x = topic, y = factor(document), fill = gamma)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0.5) +
  geom_text(aes(label = term), size = 3, position = position_jitter(width = 0.4, height = 0.4))

ggplot(head(ap1,10), aes(beta, term, fill = topic)) + 
  geom_col() +
  labs(x = "\nSandsynlighed for gentagende ord", y = "Ord",
       title = "Sprogbruget i tekstens opgaver vises at være ensartet",
       subtitle = "Analyse af tekstmæssig ensartethed i eksamensopgaven",
       caption = "\nData: AssociatedPress") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        text = element_text(family = "verdana"),
        legend.position = "none")

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

top_terms <- beta_wide %>% arrange(desc(log_ratio)) %>% head(10)
bottom_terms <- beta_wide %>% arrange(log_ratio) %>% head(10)
plot_data <- bind_rows(top_terms, bottom_terms)

ggplot(plot_data, aes(x = reorder(term, log_ratio), y = log_ratio)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Terms",
    y = "Log2 ratio of beta in topic 2 / topic 1",
    title = "Top and Bottom Terms by Log2 Ratio"
  ) +
  theme_minimal()

# Wordlcoud
df_count <- sherlock_count %>% 
  filter(story == "I. A SCANDAL IN BOHEMIA")

png("test.png", width = 2000, height = 2000, res = 300)
wordcloud(words = df_count$word,
          freq = df_count$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"),
          scale=c(2,0.25))
dev.off()

# Bigrams
sherlock_bigram <- sherlock %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  na.omit() %>% 
  filter(!grepl("\\d", bigram))

# Data feature engineering
sherlock_bigram <- sherlock_bigram %>% 
  separate(bigram, c("w1","w2"), sep = " ")

# Renser for stopord (Hvis man fjerner stopord, vil der mangle data til at kunne sammenlinge mænd og kvinder)
#hcabigram <- hcabigram %>% 
#filter(!w1 %in% stop_words$word & !w2 %in% stop_words$word)

# Finder hvor der nævnes he/his & she/hers
sherlock_bigramSubM <- sherlock_bigram %>% 
  filter(w1 %in% c("he","his"))

sherlock_bigramSubF <- sherlock_bigram %>% 
  filter(w1 %in% c("she","hers"))

# Count af bigrams
sherlock_bigramSubM <- sherlock_bigramSubM %>% 
  count(w1, w2, sort = TRUE)

sherlock_bigramSubF <- sherlock_bigramSubF %>% 
  count(w1, w2, sort = TRUE)

# Plot (EDA) Male
hist(sherlock_bigramSubM$n)

# Plot (EDA) Female
hist(sherlock_bigramSubF$n)

# Transformation
combinedhca <- inner_join(sherlock_bigramSubM, sherlock_bigramSubF , by = "w2")

# FE - Gender ratio, Gender Column
combinedhca <- combinedhca %>% 
  mutate(mfratio = round(n.y/n.x,2), 
         gender = ifelse(mfratio > 1, "F", "M")) %>% 
  arrange(mfratio)

combinedhca <- combinedhca %>% 
  mutate(total = n.y + n.x)

combinedhca_top <- combinedhca %>% 
  slice_head(n = 10)

combinedhca_bot <- combinedhca %>% 
  slice_tail(n = 10)

combined <- bind_rows(combinedhca_top, combinedhca_bot)

# Plot Gender
ggplot(combined, aes(x = reorder(w2, mfratio), y = mfratio, fill = gender)) + 
  geom_col() +
  labs(title = "Kvindekønnet benytter sig oftere af de samme ord \nsammenlignet med mænd",
       subtitle = "Benyttelsen af udsagnsord efter køn",
       x = "",
       y = "Køn-Ratio") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        text = element_text(family = "verdana"),
        legend.title = element_blank()) +
  coord_flip() 
