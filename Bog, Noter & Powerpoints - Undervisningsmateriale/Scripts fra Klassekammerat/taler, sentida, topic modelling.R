library(tidyverse)
library(tidytext)
library(Sentida)
library(stopwords)
library(quanteda)
library(tm)
library(stm)


# Indlæser filer
alle_taler <- readRDS("27-05 Wulf/alletaler.rds")
pm <- readRDS("27-05 Wulf/pm.rds")

# Danner nye varible (year, speaker, folketing)
alle_taler$year <- str_extract(alle_taler$title, "\\d{4}")
alle_taler <- merge(alle_taler, pm, by = "year")
alle_taler <- alle_taler %>%
  mutate(folketing = ifelse(str_detect(title, "folketing|aabningstale-1|aabningstale-2"), 1, 0))

taler_folketing <- alle_taler %>% 
  filter(folketing == 1)

# Laver sentiment-score med sentida
alle_taler$sentiment_scoresentida <- NA

for (i in 1:nrow(alle_taler)) {
  score <- sentida(alle_taler$content[i], output = "mean")
  alle_taler$sentiment_scoresentida[i] <- score
}

# Udvælger 4 taler og laver topic modelling
taler <- as.data.frame(matrix(ncol = 2, nrow = 4))
colnames(taler) <- c("tale", "content")
taler$tale[1] <- alle_taler$title[40]
taler$tale[2] <- alle_taler$title[70]
taler$tale[3] <- alle_taler$title[100]
taler$tale[4] <- alle_taler$title[6]
taler$content[1] <- alle_taler$content[40]
taler$content[2] <- alle_taler$content[70]
taler$content[3] <- alle_taler$content[100]
taler$content[4] <- alle_taler$content[6]
taler$content <- gsub("Udskriv", "", taler$content)

# Tæller ord i hver tale
taler_count <- taler %>% 
  unnest_tokens(word, content) %>% 
  group_by(tale) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n))

# Fjerner stopord
stopord <- stopwords("da")
stopord <- tibble(word = stopord)
ekstra_stopord <- tibble(word = c("kan", "fik", "igen", "ved", "så", "få",
                                  "får","gør","gå","kun","uden","ingen","selvom", 
                                  "høre", "vide","står","kom","jeres","fået","nok",
                                  "bare","intet","før","gik","går","må","lige",
                                  "flere", "hver", "nye","ny","første","par", "a"))
stopord <- rbind(stopord, ekstra_stopord)
taler_count <- anti_join(taler_count, stopord, by = "word")

# Sentiment igen (anden liste)
sentiment_da <- read.csv("27-05 Wulf/2_headword_headword_polarity.csv", header = FALSE)
# Sepererer V6 kolonnen i flere rækker baseret på ;
sentiment_da <- sentiment_da %>%
  separate_rows(V6, sep = ";") %>%
  select(V1 = V6, V5)
sentiment_da$V1 <- gsub("'", "", sentiment_da$V1)
colnames(sentiment_da) <- c("word", "score")

# Laver ny sentiment score
taler_count_merged <- merge(taler_count, sentiment_da, by = "word")

# Sentiment-score for hver tale
average_score_per_tale <- taler_count_merged %>%
  group_by(tale) %>%
  summarise(total_weighted_score = sum(n * score),
            total_words = sum(n)) %>%
  mutate(average_score = total_weighted_score / total_words)

# Laver topic modelling
# Laver tf_idf
taler_top_10 <- taler_count %>%
  bind_tf_idf(word, tale, n) %>% 
  top_n(10, wt = tf_idf)

ggplot(data = taler_top_10, aes(x = tf_idf, y = fct_reorder(word, tf_idf), fill = tale)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(facets = ~tale, ncol = 2, scales = "free")

# Laver LDA
# Laver Document Frequency Matrix
taler_tm <- taler_count %>% 
  cast_dfm(tale, word, n)

# Laver model
topic_model <- stm(taler_tm, K = 4, init.type = "Spectral")
summary(topic_model)

td_beta <- tidy(topic_model)

# Finder top 10 ord af hver topic
td_beta <- td_beta %>% 
  group_by(topic) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(term = reorder(term, beta))

# Plotter
ggplot(data = td_beta, aes(x = beta, y = term, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free")

# Finder gamma
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(taler_tm))

# Laver til faktor
td_gamma$topic <- as.factor(td_gamma$topic)
td_gamma$document <- as.factor(td_gamma$document)

# Plotter gamma
ggplot(td_gamma, aes(gamma, fill = topic)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~topic, ncol = 3)

ggplot(data = td_gamma, aes(x = topic, y = document, fill = gamma)) +
  geom_tile() +
  scale_fill_gradient(low = "#ffffff", high = "#ff0000")

# Wordlcoud
df_count <- taler_count %>% 
  filter(tale == "aabningstale-1964")

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
