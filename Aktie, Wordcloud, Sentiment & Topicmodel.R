#### ---------------------------------------------------- Aktiekurs og Sentimentanalyse - Mærsk 2020-2022 ------------------------------------------------- ####  
# Emne: Analyse af Mærsk aktiekurser og sentiment i årsrapporter for perioden 2019-2022  
# Dette script demonstrerer, hvordan man:  
#   1. Henter aktiekurser for Mærsk A/S fra Yahoo Finance  
#   2. Visualiserer aktiekursudviklingen i 2024  
#   3. Analyserer indholdet af Mærsk årsrapporter fra 2019-2022 ved hjælp af tekstdata  
#   4. Skaber en ordtabel og visualiserer hyppige ord i årsrapporterne med en wordcloud  
#   5. Beregner sentiment-score for de forskellige årsrapporter baseret på ordets sentiment  
#   6. Udfører en topicmodel for at identificere de mest relevante emner i årsrapporter  
#   7. Udfører en logratio-analyse for at sammenligne emner mellem årsrapporter og analysere deres relative betydning  

# Libraries  
library(quantmod)      # Aktiekurs data  
library(tidyquant)     # Aktiekurs data manipulation  
library(ggplot2)       # Visualisering af data  
library(textdata)      # Sentiment analyse ordlister  
library(tidytext)      # Tekstbehandling  
library(dplyr)         # Data manipulation  
library(pdftools)      # Læsning af PDF-filer  
library(stringr)       # Tekst manipulation  
library(wordcloud)     # Wordcloud visualisering  
library(tm)            # Text mining  
library(topicmodels)   # Topic modelling  

# ------------------------------------------------------------------------ Aktiekurs ------------------------------------------------------------------------ # 
# Hvordan hiver man aktiekurserne ud - 
#  - yahoofinance
#getDividends 
#Nordea, Danske Bank, Jyske Bank. Kig på deres hjemmesider.

# Ticker-symbol for A.P. Møller - Mærsk A/S
maersk_ticker <- "MAERSK-B.CO"
# Henter aktiekurserne for Mærsk siden starten af 2020
aktiekurser_maersk <- tq_get(maersk_ticker, from = "2020-01-01")
# Udskriver de seneste aktiekurser for Mærsk
print(tail(aktiekurser_maersk))

# Plot - Af aktiekurserne for Mærsk i 2024
ggplot(aktiekurser_maersk, aes(x = date, y = close)) +
  geom_line(color = "blue") +
  labs(title = "Udvikling af Mærsk Aktiekurs",
       x = "Dato",
       y = "Aktiekurs (Dansk krone)") +
  theme_minimal()

# -------------------------------------------------------------- Ordtabel, Wordcloud, Sentiment ------------------------------------------------------------- # 
library(textdata)
library(tidytext)
library(dplyr)
library(pdftools)
library(dplyr)
library(stringr)
library(wordcloud)

# Læs indholdet af din PDF-fil Hovedopgave.pdf 
# Læs indholdet af PDF-filer, bestemte sider
text1 <- pdf_text("2019MAERSK.pdf")[6:9]
text2 <- pdf_text("2020MAERSK.pdf")[7:10]
text3 <- pdf_text("2021MAERSK.pdf")[7:9]
text4 <- pdf_text("2022MAERSK.pdf")[5:7]

# Beregn antallet af ord i teksten ved at tælle antallet af ikke-blanktegn (\\S+)
antal_ord <- sum(str_count(text1, "\\S+"))
# Opdel teksten i ord ved hjælp af mellemrum og gem det som en vektor
ord <- unlist(str_split(maersk_aarsrapport, "\\s+"))
# Omdan vektoren af ord til en data frame
ord_df <- data.frame(ord)
# Opret en tabel over ord og deres forekomster ved at gruppere efter ord og tælle forekomsterne
ord_tabel <- ord_df %>%
  group_by(ord) %>%
  summarise(count = n()) %>%
  # Sorter tabellen i faldende rækkefølge efter antallet af forekomster af hvert ord
  arrange(desc(count))

#få stopord ind
stopord <- readLines("stopord.txt")

# Antag at ord_tabel allerede er defineret som tidligere beskrevet
filtreret_ordtabel <- ord_tabel %>%
  filter(!ord %in% stopord) %>%
  arrange(desc(count))
# Vis den filtrerede ordtabel
print(filtreret_ordtabel)

# Fjerner ekstra ord - Definer ekstra stopord
ekstra_stopord <- c("-", "I", " ", "1", "2", "4", "31", ".", "/", "the", "The", "and", "•", "with", "of", "in", "on", "is", "a", "as", "by", "are")
# Kombiner eksisterende stopord med de nye
stopord <- unique(c(stopord, ekstra_stopord))
# Brug den opdaterede liste til at filtrere din ordtabel
filtreret_ordtabel <- ord_tabel %>%
  filter(!ord %in% stopord) %>%
  arrange(desc(count))
# Vis den filtrerede ordtabel
print(filtreret_ordtabel)

# ------------------------------------------------------------------------ Wordcloud ------------------------------------------------------------------------ # 
# Kombiner teksterne til en enkelt streng
combined_text <- paste(text1, text2, text3, text4, collapse = " ")

# Opret et Corpus objekt
corpus <- Corpus(VectorSource(combined_text))
# Forbehandle teksten
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Opret en frekvenstabel for ordene
word_freq <- TermDocumentMatrix(corpus)
word_freq <- as.matrix(word_freq)
word_freq <- sort(rowSums(word_freq), decreasing = TRUE)
# Opret wordcloud
wordcloud(words = names(word_freq), freq = word_freq, min.freq = 1, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

# -------------------------------------------------------- Sentiment score - Positive & Negative ladet ord -------------------------------------------------- #
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")
# Hent sentiment-ordlisterne
afinn_sentiments <- get_sentiments("afinn")
bing_sentiments <- get_sentiments("bing")
nrc_sentiments <- get_sentiments("nrc")

# Funktion til at beregne sentiment
calculate_sentiment <- function(text, lexicon) {
  # Tokenize teksten
  tokens <- unlist(strsplit(text, "\\s+"))
  
  # Match ord med sentimentværdier i lexikonet
  matched_words <- intersect(tokens, lexicon$word)
  
  # Beregn sentimentet
  sentiment_score <- sum(lexicon$value[match(matched_words, lexicon$word)], na.rm = TRUE)
  
  return(sentiment_score)
}

# Anvend funktionen til hver tekst og lexikon
alltexts <- data.frame(text = c(text1, text2, text3, text4))
# Beregn sentimentet for hvert tekst ved hjælp af AFINN lexikonet
alltexts <- alltexts %>%
  mutate(afinn_sentiment = sapply(text, calculate_sentiment, lexicon = afinn_sentiments))
# Udskriv resultatet
print(alltexts)

# Opret et data frame med sentiment-score for hvert tekst
sentiment_data <- data.frame(
  Text = 1:nrow(alltexts),  # Angiver tekstenummer
  Sentiment = alltexts$afinn_sentiment  # Sentiment-score fra AFINN lexikonet
)

# Opret plot
ggplot(sentiment_data, aes(x = Text, y = Sentiment, label = Text)) +
  geom_line() +
  geom_point() +
  geom_text(vjust = -1, hjust = 0.5, size = 3, color = "blue") +  # Justerer placering og udseende af tekstetiketter
  labs(
    title = "Sentiment over Mærsk årsrapporter 2019 - 2022",
    subtitle = "Hver årsrapport er delt op i 2 til 4 tekstnumre",
    x = "Tekstnummer",
    y = "Sentiment Score"
  )

# -------------------------------------------------------------- Topicmodel, Logratio, Sentiment ------------------------------------------------------------ # 
library(tm)
library(pdftools)
library(wordcloud)
library(topicmodels)
library(textdata)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Læs indholdet af PDF-filer - for hele PDFerne
#text1 <- pdf_text("2019MAERSK.pdf") 
#text2 <- pdf_text("2020MAERSK.pdf")
#text3 <- pdf_text("2021MAERSK.pdf")
#text4 <- pdf_text("2022MAERSK.pdf")

# Læs indholdet af PDF-filer, bestemte sider
text1 <- pdf_text("2019MAERSK.pdf")[6:9]
text2 <- pdf_text("2020MAERSK.pdf")[7:10]
text3 <- pdf_text("2021MAERSK.pdf")[7:9]
text4 <- pdf_text("2022MAERSK.pdf")[5:7]

#Læs PDF-filen og opret en tekstcorpus:
dokumenter <- Corpus(VectorSource(c(text1, text2, text3, text4)))

#Rens teksten ved at anvende tm-funktioner som tolower, removePunctuation, removeNumbers, 
#removeWords, osv.:
dokumenter <- tm_map(dokumenter, content_transformer(tolower))
dokumenter <- tm_map(dokumenter, removePunctuation)
dokumenter <- tm_map(dokumenter, removeNumbers)
dokumenter <- tm_map(dokumenter, removeWords, stopwords("en")) # Fjern engelske stopord
dokumenter <- tm_map(dokumenter, stripWhitespace)

#Opret dokument-term-matrix
dtm <- DocumentTermMatrix(dokumenter)

#---------------------------------------------- Topic models: 4 topics ---------------------------------------------#
#laver LDA
dtm_lda <- LDA(dtm, k = 2, control = list(seed = 1234))

# Konverter LDA-modellen til en ryddig datastruktur
ap_topics <- tidy(dtm_lda, matrix = "beta")

# Rens termer i ap_topics ved at fjerne mindre relevante eller irrelevante termer
ap_topics <- ap_topics %>%
  filter(!(str_detect(term, "\\d+") | term %in% stopwords("en") | term %in% c("also")))

# Hent de øverste termer for hver emne
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# Reorganisér termerne inden for hvert emne baseret på deres beta-værdi
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# ------------------------------------------------------------------------ Log Ratio ------------------------------------------------------------------------ #
# Plot hvilke artikler der indeholder de forskellige topic
beta_wide <- ap_topics%>%
  mutate(topic = paste0("topic",topic))%>%
  pivot_wider(names_from = topic, values_from = beta)%>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

# Plotte dine emner
beta_wide <- beta_wide %>%
  mutate(log_ratio = log2((topic2 + 1e-10) / (topic1 + 1e-10)))


# Filtrer for at fjerne termer, der er insignifikante i begge topics
significant_terms <- beta_wide %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  arrange(desc(abs(log_ratio))) %>%
  head(20)

# Plot de mest signifikante termer
ggplot(significant_terms, aes(x = reorder(term, log_ratio), y = log_ratio)) +
  geom_col(aes(fill = log_ratio > 0), show.legend = FALSE) + 
  coord_flip() +
  labs(title = "Miljø/Forretning - Top signifikante termer i topics",
       x = "Terms",
       y = "Log2 Ratio (Topic 2 / Topic 1)") +
  theme_minimal()


# ------------------------------------------------------ Sentiment score - Positive & Negative ladet ord ---------------------------------------------------- #
#funktionen tager en tekst som input, analyserer den for at finde ud af, hvor mange positive og
#negative ord der er i teksten, og beregner derefter en sentiment score baseret på forskellen
#mellem antallet af positive og negative ord

# Funktion til at udføre sentimentanalyse på en enkelt tekst
perform_sentiment_analysis <- function(text) {
  # Opretter en tibble med teksten
  sentiment_score <- text %>%
    tibble(text = .) %>%
    
    # Tokeniserer teksten
    unnest_tokens(word, text) %>%
    
    # Joiner med Bing sentimentlexikonet
    inner_join(get_sentiments("bing")) %>%
    
    # Tæller antallet af hvert sentiment
    count(sentiment) %>%
    
    # Ændrer dataformatet, så der er en kolonne for hvert sentiment
    spread(sentiment, n, fill = 0) %>%
    
    # Beregner den samlede sentiment score ved at trække antallet af negative ord fra antallet af positive ord
    mutate(sentiment_score = positive - negative)
  
  # Returnerer den samlede sentiment score
  return(sentiment_score$sentiment_score)
}

# Sentimentanalyse for hver tekst separat
sentiment_df <- data.frame(
  Text = c("Text1", "Text2", "Text3", "Text4"),
  Sentiment = sapply(list(text1, text2, text3, text4), perform_sentiment_analysis)
)

# Plot sentiment for hver tekst
ggplot(sentiment_df, aes(x = Text, y = Sentiment)) +
  geom_col(fill = "skyblue", width = 0.5) +
  geom_text(aes(label = Sentiment), vjust = -0.5, size = 3) +
  labs(
    title = "Sentiment over Mærsk årsrapporter 2019 - 2022",
    x = "Årsrapporter",
    y = "Sentiment Score"
  ) +
  scale_x_discrete(labels = c("2019", "2020", "2021", "2022"))

