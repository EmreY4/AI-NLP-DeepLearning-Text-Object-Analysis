#### --------------------------------------------------------- Analyse af PDF-tekst & Ordfrekvenser ------------------------------------------------------- ####  
# Emne: Tekstbehandling og analyse af ordfrekvenser fra en PDF  
# Dette script demonstrerer, hvordan man:  
#   1. Indlæser og konverterer tekst fra en PDF-fil  
#   2. Renser teksten ved at fjerne tegnsætning, tal og stopord  
#   3. Beregner det samlede antal ord i PDF'en  
#   4. Opdeler teksten i individuelle ord og opretter en ordtabel  
#   5. Filtrerer ordtabelen for at fjerne stopord og sorterer efter hyppighed  
#   6. Udfører sentimentanalyse ved hjælp af "afinn" og "nrc" sentimentbiblioteker  
#   7. Visualiserer ordfrekvenser ved hjælp af wordclouds og histogrammer  
#   8. Analyserer ordets rang og termfrekvenser gennem logaritmisk regression  
#   9. Udfører lineær regressionsanalyse på ordfrekvenserne for de 500 mest hyppige ord  

# Libraries  
library(pdftools)      # Læsning og ekstraktion af tekst fra PDF  
library(tm)             # Tekstforberedelse og rensning  
library(janeaustenr)    # Eksempler på bøger og tekster  
library(dplyr)          # Data manipulation  
library(stringr)        # Streng manipulation  
library(tidytext)       # Tekstdata behandling  
library(textdata)       # Sentimentanalyse  
library(ggplot2)        # Visualisering af data  
library(reshape2)       # Omdannelse af data for visualisering

# ------------------------------------------------------------------------- Kapitel 1 ----------------------------------------------------------------------- #
# Indlæser PDF tekst
pdf_text <- pdf_text("Case.pdf")

# Konverter teksten til et korpus
corpus <- Corpus(VectorSource(pdf_text))
# Fjern tegn, konverter til små bogstaver og tokeniser teksten
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("danish")) # Fjern danske stopord
corpus <- tm_map(corpus, stripWhitespace)
# Konverter korpus til en tekstvektor
text_vector <- unlist(sapply(corpus, `[`, "content"))

# Tæl det totale antal ordpdf_
antal_ord <- sum(str_count(pdf_text, "\\S+"))

# Opdel tekst i ord og fjern bindestreger
ord <- unlist(str_split(pdf_text, "\\s+"))
ord <- str_replace_all(ord, "-", "")
# Opret en data frame med ordene
ord_df <- data.frame(ord)
# Print det totale antal ord (uden bindestreger)
print(antal_ord)
# Opret ordtabel og tæl forekomster
ord_tabel <- ord_df %>%
  group_by(ord) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Indlæs stopord
stopord <- readLines("stopord.txt")
# Filtrér ordtabel for stopord
filtreret_ordtabel <- ord_tabel %>%
  filter(!ord %in% stopord) %>%
  arrange(desc(count))
# Vis den filtrerede ordtabel
print(filtreret_ordtabel)


#
# Tidytext - Afinn
afinn <- get_sentiments("afinn")
# Glædelige ord
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]",
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
# Filtrering og subsetting
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
# Subsetter efter bogen "Emma"
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)
# Sektionsopdeling - Positive & Negative ord
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)


# ------------------------------------------------------------------------- Kapitel 2 ----------------------------------------------------------------------- #
# Wordclouds
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# Word frekvens - Fjern stopord
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)
total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))
book_words <- left_join(book_words, total_words)
# Print resume
book_words
# plot
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# Arrangering - Arrangere hver bog efter ordets forekomst
freq_by_rank <- book_words %>%
  arrange(desc(n)) %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n / total) %>%
  ungroup()
# Print resumé
freq_by_rank
# Plot - Rangliste
ggplot(freq_by_rank, aes(x = rank, y = `term frequency`, color = book)) +
  geom_line() +  # Bruger linjegeometri til at forbinde punkter
  scale_x_log10() +  # Logaritmisk skala på x-aksen
  scale_y_log10() +  # Logaritmisk skala på y-aksen
  geom_abline(intercept = coef(lm_model)[1], 
              slope = coef(lm_model)[2], 
              linetype = "dotted", 
              color = "black") +
  theme_minimal()  # Anvender et minimalt tema for en renere præsentation

# lm-model
rank_subset <- freq_by_rank %>%
  filter(rank < 500, 
         rank > 10)
# Resultater for lm
lm_model <- lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
# Print resultater
print(lm_model)
# Plot af rangliste - lm-model
ggplot(rank_subset, aes(x = rank, y = `term frequency`, color = book)) +
  geom_line() +
  scale_x_log10() +  # Logaritmisk skala på x-aksen
  scale_y_log10() +  # Logaritmisk skala på y-aksen
  geom_abline(intercept = coef(lm_model)[1], 
              slope = coef(lm_model)[2], 
              linetype = "dotted", 
              color = "black") +
  theme_minimal()

