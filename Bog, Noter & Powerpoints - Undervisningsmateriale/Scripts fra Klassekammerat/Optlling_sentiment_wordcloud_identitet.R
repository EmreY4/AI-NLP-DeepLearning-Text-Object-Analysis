library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringr)
library(stopwords)
library(pdftools)
library(ggthemes)
library(wordcloud)
library(wordcloud2)


# Read pdf
text <- pdf_text("Eksamenscase.pdf")

# Vælger side 4 til 10 i pdf
opgaver <- as.character(text[4:10])

# Make tibble
df <- tibble(side = 1:7, text = opgaver)

df <- df %>% 
  unnest_tokens(word, text)

# Fjerner stopord
stopord <- stopwords(language = "da")
stopord <- c(stopord, c("opgave", "jeres", "if", "kan", "to"))
stopord <- tibble(word = stopord)


df <- df %>%
  anti_join(stopord)

# Fjerner alt som ikke er ord
df <- df[grepl("^[A-Za-z]+$", df$word), ]

# Plot resultat for side it
df_count <- df %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n))

# Omregner til procent
total_count <- sum(df_count$n)
df_count$percentage <- (df_count$n / total_count) * 100

# Filtrerer dataframe før plot
top_10 <- as.integer(df_count[10,"n"])

df_count_top <- df_count %>% 
  filter(n >= top_10)

# Plot 
ggplot(data = df_count_top, aes(x = percentage, y = reorder(word, percentage))) +
  geom_col(fill = "#4470b8") +
  labs(y = NULL,
       x = "Antal gange ord optræder",
       title = 'Ordet "model" optræder flest gange i opgavebeskrivelsen',
       subtitle = "Optælling af ord som optræder i dataanalyse eksamensopgaven",
       caption = "Data: eksamensopgave.pdf") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        text = element_text(family = "verdana"))

##### Wordclouds
# Laver wordcloud og gemmer
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

##### Sentiment-analyse
afinn <- get_sentiments("afinn")
aarup <- read.csv("aarup.csv", encoding = "latin1")
aarup <- aarup[,3:4]
colnames(aarup) <- c("word", "score")

df_count <- merge(df_count, aarup, by = "word")


##### Text identitet
# Read pdf
besvarelse <- pdf_text("Besvarelse-2.pdf")

# Vælger side 4 til 10 i pdf
opgaver <- as.character(besvarelse[4:68])


opgave_df <- tibble(side = 1:65, text = opgaver)

# Inddeler tekst i opgaver
opgave_df <- test %>%
  mutate(
    linenumber = row_number(),
    opgave = cumsum(str_detect(text, 
                                regex("^Opgave [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>% 
  unnest_tokens(word, text)

# Fjerner stopord
stopord <- stopwords(language = "da")
stopord <- c(stopord, c("opgave", "jeres", "if", "kan", "to"))
stopord <- tibble(word = stopord)


opgave_df <- opgave_df %>%
  anti_join(stopord)

# Fjerner alt som ikke er ord
opgave_df <- opgave_df[grepl("^[A-Za-z]+$", opgave_df$word), ]

# Finder count
opgave_count <- opgave_df %>% 
  group_by(opgave) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n))

# Finder total for hver opgave
opgave_total <- opgave_count %>% 
  group_by(opgave) %>% 
  summarise(total = sum(n))
  
opgave_count <- left_join(opgave_count, opgave_total)

# Frekvens pr. ord og rangering
freq_by_rank <- opgave_count %>% 
  arrange(desc(n)) %>%  
  group_by(opgave) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total) %>%
  ungroup() %>% 
  mutate(opgave = as.factor(opgave))

# Undersøger koefficienter
lm_resultat <- summary(lm(log10(term_frequency) ~ log10(rank), data = freq_by_rank))

# Plot
ggplot(freq_by_rank, aes(rank, term_frequency, color = opgave)) + 
  geom_abline(intercept = lm_resultat$coefficients[1], slope = lm_resultat$coefficients[2], 
              color = "gray50", linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = "none")

