library(tidyquant)
library(quantmod)
library(tidyverse)
library(ggthemes)
library(pdftools)
library(tidytext)
library(stopwords)
library(lubridate)

# Indhenter aktiekurs
stocknames <- c("NVDA", "AMD")
stock_prices <- tq_get(stocknames)

# Indlæser NVDA regnskaber
folder <- "/Users/marius/Documents/DataAnalyseProjekter/NLP & Deep-Learning/NLP & Deep-Leaning R/NVDA" 
pdf_files <- list.files(folder, pattern = "\\.pdf$", full.names = TRUE)

# Laver liste til at gemme pdf filer
pdf_list <- list()

# Looper igennem hver pdf fil
for (i in 1:length(pdf_files)) {
  
  # Læser pdf fil
  pdf_text <- pdf_text(pdf_files[i])
  
  # Gemmer i liste
  pdf_list[[basename(pdf_files[i])]] <- pdf_text
}

# Udvælger relevant indformation (stakeholder text)
pdf_list[[1]] <- pdf_list[[1]][12:14]
pdf_list[[2]] <- pdf_list[[2]][12:14]
pdf_list[[3]] <- pdf_list[[3]][12:14]
pdf_list[[4]] <- pdf_list[[4]][12:14]
pdf_list[[5]] <- pdf_list[[5]][12:16]
pdf_list[[6]] <- pdf_list[[6]][14:18]
pdf_list[[7]] <- pdf_list[[7]][14:18]
pdf_list[[8]] <- pdf_list[[8]][14:18]

## Laver sentiment score på hvert år
# Indhenter sentiment ord
sentiment_list <- get_sentiments("afinn")

# Laver dataframe ti 

# Laver dataframe til at gemme resultater af sentiment
result_df <- as.data.frame(matrix(ncol = 3, nrow = length(pdf_files)))
colnames(result_df) <- c("Årstal", "Sentiment", "Afkast")
årstal <- seq((2023 - length(pdf_list) + 1),2023,1)

for (i in 1:nrow(result_df)) {
  result_df[i,1] <- årstal[i]
}


for (i in 1:length(pdf_list)) {
  # Indlæser text og laver til dataframe med ord
  df <- tibble(side = 1:length(pdf_list[[i]]), text = as.character(pdf_list[[i]]))
  df <- df %>% 
    unnest_tokens(word, text)
  
  # Fjerner stopord
  stopord <- stopwords(language = "en")
  stopord <- tibble(word = stopord)
  df <- df %>%
    anti_join(stopord, by = "word")
  
  # Fjerner alt som ikke er ord
  df <- df[grepl("^[A-Za-z]+$", df$word), ]
  
  # Laver ordoptælling
  df <- df %>% 
    count(word, sort = TRUE) %>% 
    mutate(word = reorder(word, n))
  
  df <- merge(df, sentiment_list, by = "word")
  
  # Sentiment weight
  df$sentiment <- df$value * df$n
  
  # Sentiment score
  sentiment <- sum(df$sentiment)
  
  result_df[i,2] <- sentiment
  # Printer sentiment score
  cat("Sentiment score for", names(pdf_list)[i], "er", sentiment, "\n")
  
}

# Udregner årlig afkst på aktie
stock_data <- stock_prices %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  mutate(days_from_jun1 = abs(as.numeric(date - as.Date(paste(year, "-06-01", sep = ""))))) %>%
  slice(which.min(days_from_jun1)) %>%
  ungroup()


stock_data$yoy <- NA

for (i in 1:(nrow(stock_data)-1)) {
  increase <- (stock_data[i + 1,"adjusted"] - stock_data[i,"adjusted"]) / stock_data[i,"adjusted"] * 100
  stock_data[i + 1, "yoy"] <- increase
}

# Gemmer i resultat df
result_df["Afkast"] <- stock_data[(nrow(stock_data) - nrow(result_df)) : nrow(stock_data), "yoy"]

# Omregner resultat_df til procent
result_df_pct <- result_df

avg_sentiment <- mean(result_df_pct$Sentiment)
result_df_pct$Sentiment_Percentage <- ((result_df_pct$Sentiment - avg_sentiment) / avg_sentiment) * 100

avg_afkast <- mean(result_df_pct$Afkast)
result_df_pct$Afkast_Percentage <- ((result_df_pct$Afkast - avg_afkast) / avg_afkast) * 100

result_df_pct$Årstal <- as.Date(paste(result_df_pct$Årstal, "-06-01", sep = ""))

# Plotter resultat
nvda_plot <- ggplot(data = result_df_pct, aes(x = Årstal)) +
  geom_line(aes(y = Sentiment_Percentage, color = "Sentiment-score"), linewidth = 1) +
  geom_line(aes(y = Afkast_Percentage, color = "Afkast"), linewidth = 1) +
  scale_color_manual(values = c("Sentiment-score" = "#76B900", "Afkast" = "#6e6e6e")) +
  labs(title = "NVIDIA's aktiekurs følger delvist sentiment-score i de seneste år",
       subtitle = "Afkast & Sentiment-score sammenlignet med gennemsnit \nSentiment-score beregnet på NVIDIA's stakeholder statement i årsrapport",
       x = "",
       y = "",
       caption = "Data: Yahoo Finance & NVIDIA's årsrapport") +
  scale_y_continuous(breaks = seq(-200,200,50), 
                     labels = function(x) paste0(x, "%")) +
  coord_cartesian(ylim = c(-200,200)) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(filename = "nvda_plot.jpeg", plot = nvda_plot, width = 8, height = 5, dpi = 300)

# Beregner sentimentscore's korrelation med næste års afkast
cor_df <- as.data.frame(matrix(ncol = 2, nrow = (nrow(result_df)-1)))
colnames(cor_df) <- c("Sentiment", "Afkast+1år")
cor_df$Sentiment <- result_df$Sentiment[1:7]
cor_df$`Afkast+1år` <- result_df$Afkast[2:8]

cor(x = cor_df$Sentiment, y = cor_df$`Afkast+1år`)
