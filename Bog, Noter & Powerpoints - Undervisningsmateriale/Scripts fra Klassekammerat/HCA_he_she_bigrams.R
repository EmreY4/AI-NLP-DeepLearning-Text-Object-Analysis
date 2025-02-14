library(gutenbergr)
library(tidyverse)
library(tidytext)

# Indhenter gutenberg metadata
metagut <- gutenberg_metadata

# Sørger efter hc andersne på englesk
metaidhca <- metagut %>% 
  filter(str_detect(author, "Hans Christian") & language == "en")

# Indhenter text (17 bøger)
hcatext <- gutenberg_download(metaidhca$gutenberg_id)


# Indhenter 1 bog
hcatext2 <- gutenberg_download(gutenberg_id = 1597)

# Deler tekst op ud fra kapitler
# Pattern til at finde sætninger hvor alt er stort
pattern <- "^[A-Z\\s-]+$"
hcatext2 <- hcatext2 %>% 
  mutate(kapitel = str_detect(hcatext2$text, pattern = pattern))
grep(pattern, hcatext2$text, value = TRUE, perl = TRUE)

# Laver bigrams, fjerner NA rækker, fjerner rækker med numre
hca_bigrams <- hcatext2 %>% 
  unnest_tokens(bigram, text,token = "ngrams", n = 2) %>% 
  na.omit() %>% 
  filter(!grepl("\\d", bigram))

# Splitter bigrams til 2 kolonner
hca_bigrams <- hca_bigrams %>% 
  separate(bigram, c("w1", "w2"), sep = " ")

# Splitter i køn
hca_bigrams_he <- hca_bigrams %>% 
  filter(w1 == "he")

hca_bigrams_she <- hca_bigrams %>% 
  filter(w1 == "she")

# Count af bigrams
hca_bigrams_he <- hca_bigrams_he %>% 
  count(w1, w2, sort = TRUE)

hca_bigrams_she <- hca_bigrams_she %>% 
  count(w1, w2, sort = TRUE)

# Samler he og she for at finde fælles ord og count
combined_hca <- inner_join(hca_bigrams_she, hca_bigrams_he, by = "w2")

# Laver gender ratio og gender kolonne
combined_hca <- combined_hca %>% 
  mutate(mfratio = n.x/n.y, gender = ifelse(mfratio > 1, "female", "male")) %>% 
  arrange(mfratio)

# Vælger top og bund 20
top_10 <- combined_hca %>%
  slice_head(n = 10)

bottom_10 <- combined_hca %>%
  slice_tail(n = 10)

combined_df <- bind_rows(top_10, bottom_10)


# Plotter 
ggplot(data = combined_df, aes(x = mfratio, y = reorder(w2, mfratio), fill = gender)) +
  geom_col()
