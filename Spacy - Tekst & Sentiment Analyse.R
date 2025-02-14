#### ---------------------------------------------------- Tekst & Sentimentanalyse af produktanmeldelser -------------------------------------------------- ####  
# Emne: Sentimentanalyse og ordforrådsanalyse af produktanmeldelser fra Elgiganten og Power  
# Dette script demonstrerer, hvordan man:  
#   1. Læser og renser anmeldelser fra Elgiganten og Power ved at filtrere korte anmeldelser og specialtegn  
#   2. Beregner LIX for hver anmeldelse for at vurdere læsbarheden  
#   3. Anvender sentimentanalyse ved hjælp af Sentida for at beregne den gennemsnitlige score for hver anmeldelse  
#   4. Udfører ordforrådsanalyse ved at finde de mest hyppige substantiver i anmeldelserne  
#   5. Skaber ordtælling for at sammenligne ordforrådet mellem Elgiganten og Power  
#   6. Genererer et plot for at visualisere forskelle i hyppigheden af ord i begge datasæt  
#   7. Analyserer bigrams (to-ords kombinationer) i anmeldelserne og deres relation til sentiment  
#   8. Identificerer positive og negative bigrams og visualiserer deres fordeling i anmeldelserne  

# Libraries  
library(corrplot)      # Visualisering af korrelationer  
library(ggplot2)       # Visualisering af data  
library(corrgram)      # Korrelation visualisering  
library(dplyr)         # Data manipulation  
library(tidyr)         # Datatransformation  
library(stringr)       # Streng manipulation  
library(tidytext)      # Tekst mining  
library(spacyr)        # Natural language processing  
library(Sentida)       # Sentimentanalyse

# --------------------------------------------------------------------------- Util -------------------------------------------------------------------------- #
#UTIL
getLix <- function(content) {
  words=str_split(content," ")
  size=nchar(content)
  lix=size/length(words[[1]])
  return(lix)
}

# Dataindsamling - Byg en dataframe med review-indhold fra Silvan
elgig_reviews=readRDS("elgiganten.rds")
pow_reviews=readRDS("powerreviews.rds")

# Datapreperation - clean and transform
elrv <- elgig_reviews %>% 
  rename(text=content) %>% 
  filter(nchar(text) > 35) %>% 
  filter(!grepl("Dato for oplevelse",text)) %>% 
  mutate(text=str_replace_all(text,"[^[:alpha:]\\s]","")) %>% 
  mutate(length=nchar(text)) 

pwrv <- pow_reviews %>% 
  rename(text=content) %>% 
  filter(nchar(text) > 35) %>% 
  filter(!grepl("Dato for oplevelse",text)) %>% 
  mutate(text=str_replace_all(text,"[^[:alpha:]\\s]","")) %>% 
  mutate(length=nchar(text))

elrv <- elrv %>%  rowwise() %>% mutate(score=sentida(text, output = "mean"))
elrv <- elrv %>%  rowwise() %>% mutate(lix=getLix(text))
pwrv <- pwrv %>%  rowwise() %>% mutate(score=sentida(text, output = "mean"))
pwrv <- pwrv %>%  rowwise() %>% mutate(lix=getLix(text))

elrv$doc_id <- rownames(elrv)
pwrv$doc_id <- rownames(pwrv)

# -------------------------------------------------------------------------- Spacy -------------------------------------------------------------------------- #
# Parse
spacy_initialize(model = "da_core_news_sm")
elrv_spacy=spacy_parse(elrv)
pwrv_spacy=spacy_parse(pwrv)

# nouns
elNouns=elrv_spacy %>% filter(pos=="NOUN")  %>% select(doc_id,lemma)
pwNouns=pwrv_spacy %>% filter(pos=="NOUN")  %>% select(doc_id,lemma)
# clean nouns
elNouns1 = elNouns %>% mutate(text=str_replace_all(lemma,"[^[:alpha:]\\s]",""))
pwNouns1 = pwNouns %>% mutate(text=str_replace_all(lemma,"[^[:alpha:]\\s]",""))
# top 10 nouns
elNounsC=elNouns1 %>% count(lemma) %>% arrange(-n) %>% filter(n>400)
pwNounsC=pwNouns1 %>% count(lemma) %>% arrange(-n) %>% filter(n>400)

# elg and power
pw_el = inner_join(elNounsC,pwNounsC, by="lemma")
colnames(pw_el)=c("lemma","elgig","power")
pw_el_long <- pivot_longer(pw_el, cols = c(elgig, power), names_to = "Variable", values_to = "Occurrence")

# Create a plot using ggplot
ggplot(pw_el_long, aes(x = reorder(lemma,-Occurrence), y = Occurrence, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Words", y = "Occurrences") +
  scale_fill_manual(values = c("blue", "green"), 
                    labels = c("elgiganten", "power"),
                    name = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# -------------------------------------------------------------------------- Bigrams ------------------------------------------------------------------------ #
# create 
elgrams=elrv %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)
pwgrams=pwrv %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)
# split and filter
elgrams_sep = elgrams %>% separate(ngram,c("w1","w2"), sep = " ")
pwgrams_sep = pwgrams %>% separate(ngram,c("w1","w2"), sep = " ")

# importer dansk sentiment-liste og filtrér ud tillægsord
dkSenti <- read.csv("2_headword_headword_polarity.csv", header = F)
colnames(dkSenti)=c("word","V2","pos","V4","score","lemmas")
dkSenti_adj <- dkSenti %>% filter(pos=="adj.")
dkSenti_adj_sm<- dkSenti_adj %>% select(word,score)

# lav en liste af positive ord som forekommer hos begge
# elg
el_bigrams_filtered <- elgrams_sep %>%
  filter(w1 %in% dkSenti_adj$word) %>%
  filter(w2 %in% elNounsC$lemma)
# pw
pw_bigrams_filtered <- pwgrams_sep %>%
  filter(w1 %in% dkSenti_adj$word) %>%
  filter(w2 %in% pwNounsC$lemma)

# tæl forekomster
#elg
el_bigram_counts <- el_bigrams_filtered %>% 
  count(w1, w2, sort = TRUE)
#pw
pw_bigram_counts <- pw_bigrams_filtered %>% 
  count(w1, w2, sort = TRUE)

#join the count with dk-senti
#elg
el_bigram_counts_sent=inner_join(el_bigram_counts,dkSenti_adj_sm, by=c("w1"="word"))
#pw
pw_bigram_counts_sent=inner_join(pw_bigram_counts,dkSenti_adj_sm, by=c("w1"="word"))

# do the sent-count
#el
el_bigram_counts_sent_scored <- el_bigram_counts_sent %>% mutate(sscore=n*score) %>% arrange(-sscore)
#pw
pw_bigram_counts_sent_scored <- pw_bigram_counts_sent %>% mutate(sscore=n*score) %>% arrange(-sscore)

# Find top pos 10 og top neg 10
#elg
el_pos = head(el_bigram_counts_sent_scored,10)
el_neg = tail(el_bigram_counts_sent_scored,10)
el_tot=rbind(el_pos,el_neg)
el_tot_plot= el_tot %>% unite(col="bigram",c("w1","w2")) %>% select(bigram,sscore) %>% 
  mutate(company="elgiganten")
#pw
pw_pos = head(pw_bigram_counts_sent_scored,10)
pw_neg = tail(pw_bigram_counts_sent_scored,10)
pw_tot=rbind(pw_pos,pw_neg)
pw_tot_plot= pw_tot %>% unite(col="bigram",c("w1","w2")) %>% select(bigram,sscore) %>% 
  mutate(company="power")

#Plot
# elg
ggplot(el_tot_plot,aes(x=reorder(bigram,sscore), y=sscore))+
  geom_bar(stat = "identity")+
  coord_flip()
# Plot
# pw
ggplot(pw_tot_plot,aes(x=reorder(bigram,sscore), y=sscore))+
  geom_bar(stat = "identity")+
  coord_flip()

# pw og elg
# join data-frames
pw_el_tot = inner_join(pw_tot_plot,el_tot_plot, by="bigram")
colnames(pw_el_tot) = c("bigram","score-power","power","score-elgiganten","elgiganten")
pw_el_tot_plot <- pivot_longer(pw_el_tot, cols = c(`score-power`,`score-elgiganten`), names_to = "score", values_to = "Occurrence")
pw_el_tot_plot2 = pw_el_tot_plot %>% select(-power,-elgiganten)

# Plot
ggplot(pw_el_tot_plot2, aes(x = reorder(bigram,Occurrence), y = Occurrence, fill = score)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bigram", y = "Count")+
  coord_flip()