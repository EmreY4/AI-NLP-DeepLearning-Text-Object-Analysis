#### ---------------------------------------------------- Tekstanalyse: Sentimentanalyse af Elgiganten & Power -------------------------------------------- ####  
# Emne: Analyse af kundeanmeldelser for Elgiganten og Power  
# Dette script demonstrerer, hvordan man:  
#   1. Indlæser og forbereder data fra kundeanmeldelser  
#   2. Rensning og forbehandling af tekstdata (fjerner støj og uønskede tegn)  
#   3. Udfører sentimentanalyse ved hjælp af Sentida-pakken  
#   4. Beregner gennemsnitlige sentiment scores for Elgiganten og Power  
#   5. Analyserer lix-tal for læsbarhed af anmeldelser  
#   6. Visualiserer sentimentfordelingen ved hjælp af histogrammer og barplots  
#   7. Udfører avanceret feature engineering med spacyr til navneordsekstraktion  
#   8. Undersøger bigrams og deres sentiment scores for at identificere vigtigste nøgleord  

# Libraries  
library(dplyr)        # Data manipulation  
library(stringr)      # Tekstmanipulation  
library(tidytext)     # Tekstanalyse  
library(tidyverse)    # Datahåndtering og visualisering  
library(spacyr)       # NLP analyse  
library(Sentida)      # Sentimentanalyse  
library(quanteda)     # Korpusanalyse  
library(ggplot2)      # Visualisering  

# --------------------------------------------------------- SCIPT NEEDS ATTENTION - LAVET AF WULF ----------------------------------------------------------- #
mylix <- function(text) {
  words=unlist(str_split(text," "))
  retval=mean(nchar(words))
  return(retval)
}

# RESEARCH GOAL
# som leder af Elg's kundeservice vil jeg gerne følge med i omtalen (neg og pos)
# af vores kerne-KPI'er (ex: kundeservice, transport)

# RETRIEVE - egl webscraping
elrv <- readRDS("elgiganten.rds")
pwrv <- readRDS("powerreviews.rds")

# PREPARE
elrv <- elrv %>% filter(nchar(content) > 35) %>% 
  filter(!str_detect(content,"Dato for oplevelse")) %>% 
  mutate(text=str_replace_all(content,"[^a-zæøåA-ZÆØÅ\\s-\\.,]",""), 
         length=nchar(text),
         trate=as.integer(rating / 10))
         
pwrv <- pwrv %>% filter(nchar(content) > 35) %>% 
  filter(!str_detect(content,"Dato for oplevelse")) %>% 
  mutate(text=str_replace_all(content,"[^a-zæøåA-ZÆØÅ\\s-\\.,]",""), 
         length=nchar(text),
         trate=as.integer(rating / 10))
         
elrv <- elrv %>% rowwise() %>% mutate(score=sentida(text, output = "mean"))
pwrv <- pwrv %>% rowwise() %>% mutate(score=sentida(text, output = "mean"))
elrv <- elrv %>% rowwise() %>% mutate(lix=mylix(text))
pwrv <- pwrv %>% rowwise() %>% mutate(lix=mylix(text))

labs=c("5","4","3","2","1")
interval=c(5,3,1,-1,-3,-5)
elrv <- elrv %>% rowwise() %>% mutate(catscore=cut(score, breaks=interval,labels=labs))
pwrv <- pwrv %>% rowwise() %>% mutate(catscore=cut(score, breaks=interval,labels=labs))

# EXPLORE

hist(pwrv$trate)
hist(elrv$trate)
hist(pwrv$score)
hist(elrv$score)
hist(elrv$trate)
hist(elrv$lix)
ggplot(elrv,aes(x=catscore))+geom_bar()
ggplot(pwrv,aes(x=catscore))+geom_bar()


# MORE FE with spacy
# vi skal have fundet navneord fra reviews
spacy_initialize(model = "da_core_news_sm")
elrv_spacy=spacy_parse(elrv$text)
pwrv_spacy=spacy_parse(pwrv$text)

elrvNouns = elrv_spacy %>% filter(pos=="NOUN") %>% select(doc_id,lemma)
pwrvNouns = pwrv_spacy %>% filter(pos=="NOUN") %>% select(doc_id,lemma)
elrvNounsC = elrvNouns %>% count(lemma)
pwrvNounsC = pwrvNouns %>% count(lemma)

# inner-join så vi finder fælles navneord
pw_el = inner_join(elrvNounsC,pwrvNounsC,by="lemma")
colnames(pw_el)=c("lemma","elcount","pwcount")
pw_el = pw_el %>% mutate(total=elcount+pwcount,
                         elpwrat=elcount/pwcount,
                         elwin=ifelse(elpwrat > 1,1,0))

pw_el %>% filter(total > 4000) %>%  filter(!str_detect(lemma,"lgigant")) %>% 
  ggplot(aes(x=reorder(lemma,elpwrat),y=elpwrat, fill=as.factor(elwin)))+geom_bar(stat="identity")+
  coord_flip()+
  labs(title="Elgiganten vinder på pakker",x="Forhold mellem El og PW", y="ord")+
  theme(legend.position = "none")

# MORE FE with Bigrams
# create
elgrams <- elrv %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2)
pwgrams <- pwrv %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2)

# splitte bigram i to mhp filtering and counting
elgrams_sep <- elgrams %>% separate(bigram,c("w1","w2"),sep=" ")
pwgrams_sep <- pwgrams %>% separate(bigram,c("w1","w2"),sep=" ")

# sentiment-library fra dsl
dkSent=read_csv("2_headword_headword_polarity.csv")
colnames(dkSent)=c("w1","V2","pos","V3","score","family")
dkSent_adj=dkSent %>% filter(pos=="adj.") %>% select(w1,score)

elgrams_sep_u = distinct(elgrams_sep)
elgrams_sep_filter = elgrams_sep_u %>% filter(w1 %in% dkSent_adj$word,
                                            w2 %in% pw_el$lemma)
elgrams_C = elgrams_sep_filter %>% count(w1,w2,sort=T)
elgrams_C =inner_join(elgrams_C,dkSent_adj, by=c("w1"))
elgrams_C= elgrams_C %>% mutate(totscore=n*score) %>% arrange(totscore)

# Find top og bund
elBund=head(elgrams_C,10)
elTop=tail(elgrams_C,10)
elTopBund=rbind(elBund,elTop)
elTopBund = elTopBund %>% unite(col="bigram",c("w1","w2"))

ggplot(elTopBund, aes(x=reorder(bigram,totscore),y=totscore))+geom_bar(stat="identity")+
  coord_flip()

# gentages for power

pwgrams_sep_u = distinct(pwgrams_sep)
pwgrams_sep_filter = pwgrams_sep_u %>% filter(w1 %in% dkSent_adj$w1,
                                            w2 %in% pw_el$lemma)
pwgrams_C = pwgrams_sep_filter %>% count(w1,w2,sort=T)
pwgrams_C =inner_join(pwgrams_C,dkSent_adj, by=c("w1"))
pwgrams_C= pwgrams_C %>% mutate(totscore=n*score) %>% arrange(totscore)

# Find top og bund
pwBund=head(pwgrams_C,10)
pwTop=tail(pwgrams_C,10)
pwTopBund=rbind(pwBund,pwTop)

pwTopBund = pwTopBund %>% unite(col="bigram",c("w1","w2"))
ggplot(pwTopBund, aes(x=reorder(bigram,totscore),y=totscore))+geom_bar(stat="identity")+
  coord_flip()

# join
pw_el_totscore=inner_join(pwTopBund,elTopBund,by="bigram")
colnames(pw_el_totscore)=c("bigram","pwcnt","pwscore","pwtotscore","elcnt","elscore","eltotscore")

pw_el_totscore_long=pw_el_totscore %>% 
  pivot_longer(cols = c("pwtotscore","eltotscore"),names_to="varehus",values_to = "totscore") %>% 
  select("bigram","varehus","totscore")

ggplot(pw_el_totscore_long, aes(x=reorder(bigram,totscore),y=totscore, fill=as.factor(varehus)))+
  geom_bar(stat="identity", position="dodge")+
  coord_flip()+
  labs(title="Power (de grønne) har bedste kundeservice", x="total score",y="bigram")+
  theme(legend.position = "none")



