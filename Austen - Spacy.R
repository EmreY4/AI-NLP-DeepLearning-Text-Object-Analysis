#### --------------------------------------------------------- Tekstanalyse med Jane Austen-tekster via Spacy----------------------------------------------- ####  
# Emne: Analyse af Jane Austen-tekster med fokus på ordklasser og entiteter  
# Dette script demonstrerer, hvordan man:  
#   1. Indlæser og forbereder Jane Austen-teksterne for analyse  
#   2. Bruger `spacy` til at analysere syntaks og ordklasser i teksterne  
#   3. Filtrerer og renser data for specifikke ordklasser, såsom adjektiver og verber  
#   4. Udfører POS-tagging og entitetsgenkendelse for at udtrække personnavne og verber  
#   5. Analyserer ordsætninger i specifikke bøger som "Sense and Sensibility"  
#   6. Beregner antallet af personer og verber i forskellige sektioner af teksten  
#   7. Visualiserer resultaterne af tekstanalyserne ved at bruge relevante metoder i `ggplot2`

# Libraries  
library(dplyr)        # Data manipulation  
library(tidyr)        # Data transformation  
library(spacyr)       # Natural Language Processing  
library(tidyverse)    # Data manipulation and visualization  
library(ggplot2)      # Data visualization  
library(janeaustenr)  # Jane Austen text dataset  
library(stringr)      # String manipulation  

# ----------------------------------------------------------------------------- Spacy ------------------------------------------------------------------------ # 
# Load English anguage model
spacy_initialize(model = "en_core_web_sm")

# Get the total text of all JA books
austen_textSS <- austen_books() %>%
  group_by(book) %>% 
  filter(book=="Sense & Sensibility") %>% ungroup()
austen_textSS = austen_textSS %>% mutate(doc_id=row_number()) %>% select(-book)
JASS=spacy_parse(austen_textSS)
JASS_verbs=JASS %>% filter(pos=="ADJ") %>% select(token) %>% unique() %>% rename('w2'='token')
austeks <- austen_books() %>% group_by(book) %>% filter(str_detect(book,"Sense")) %>% ungroup()
sense_sentences <- austeks %>% 
  unnest_tokens(sentence, text, token = "sentences")
sense_sentences['doc_id']=1:nrow(sense_sentences)
sense_sentences <- sense_sentences %>% rename('text'='sentence')
ausPOSparse <- spacy_parse(sense_sentences, pos = TRUE, entity = TRUE, dependency = TRUE)

ausPOSEx = ausPOS %>% filter(str_detect(entity,"PERSON") |  pos=="VERB")
ausPOSExP = ausPOSEx %>% filter(str_detect(entity,"PERSON")) %>% group_by(doc_id) %>%
  summarise(pc=n()) %>% ungroup()