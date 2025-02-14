library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(ggthemes)
library(pdftools)
library(RColorBrewer)
library(stopwords)

##### Topic Modelling
# LDA - Lineær Diskriminant Analyse
# Sandsynlighed for emneord
# Klassifikation for hvert dokument

data("AssociatedPress")

AssociatedPress <- as.matrix(AssociatedPress)

# Indlæser tekst
article1 <- "DENVER, COLORADO — The Southern Nevada Water Authority on Thursday voted to accept a $2.4 million grant from the U.S. Bureau of Reclamation to fund cloud seeding in other Western states whose rivers feed the parched desert region.The weather modification method uses planes and ground-based cannons to shoot silver iodide crystals into clouds, attracting moisture to the particles that fall as additional snow and rain.The funding comes as key reservoirs on the Colorado River hit record lows and booming Western cities and industries fail to adjust their water use to increasingly shrinking supplies.This money from Reclamation is wonderful. We just have to decide how exactly it's going to benefit us, said Andrew Rickert, who coordinates Colorado's cloud seeding for the Colorado Water Conservation Board.The federal funding will go toward upgrading manual generators to ones that can be remotely operated and using planes to seed clouds in key parts of the Upper Colorado River Basin, according to Southern Nevada Water Authority documents for its board meeting.Securing enough generators could be a challenge, Rickert said. There's not a lot of makers of cloud seeding generators, he said. Not only do we have to make sure we can find that, but that they could make as many as we need.The Bureau of Reclamation declined to comment about the funding decision.The Southern Nevada Water Authority said the grant, while administered by Nevada, is not exclusively for the state's benefit. It will all be used to do cloud seeding in the Upper Basin for the benefit of all the river's users, wrote public outreach officer Corey Enus over email.In the Upper Colorado River Basin, Utah and Colorado have been seeding clouds for decades. Wyoming has nearly a decade of experience, and New Mexico recently began approving permits for warm weather seeding in the eastern part of the state.Colorado, Utah and Wyoming each spend between about $1 million and $1.5 million a year for cloud seeding. Utah's legislature recently expanded its investment in cloud seeding programs in next year's state budget, allocating more than $14 million.Numerous studies indicate cloud seeding can add 5% to 15% more precipitation from storm clouds. Contractors work with states to estimate how much ends up in water supplies.Since 2007, various groups have contributed to the overall cloud seeding budgets in those states. In 2018, several entities, including the Southern Nevada Water Authority, committed to long-term funding for those efforts, collectively contributing about $1.5 million annually.The reclamation bureau regularly funded cloud seeding operations toward the end of the 20th century, but has largely backed off in recent years, according to Frank McDonough a scientist at the nonprofit Desert Research Institute.The research that's come out over the last 10 years or so really seems to have convinced them that cloud seeding is a legitimate way to increase snowpack and subsequent water resources, McDonough said.The grant from the bureau will be spread out over two years, temporarily doubling financial support for the Upper Basin cloud seeding from outside parties.The seven Colorado River basin states are still negotiating with the Bureau of Reclamation on how they will conserve 2 million to 4 million acre-feet of water. The bureau is expected to release a draft proposal this month and expects to finalize plans by mid-August, when it typically announces the amount of water available from the Colorado River for the following year.With such an overallocated river, everyone will have to use less, particularly the agricultural sector, said Kathryn Sorensen of the Kyl Center for Water Policy think tank.I think a lot the allure of this type of program is it's easier to talk about how do we get more than to talk about who has to use less, she said."
article2 <- "Hilarious Skyrim Bug Makes Ralof Do Irish Jig Skyrim fans have encountered some funny glitches while playing the game, and one bug makes it look like Ralof is doing the Irish jig The bug in The Elder Scrolls 5: Skyrim makes it look like Ralof is doing the Irish jig, which some players find hilarious. Players have encountered numerous bugs and glitches in Skyrim over the years, some of which can negatively impact gameplay. The bug with Ralof walking in mid-air and glitching out does not appear to be a major issue, and similar glitches can often be fixed by reloading the game. A hilarious bug in The Elder Scrolls 5: Skyrim makes it look like Ralof is performing the Irish jig. The Elder Scrolls 5: Skyrim is infamous for its bugs, as gamers have encountered a wide variety of glitches in the game over the years. Some of these bugs can lead to hilarious situations, while others can be frustrating to deal with for the players. As an example, a recently reported Skyrim bug made the world look more like Morrowind, as the shared images highlighted broken ground textures. Similar glitches can negatively impact the gameplay experience, and it seems like users are still coming across bugs in The Elder Scrolls 5: Skyrim. Another funny glitch in Skyrim is just as ridiculous, as it makes it look like Ralof is doing the Irish jig. A Reddit user named Quiet_Pelican72 has shared a clip featuring this Skyrim glitch, and gamers can see that Ralof is completely bugged out in the video. This glitch mainly impacts Ralof’s body parts and animations, as it appears that he is walking in mid-air while constantly moving his legs. He hurries into the nearby keep to get away from the attack, but the bug continues to impact his animations. However, it doesn’t seem like a major issue, as these glitches can sometimes be fixed by reloading the game. Quiet_Pelican72 also confirmed that they were playing on the Xbox Series X version of Skyrim, before calling it the buggiest version of the game. The player then claimed to have encountered more crashes on Xbox Series X than any other version of The Elder Scrolls 5: Skyrim. It’ll be interesting to see if this bug is present on other platforms, and if there is a particular event that triggers it. A few users in the comments section jokingly state that Ralof is just trying to do the Irish jig, but one fan claims to have seen another NPC perform the same animations. Some other players comment on how Ralof appears to be dancing in Skyrim, with one gamer stating that he’s on an invisible unicycle. One fan also makes note of the overall buggy nature of Skyrim, stating that they have never seen a game with so many glitches. Even though Ralof was walking in mid-air, it seems like only his animations and body parts were bugged out. Skyrim players can encounter other glitches that can greatly impact the gameplay experience, such as the intro sequence not functioning correctly. The Elder Scrolls 5: Skyrim - Anniversary Edition is available on PC, PS4, PS5, Switch, Xbox One, and Xbox Series X/S."

article3 <- as.String(pdf_text("articles/Svindel.pdf"))
article4 <- as.String(pdf_text("articles/Macron.pdf"))
article5 <- as.String(pdf_text("articles/Doegninsti.pdf"))
article6 <- as.String(pdf_text("articles/Salling.pdf"))

# Laver corpus fra text
corpus <- Corpus(VectorSource(c(article3, article4, article5, article6)))
#corpus <- Corpus(VectorSource(c(article1, article2)))

# Renser teksten
# Preprocess the corpus
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("da"))

# Laver funktion til at fjerne symboler
removeSymbols <- function(text) {
  text <- gsub("[[:punct:]]", " ", text)
  text <- gsub("\\s+", " ", text)  
  text <- gsub("^\\s+|\\s+$", "", text) 
  return(text)
}

# Bruger funktion på corpus
corpus <- tm_map(corpus, content_transformer(removeSymbols))

# Laver DocumentTermMatrix
dtm <- DocumentTermMatrix(corpus)

# Convert the DTM to a matrix
# dtm_matrix <- as.matrix(dtm)

# Laver LDA model 
test <- LDA(dtm, k = 4, control = list(seed = 1234))

topics <- tidy(test, matrix = "beta")

# Finder top 10 ord for hvert emne
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, desc(beta)) %>% 
  mutate(topic = paste0("Topic ", topic))


# Plotter top 10 ord
ggplot(data = top_terms, aes(x = beta, y = reorder(str_to_title(term), beta))) +
  geom_col(aes(fill = factor(topic))) +
  labs(title = "Betydelige forskelle mellem de mest anvendte ord",
       subtitle = "Top 10 mest brugte ord i hvert emne") +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  facet_wrap(~ topic, scales = "free_y")
  

# Laver andet plot
beta_wide <- topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

# Laver top og bund 5
top_bottom_terms <- beta_wide %>%
  arrange(log_ratio) %>%
  filter(row_number() <= 15 | row_number() > n() - 15)

ggplot(data = top_bottom_terms, aes(x = log_ratio, y = reorder(str_to_title(term), log_ratio))) +
  geom_col(fill = "#3691d1") +
  labs(title = "Stor forskellen mellem brug af ord i de forskellige emner",
       subtitle = "Ord med de største forskelle mellem de to emner",
       x = "Log Ratio",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"))


midt_beta <- beta_wide %>% 
  filter(log_ratio > -15 & log_ratio < 15)

ggplot(data = midt_beta, aes(x = log_ratio, y = reorder(str_to_title(term), log_ratio))) +
  geom_col(fill = "#3691d1") +
  labs(title = "Stor forskellen mellem brug af ord i de forskellige emner",
       subtitle = "Ord med de største forskelle mellem de to emner") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        text = element_text(family = "Verdana"))

