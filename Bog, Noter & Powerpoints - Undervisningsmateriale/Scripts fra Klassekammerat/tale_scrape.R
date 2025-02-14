library(tidyverse)
library(rvest)
library(RSelenium)

# Starter Selenium driver
rD <- rsDriver(browser = "firefox", port = 4546L)
remDr <- rD$client

# Webscrape - lav liste med links til 1. maj taler
base_url <- "https://www.dansketaler.dk/soeg?tag=1.+maj-tale"
antal_sider <- 187

# Åbner side
remDr$navigate(base_url)

# Scroller til slutningen af siden
for (i in 1:(ceiling(antal_sider/20))) {
  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(2)
}


webElems <- remDr$findElements(using = "xpath", value = "//a[@class='special']")
hrefs <- lapply(webElems, function(elem) {
  elem$getElementAttribute("href")
})

# Webscrape - besøg hvert link og hent tekst

speech_list <- list()

for (i in 1:length(hrefs)) {
  url <- hrefs[[i]][[1]]
  remDr$navigate(url)
  element <- remDr$findElement(using = "class", value = "speech-article-content")
  text <- element$getElementText()
  speech_list[[i]] <- text
  Sys.sleep(0.5)
  print(i)
}

