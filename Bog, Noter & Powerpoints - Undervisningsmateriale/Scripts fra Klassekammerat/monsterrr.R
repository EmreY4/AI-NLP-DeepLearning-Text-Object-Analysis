library(tidyverse)
library(tensorflow)
library(keras3)
library(imager)

# Set seed for at kunne genskabe resultater
set.seed(123)

data_dir <- "/Users/marius/Documents/DataAnalyseProjekter/NLP & Deep-Learning/NLP & Deep-Leaning R/billeder_cnn"

batch_size <- 9
img_height <- 64
img_width <- 64

# Indlæser billeder
med_monner_list <- list.files(path = "billeder_cnn/med_monner", pattern = "*.jpg", full.names = TRUE)
uden_monner_list <- list.files(path = "billeder_cnn/uden_monner", pattern = "*.jpg", full.names = TRUE)

# Loader billeder
med_monner <- lapply(med_monner_list, load.image)
uden_monner <- lapply(uden_monner_list, load.image)

# Sætter etiketter
med_monner_etiketter <- rep(1, length(med_monner))
uden_monner_etiketter <- rep(0, length(uden_monner))

# Saml alle billeder og etiketter
billeder <- c(med_monner, uden_monner)
etiketter <- c(med_monner_etiketter, uden_monner_etiketter)

# Resizer billeder
for (i in 1:length(billeder)) {
  billeder[[i]] <- resize(im = billeder[[i]], size_x = 64, size_y = 64)
}


