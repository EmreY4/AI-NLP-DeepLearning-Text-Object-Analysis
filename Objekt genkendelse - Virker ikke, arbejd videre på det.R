#### ---------------------------------------------------- Machine Learning: Objekt Genkendelse ------------------------------------------------------------ ####  
# Emne: Identifikation og klassificering af objekter i billeder  
# Dette script demonstrerer, hvordan man:  
#   1. Indlæser og forbehandler billeddata til brug i en maskinlæringsmodel  
#   2. Anvender billedbehandlingsteknikker såsom beskæring, lysjustering og gråskalakonvertering  
#   3. Opdeler billeddata i trænings- og testdatasæt for modelvalidering  
#   4. Opbygger en konvolutionel neuralt netværk (CNN) til billedklassifikation  
#   5. Træner modellen ved hjælp af binær klassifikation og evaluerer dens præstation  
#   6. Visualiserer resultater ved at vise billeder med forudsigelser  

# Libraries  
library(dplyr)        # Data manipulation  
library(tidyr)        # Datatransformation  
library(tidyverse)    # Datastrukturering og analyse  
library(ggplot2)      # Visualisering af data  
library(imager)       # Billedbehandling  
library(abind)        # Kombinering af arrays  
library(keras)        # Deep learning framework  

# ------------------------------------------------------- SCIPT NEEDS ATTENTION - DOES NOT WORK YET ----------------------------------------------------------- #
# Funktion til at vise billeder med ggplot2
show_images_with_predictions <- function(data) {
  data %>%
    mutate(img = map(file_path, image_read)) %>%
    ggplot(aes(x = 1, y = 1)) +
    geom_tile(aes(fill = as.factor(prediction))) +  # Konverter prediction til en faktor
    facet_wrap(~prediction, scales = 'free') +
    theme_void() +
    theme(legend.position = 'none')
}

# Funktion til at forbehandle et enkelt billede
preprocess_image <- function(image_path) {
  # Indlæs billedet fra stien
  image <- load.image(image_path)
  
  # Beskær billedet til den øverste halvdel
  height <- dim(image)[1]
  image <- crop.borders(image, 0, height / 2, 0, 0)  # Beskær til øverste halvdel
  
  # Juster lysstyrken
  image <- image * 1.5  # Øg lysstyrken med 50%
  
  # Konverter til gråskala
  if (length(dim(image)) > 2) {
    image <- grayscale(image)  # Bruger grayscale funktion fra imager
  }
  
  # Resize billedet til 28x28
  image <- resize(image, 28, 28)
  
  # Normaliser pixelværdierne
  image <- as.array(image) / 255
  
  # Reshape til model input format (1, 28, 28, 1)
  array_reshape(image, c(1, 28, 28, 1))
}

# Indstil arbejdsmapper
flag_dir <- "flag"
udenflag_dir <- "udenflag"

# Få lister over billedfiler i hver mappe
image_flag <- list.files(flag_dir, pattern = "\\.(jpg|png)$", full.names = TRUE)
image_udenflag <- list.files(udenflag_dir, pattern = "\\.(jpg|png)$", full.names = TRUE)

# Kombiner listerne af billedstier
all_image_files <- c(image_flag, image_udenflag)

# Opret labels: 1 for 'flag' billeder, 0 for 'udenflag' billeder
labels <- c(rep(1, length(image_flag)), rep(0, length(image_udenflag)))

# Anvend funktionen på alle billeder
processed_images <- lapply(all_image_files, preprocess_image)

# Kombiner alle billeder i en enkelt array
image_array <- abind::abind(processed_images, along = 1)

# Opret et dataframe med filstier og labels
image_data <- data.frame(
  file_path = all_image_files,
  label = labels,
  stringsAsFactors = FALSE
)

# Opdel data i trænings- og testdatasæt
set.seed(123)  # For reproducerbarhed
train_indices <- sample(1:nrow(image_data), size = 0.8 * nrow(image_data))
test_indices <- setdiff(1:nrow(image_data), train_indices)
train_data <- image_data[train_indices, ]
test_data <- image_data[test_indices, ]

# Forbehandl billederne i trænings- og testdatasættet
x_train <- abind::abind(lapply(train_data$file_path, preprocess_image), along = 1)
y_train <- train_data$label
x_test <- abind::abind(lapply(test_data$file_path, preprocess_image), along = 1)
y_test <- test_data$label

# Byg og træn modellen
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(28, 28, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')  # Binær klassifikation

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)

# Træn model
history <- model %>% fit(x_train, y_train, epochs = 10, batch_size = 5)

# Evaluer modellen på testdatasættet
evaluation <- model %>% evaluate(x_test, y_test)
cat("Test Loss:", evaluation[1], "\n")
cat("Test Accuracy:", evaluation[2], "\n")

# Forudsig på testdata
predictions <- model %>% predict(x_test) %>% `>`(0.5) %>% k_cast("int32")

# Vis billeder med forudsigelser
test_data$prediction <- as.vector(predictions)
test_data <- test_data %>% mutate(prediction = ifelse(prediction == 1, 'Flag', 'Uden Flag'))
show_images_with_predictions(test_data)
