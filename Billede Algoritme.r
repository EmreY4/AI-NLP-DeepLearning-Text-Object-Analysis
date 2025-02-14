#### -------------------------------------------------------- Billedgenkendelse og Klassificering --------------------------------------------------------- ####  
# Emne: Behandling, genkendelse og klassificering af billeder
# Dette script demonstrerer, hvordan man:  
#   1. Læser billeder fra mappen "Billede Genkendelse" 
#   2. Brug af `imager` og `jpeg` pakkerne til at indlæse og analysere billeder  
#   3. Manuelt definerer billednavnene og læser disse billeder ind i R  
#   4. Kombinerer billederne i en samlet datastruktur og tilføjer labels 
#   5. Visualiserer billederne for at få en fornemmelse af dataene  
#   6. Opdeler billederne i trænings- og testdatasæt for senere maskinlæringsmodeller  
#   7. Forbereder billederne til brug i en klassifikationsmodel, f.eks. et neuralt netværk eller en maskinlæringsalgoritme
#   8. Udarbejder maskinlæringsalgoritme til at genkende forskelle på billeder

devtools::install_github("rstudio/tensorflow")
install_tensorflow()
install_keras()
#rm(npred, mae, pred_df, score, img, history, prediction, x, y, trainid, testid, modnn, train_images, train_labels, test_images, test_labels, class_names, image_1, model)
#install.packages("keras")
#devtools::install_github("rstudio/keras")
#remove.packages("keras3")

# Libraries
library(tensorflow)   # TensorFlow R binding, en populær deep learning platform, bruges til at bygge og træne neurale netværk.
library(devtools)     # Devtools hjælper med at udvikle, teste og installere R-pakker, og gør det nemt at hente pakker fra GitHub.
library(tidyverse)    # Tidyverse er en samling af pakker, der bruges til data manipulation, transformation og visualisering, herunder 'dplyr', 'ggplot2', 'tidyr', etc.
library(keras)        # Keras R-binding til Keras, et brugervenligt deep learning API, der gør det muligt at bygge og træne komplekse modeller.
library(tidyr)        # Tidyr er en del af tidyverse og bruges til at transformere data mellem forskellige formater, især til at arbejde med "wide" og "long" datastrukturer.
library(ggplot2)      # ggplot2 er en af de mest populære pakker til datavisualisering i R, og bruges til at skabe elegante og informative grafikker.
library(imager)       # Imager bruges til billedbehandling og manipulation, og tilbyder funktioner til at håndtere, analysere og bearbejde billeder.
library(jpeg)         # JPEG-pakken giver funktioner til at læse og skrive JPEG-billedfiler i R.
library(class)        # class-pakken indeholder klassifikationsalgoritmer som k-Nearest Neighbors (kNN), som bruges til at klassificere data baseret på nærliggende observationer.
library(e1071)        # e1071 indeholder funktioner til maskinlæring, herunder SVM (Support Vector Machines), som bruges til klassifikation og regression.

# ---------------------------------------------------------- Billede Genkendelse - Egne billeder ------------------------------------------------------------ #
# Definer mappen med billederne af heste og hunde
base_dir <- "Billede Genkendelse" # "Billede Genkendelse er navnet på mappen med billederne
# Opret mapperne for træning, validering og test
train_dir <- file.path(base_dir, "train")
dir.create(train_dir, showWarnings = FALSE, recursive = TRUE)
validation_dir <- file.path(base_dir, "validation")
dir.create(validation_dir, showWarnings = FALSE)
test_dir <- file.path(base_dir, "test")
dir.create(test_dir, showWarnings = FALSE)
# Opret mapperne for heste og hunde i hver dataset
train_horses_dir <- file.path(train_dir, "horses")
dir.create(train_horses_dir, showWarnings = FALSE)
train_dogs_dir <- file.path(train_dir, "dogs")
dir.create(train_dogs_dir, showWarnings = FALSE)
validation_horses_dir <- file.path(validation_dir, "horses")
dir.create(validation_horses_dir, showWarnings = FALSE)
validation_dogs_dir <- file.path(validation_dir, "dogs")
dir.create(validation_dogs_dir, showWarnings = FALSE)
test_horses_dir <- file.path(test_dir, "horses")
dir.create(test_horses_dir, showWarnings = FALSE)
test_dogs_dir <- file.path(test_dir, "dogs")
dir.create(test_dogs_dir, showWarnings = FALSE)

# Funktionen til at flytte filer
move_files <- function(file_list, source_dir, target_dir) {
  for (file in file_list) {
    file.copy(file.path(source_dir, file), file.path(target_dir, file))
  }
}
# Liste over billeder i hovedmappen
all_files <- list.files(base_dir)
# Manuelt opdel billederne
horse_files <- c("Billede Genkendelse - 1.jpeg",
                 "Billede Genkendelse - 2.jpeg",
                 "Billede Genkendelse - 3.jpeg",
                 "Billede Genkendelse - 4.jpeg",
                 "Billede Genkendelse - 5.jpeg",
                 "Billede Genkendelse - 6.jpeg",
                 "Billede Genkendelse - 7.jpeg")
dog_files <- c("Billede Genkendelse - 8.jpeg",
               "Billede Genkendelse - 9.jpeg",
               "Billede Genkendelse - 10.jpeg",
               "Billede Genkendelse - 11.jpeg",
               "Billede Genkendelse - 12.jpeg",
               "Billede Genkendelse - 13.jpeg",
               "Billede Genkendelse - 14.jpeg")
# Opdel hestebilleder
horse_split <- list(
  train = horse_files[1:5],
  val = horse_files[6],
  test = horse_files[7]
)
move_files(horse_split$train, base_dir, train_horses_dir)
move_files(horse_split$val, base_dir, validation_horses_dir)
move_files(horse_split$test, base_dir, test_horses_dir)
# Opdel hundebilleder
dog_split <- list(
  train = dog_files[1:5],
  val = dog_files[6],
  test = dog_files[7]
)
move_files(dog_split$train, base_dir, train_dogs_dir)
move_files(dog_split$val, base_dir, validation_dogs_dir)
move_files(dog_split$test, base_dir, test_dogs_dir)

# Print for at bekræfte opdeling
cat("Training set:\n")
cat("Horses:", length(list.files(train_horses_dir)), "\n")
cat("Dogs:", length(list.files(train_dogs_dir)), "\n")
cat("Validation set:\n")
cat("Horses:", length(list.files(validation_horses_dir)), "\n")
cat("Dogs:", length(list.files(validation_dogs_dir)), "\n")
cat("Test set:\n")
cat("Horses:", length(list.files(test_horses_dir)), "\n")
cat("Dogs:", length(list.files(test_dogs_dir)), "\n")


# Definer data-generatorer til træning, validering og test
# Opret en billeddata-generator til træning med reskalering, rotation, zoom og vandret spejling
train_datagen <- image_data_generator(
  rescale = 1/255,            # Reskalér billederne for at normalisere pixelværdierne
  shear_range = 0.2,          # Tilføj skæringstransformation
  zoom_range = 0.2,           # Tilføj zoom-transformation
  horizontal_flip = TRUE     # Tilføj vandret spejling
)
# Opret en billeddata-generator til test med reskalering
test_datagen <- image_data_generator(rescale = 1/255)

# Opret en generator til træningsbilleder
train_generator <- flow_images_from_directory(
  train_dir,                     # Sti til træningsbillederne
  train_datagen,                 # Data-generator til træning
  target_size = c(150, 150),     # Justér billedstørrelsen til 150x150 pixels
  batch_size = 32,               # Batch-størrelse på 32 billeder
  class_mode = "binary"          # Da det er en binær klassificering, brug 'binary' class_mode
)
# Opret en generator til valideringsbilleder
validation_generator <- flow_images_from_directory(
  validation_dir,                # Sti til valideringsbillederne
  test_datagen,                  # Data-generator til validering
  target_size = c(150, 150),     # Justér billedstørrelsen til 150x150 pixels
  batch_size = 32,               # Batch-størrelse på 32 billeder
  class_mode = "binary"          # Da det er en binær klassificering, brug 'binary' class_mode
)
# Opret en generator til testbilleder
test_generator <- flow_images_from_directory(
  test_dir,                      # Sti til testbillederne
  test_datagen,                  # Data-generator til test
  target_size = c(150, 150),     # Justér billedstørrelsen til 150x150 pixels
  batch_size = 32,               # Batch-størrelse på 32 billeder
  class_mode = "binary"          # Da det er en binær klassificering, brug 'binary' class_mode
)

# Byg den neurale netværksmodel
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = 'relu', input_shape = c(150, 150, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 512, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')
# Kompiler modellen
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(learning_rate = 1e-4),
  metrics = c('accuracy')
)
# Træn modellen
history <- model %>% fit(
  train_generator,
  steps_per_epoch = 1,  # Juster dette til det rigtige antal for din træningsdata
  epochs = 30,
  validation_data = validation_generator,
  validation_steps = 1
)
# plot
plot(history)

# Evaluer modellen på testdata
evaluation <- model %>% evaluate(test_generator, steps = 1)
test_loss <- evaluation[[1]]
test_acc <- evaluation[[2]]
# Resultater
cat("Test Loss:", test_loss, "\n")
cat("Test Accuracy:", test_acc, "\n")

####
# Få forudsigelser fra testdata
predictions <- model %>% predict(test_generator, steps = 1)
print(predictions)

# Hent billeder fra testgenerator
test_data <- flow_images_from_directory(
  test_dir,
  test_datagen,
  target_size = c(150, 150),
  batch_size = 32,
  class_mode = "binary",
  shuffle = FALSE  # Undgå at ændre rækkefølgen af billeder
)

# Opret plot
par(mfrow=c(4, 4), mar=c(0, 0, 1, 0))  # Juster antal rækker og kolonner efter antallet af billeder
for (i in 1:length(test_data$filenames)) {
  filename <- test_data$filenames[i]  # Hent filnavnet
  full_path <- file.path(test_dir, filename)  # Byg den fulde sti til billedet
  image <- readJPEG(full_path)  # Læs billedet
  
  label <- test_data$classes[i]    # Hent den faktiske klasse (0 eller 1)
  prediction <- predictions[i]  # Hent forudsigelsen (en sandsynlighed)
  
  # Fremhæv korrekte forudsigelser
  if ((label == 0 && prediction < 0.5) || (label == 1 && prediction >= 0.5)) {
    border_color <- "green"  # Korrekt forudsigelse
  } else {
    border_color <- "red"    # Forkert forudsigelse
  }
  
  # Plot billedet med forudsigelsen som titel
  plot(as.raster(image), main = paste("Prediction:", round(prediction, 2)), border = border_color)
}
####

# ------------------------------------------------------------ Billede Genkendelse fra Bogen ---------------------------------------------------------------- #
# Dummy data (x og y) for at illustrere koden
set.seed(123)
x <- matrix(runif(1000), ncol = 10)
y <- rnorm(100)

# Del data i træning og test
trainid <- sample(1:nrow(x), 0.7 * nrow(x))
testid <- setdiff(1:nrow(x), trainid)

# Opret en simpel neural netværksmodel
modnn <- keras_model_sequential() %>% 
  layer_dense(units = 50, activation = "relu", input_shape = ncol(x)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 1)
# Kompiler modellen
modnn %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy'
)
# Træn modellen og gem historikken
history <- modnn %>% fit(
  x[trainid, ],
  y[trainid],
  epochs = 100,
  validation_split = 0.2,
  verbose = 0
)
# Plot træningshistorikken
plot(history)

# Lav forudsigelser på testdata
npred <- predict(modnn, x[testid, ])
mean(abs(y[testid] - npred))
# Beregn gennemsnitlig absolut fejl
mae <- mean(abs(y[testid] - npred))
cat('Mean Absolute Error:', mae, "\n")

# Plot forudsigelser vs. faktiske værdier
pred_df <- data.frame(True = y[testid], Predicted = npred)
ggplot(pred_df, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  labs(title = "Forudsigelser vs. Faktiske værdier",
       x = "Faktiske værdier",
       y = "Forudsigelser") +
  theme_minimal()

# -------------------------------------------------------------------- TensorFlow Guide --------------------------------------------------------------------- #
# Terminal - git clone git@github.com:zalandoresearch/fashion-mnist.git
# Hent Fashion MNIST datasættet
fashion_mnist <- dataset_fashion_mnist()

# Split datasættet i trænings- og test-sæt
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test
# Definer klassens navne for Fashion MNIST datasættet
class_names = c('T-shirt/top',
                'Bukser',
                'Pullover',
                'Kjole',
                'Frakke',
                'Sandal',
                'Skjorte',
                'Sneaker',
                'Taske',
                'Ankelstøvle')
# Vis dimensionerne af træningsbillederne
dim(train_images)
# Vis dimensionerne af træningslabels
dim(train_labels)
# Vis de første 20 træningslabels
train_labels[1:20]
# Vis dimensionerne af testbillederne
dim(test_images)
# Vis dimensionerne af testlabels
dim(test_labels)

# Konverter det første træningsbillede til en data frame
image_1 <- as.data.frame(train_images[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

# Plot det første træningsbillede
ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")

# Normaliser billederne ved at skalere pixelværdierne til intervallet [0, 1]
train_images <- train_images / 255
test_images <- test_images / 255

# Plot de første 25 billeder i træningssættet
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) {
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev))
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}

# Opret en sekventiel model
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')
# Kompiler modellen
model %>% compile(
  optimizer = 'adam',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)
# Træn modellen med træningsdata i 5 epoker
model %>% fit(train_images, train_labels, epochs = 5, verbose = 2)

# Evaluér modellen med testdata
score <- model %>% evaluate(test_images, test_labels, verbose = 0)
# Print tab på testdata
cat('Test loss:', score[["loss"]], "\n")
# Print nøjagtighed på testdata
cat('Test accuracy:', score[["accuracy"]], "\n")

# Lav forudsigelser på testdata
predictions <- model %>% predict(test_images)
# Vis forudsigelser for det første testbillede
predictions[1, ]
# Find klassen med den højeste sandsynlighed for det første testbillede
which.max(predictions[1, ])
# Vis den sande label for det første testbillede
test_labels[1]

# Plot de første 25 testbilleder med forudsigelser og sande labels
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) {
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev))
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800'  # Grøn for korrekt forudsigelse
  } else {
    color <- '#bb0000'  # Rød for forkert forudsigelse
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}

# Tag et billede fra testdatasættet, behold batch-dimensionen
img <- test_images[1, , , drop = FALSE]
dim(img)

# Lav forudsigelser på dette billede
predictions <- model %>% predict(img)
# Vis forudsigelserne
predictions
# Find klassen med den højeste sandsynlighed
prediction <- predictions[1, ] - 1
which.max(prediction)

# -------------------------------------- TensorFlow - Classification from scratch - Opskrift fra tensorflow SKAL RETTES ------------------------------------- #
set.seed(1234)

url <- "https://download.microsoft.com/download/3/E/1/3E1C3F21-ECDB-4869-8368-6DEBA77B919F/kagglecatsanddogs_5340.zip"
options(timeout = 60 * 5) # 5 minutes
download.file(url, destfile = "kagglecatsanddogs_5340.zip") # (786.7 MB)
## To see a list of everything in the zip file:
# zip::zip_list("kagglecatsanddogs_5340.zip") |> tibble::as_tibble()
zip::unzip("kagglecatsanddogs_5340.zip")
#
fs::dir_info("PetImages")
#
fs::dir_info("PetImages", recurse = TRUE)
#
n_deleted <- 0L
for(filepath in list.files("PetImages", pattern = "\\.jpg$",
                           recursive = TRUE, full.names = TRUE)) {
  header <- readBin(filepath, what = "raw", n = 10)
  if(!identical(header[7:10], charToRaw("JFIF"))) {
    n_deleted <- n_deleted + 1L
    unlink(filepath)
  }
}
cat(sprintf("Deleted %d images\n", n_deleted))
#
image_size <- c(180, 180)
batch_size <- 32
train_ds <- image_dataset_from_directory(
  "PetImages",
  validation_split = 0.2,
  subset = "training",
  seed = 1337,
  image_size = image_size,
  batch_size = batch_size,
)
val_ds <- image_dataset_from_directory(
  "PetImages",
  validation_split = 0.2,
  subset = "validation",
  seed = 1337,
  image_size = image_size,
  batch_size = batch_size,
)
#
batch <- train_ds %>%
  as_iterator() %>%
  iter_next()

str(batch)
#
c(images, labels) %<-% batch
#
display_image_tensor <- function(x, ..., max = 255,
                                 plot_margins = c(0, 0, 0, 0)) {
  if(!is.null(plot_margins))
    par(mar = plot_margins)
  
  x %>%
    as.array() %>%
    drop() %>%
    as.raster(max = max) %>%
    plot(..., interpolate = FALSE)
}

par(mfrow = c(3, 3))
for (i in 1:9)
  display_image_tensor(images[i,,,],
                       plot_margins = rep(.5, 4))
#
# If you are on an M1 mac, you may need to wrap this model definition in
# with(tf$device("CPU"), { ... })
# https://stackoverflow.com/questions/69088577/apple-m1-i-got-no-registered-rngreadandskip-opkernel-for-gpu-devices-comp

data_augmentation <-
  keras_model_sequential(input_shape = c(image_size, 3)) %>%
  layer_random_flip("horizontal") %>%
  layer_random_rotation(factor = 0.1)
#
par(mfrow = c(3, 3))
for (i in 1:9) {
  images[4, , , , drop = FALSE] %>%
    data_augmentation() %>%
    display_image_tensor()
}
#
x <- layer_input(shape = input_shape) %>%
  data_augmentation() %>%
  layer_rescaling(1./255)
...  # Rest of the model
#
augmented_train_ds <- train_ds %>%
  dataset_map(function(x, y) {
    x <- data_augmentation(x, training = TRUE)
    list(x, y)
  })
#
# Apply `data_augmentation` to the training images.
train_ds <- train_ds %>%
  dataset_map(function(images, labels) {
    list(data_augmentation(images, training = TRUE),
         labels)
  })
# Prefetching samples in GPU memory helps maximize GPU utilization.
train_ds %<>% dataset_prefetch()
val_ds   %<>% dataset_prefetch()
#
# Modellen
make_model <- function(input_shape, num_classes) {
  
  inputs <- layer_input(shape = input_shape)
  
  x <- inputs %>%
    # data augmentation() ? %>%
    layer_rescaling(1.0 / 255)
  
  x <- x %>%
    layer_conv_2d(128, 3, strides = 2, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  previous_block_activation <- x  # Set aside residual
  for (size in c(256, 512, 728)) {
    x <- x %>%
      layer_activation("relu") %>%
      layer_separable_conv_2d(size, 3, padding = "same") %>%
      layer_batch_normalization() %>%
      
      layer_activation("relu") %>%
      layer_separable_conv_2d(size, 3, padding = "same") %>%
      layer_batch_normalization() %>%
      
      layer_max_pooling_2d(3, strides = 2, padding = "same")
    
    # Project residual
    residual <- previous_block_activation %>%
      layer_conv_2d(filters = size, kernel_size = 1, strides = 2,
                    padding = "same")
    
    x <- tf$keras$layers$add(list(x, residual))  # Add back residual
    previous_block_activation <- x  # Set aside next residual
  }
  
  x <- x %>%
    layer_separable_conv_2d(1024, 3, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_global_average_pooling_2d()
  
  if (num_classes == 2) {
    activation <- "sigmoid"
    units <- 1
  } else {
    activation <- "softmax"
    units <- num_classes
  }
  
  outputs <- x %>%
    layer_dropout(0.5) %>%
    layer_dense(units, activation = activation)
  
  return(keras_model(inputs, outputs))
}

model <- make_model(input_shape = c(image_size, 3), num_classes = 2)
# Plot
plot(model)
#
epochs <- 25

callbacks <- list(callback_model_checkpoint("save_at_{epoch}.keras"))
model %>% compile(
  optimizer = optimizer_adam(1e-3),
  loss = "binary_crossentropy",
  metrics = list("accuracy"),
)
history <- model %>% fit(
  train_ds,
  epochs = epochs,
  callbacks = callbacks,
  validation_data = val_ds,
)
plot(history)
#
model <- load_model_tf("save_at_25.keras")
#
# load an image as a tensor
img_tensor <-
  "PetImages/Cat/6779.jpg" %>%
  tf$io$read_file() %>%
  tf$io$decode_image() %>%
  tf$image$resize(as.integer(image_size)) %>%
  tf$expand_dims(0L)  # Create batch axis
score <- model %>% predict(img_tensor)
display_image_tensor(img_tensor)
#
sprintf("This image is %.2f%% cat and %.2f%% dog.", 100 * (1 - score), 100 * score)

# -------------------------------------------------------------- Billede Genkendelse fra Bogen -------------------------------------------------------------- #
# Indlæser pakker
library(tensorflow)
library(keras)
library(tidyverse)
library(jpeg)
library(ISLR2)
library(glmnet)

# Indlæser data
Gitters <- na.omit(Hitters)
# Seed
set.seed(13)
# Træning- og testdata
n <- nrow(Gitters)
ntest <- trunc(n / 3)
testid <- sample(1:n, ntest)

# Linear Regression
lfit <- lm(Salary ~ ., data = Gitters[-testid, ])
summary(lfit)
lpred <- predict(lfit, newdata = Gitters[testid, ])
linear_regression_mae <- mean(abs(lpred - Gitters$Salary[testid]))
# Lasso Regression
x <- scale(model.matrix(Salary ~ . - 1, data = Gitters))
y <- Gitters$Salary
cvfit <- cv.glmnet(x[-testid, ], y[-testid], type.measure = "mae")
cpred <- predict(cvfit, s = "lambda.min", newx = x[testid, ])
lasso_regression_mae <- mean(abs(cpred - Gitters$Salary[testid]))
# Neural Network Model (Keras)
modnn <- keras_model_sequential() %>% 
  layer_dense(units = 50, activation = "relu", input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

x <- scale(model.matrix(Salary ~ . - 1, data = Gitters))
x <- model.matrix(Salary ~ . - 1, data = Gitters) %>% scale ()

modnn %>% 
  compile(loss = "mse",
          optimizer = optimizer_rmsprop (),
          metrics = list("mean_absolute_error"))
history <- modnn %>% 
  fit(x[-testid , ], y[-testid], epochs = 1500, batch_size = 32,
      validation_data = list(x[testid , ], y[testid ]))
# plot
plot(history)

npred <- predict(modnn , x[testid , ])
mean(abs(y[testid] - npred))
