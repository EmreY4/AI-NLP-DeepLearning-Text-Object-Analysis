library(tensorflow)
library(keras)
library(tfdatasets)
set.seed(1234)

# Installer første gange
#install_tensorflow(method = c("auto", "virtualenv", "conda"),  conda = "auto",  version = "default",  envname = NULL,  extra_packages = NULL,  restart_session = TRUE,  conda_python_version = NULL,  pip_ignore_installed = TRUE,  python_version = "3.11.5")
#install_keras()

# Sætter directory til billeder
# I mappen skal der være 2 andre mapper "Med ubjekt" og "Uden objekt"
dir <- "/Users/marius/Documents/DataAnalyseProjekter/NLP & Deep-Learning/NLP & Deep-Leaning R/train_billeder2"
image_size <- c(300, 300)
# Batch_size skal være total antal af billeder
batch_size <- 16

# Laver træningsdataset
train_ds <- image_dataset_from_directory(
  dir,
  validation_split = 0.3,
  subset = "training",
  seed = 1337,
  image_size = image_size,
  batch_size = batch_size,
)

# Laver validationsdataset
val_ds <- image_dataset_from_directory(
  dir,
  validation_split = 0.3,
  subset = "validation",
  seed = 1337,
  image_size = image_size,
  batch_size = batch_size,
)

batch <- train_ds %>%
  as_iterator() %>%
  iter_next()

str(batch)

# Viser udvalg af billederne i R 

c(images, labels) %<-% batch

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

# Laver augmentation på billederne
data_augmentation <- keras_model_sequential(input_shape = c(image_size, 3)) %>%
  layer_random_flip("horizontal") %>%
  layer_random_rotation(factor = 0.1) %>%
  layer_random_zoom(height_factor = 0.2, width_factor = 0.2) %>%
  layer_random_contrast(factor = 0.2)

par(mfrow = c(3, 3))
for (i in 1:9) {
  images[4, , , , drop = FALSE] %>%
    data_augmentation() %>%
    display_image_tensor()
}

augmented_train_ds <- train_ds %>%
  dataset_map(function(x, y) {
    x <- data_augmentation(x, training = TRUE)
    list(x, y)
  })

# Apply `data_augmentation` to the training images.
train_ds <- train_ds %>%
  dataset_map(function(images, labels) {
    list(data_augmentation(images, training = TRUE),
         labels)
  })

# Prefetching samples in GPU memory helps maximize GPU utilization.
train_ds %<>% dataset_prefetch()
val_ds %<>% dataset_prefetch()


# Laver den første (store) model
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


# Laver den anden (Lille) model
modnn <- keras_model_sequential() %>%
  layer_dense(units = 50, activation = "relu", input_shape = c(image_size, 3)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 1)


# Antal gange modellen skal køre (træne sig selv)
epochs <- 50

callbacks <- list(callback_model_checkpoint("epocs/save_at_{epoch}.keras"))
model %>% compile(
  optimizer = tf$keras$optimizers$legacy$Adam(learning_rate = 1e-3),
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

model %>% predict(val_ds)
