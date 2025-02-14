library(tensorflow)
library(ISLR2)
library(glmnet)
library(keras3)
library(tidyverse)
### Laver model til at forudsige løn til spillere
# Indhenter data
Gitters <- na.omit(Hitters)
n <- nrow(Gitters)

set.seed (13)

# Laver testdata
ntest <- trunc(n / 3)
testid <- sample(1:n, ntest)

# Laver lineær model 
lfit <- lm(Salary ~., data = Gitters[-testid, ])
lpred <- predict(lfit, Gitters[testid, ])

# Calculates the mean absolute error between the predicted and actual values
# For at bedømme modellen - fejlleddet
with(Gitters[testid, ], mean(abs(lpred - Salary)))

# Definerer x og y værdier
x <- scale(model.matrix(Salary ~ . - 1, data = Gitters))
y <- Gitters$Salary

# Laver model nr 2.
cvfit <- cv.glmnet(x[-testid , ],
                   y[-testid],
                   type.measure = "mae")

# Beregner fejl
cpred <- predict(cvfit , x[testid , ], s = "lambda.min")
mean(abs(y[testid] - cpred))

## Størrer fejlled end lineær model

### Laver modellen med nueral netværk
# Laver model strukturen
modnn <- keras_model_sequential() %>%
  # Define the input layer with input shape
  # 50 neuroner
  # input_shape = vores x værdier
  layer_dense(units = 50, activation = "relu", input_shape = ncol(x)) %>%
  # Add dropout layer
  # Fjerner tilfældigt 40% af neuroner
  layer_dropout(rate = 0.4) %>%
  # Add output layer
  # 1 output
  layer_dense(units = 1)

modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop(),
                  metrics = list("mean_absolute_error"))

history <- modnn %>% fit(
  x[-testid, ], y[-testid], epochs = 1500, batch_size = 32, validation_data = list(x[testid, ], y[testid])
)

# Plotter vejlled
plot(history)

# Tester modellen 
npred <- predict(modnn , x[testid , ])
mean(abs(y[testid] - npred))


