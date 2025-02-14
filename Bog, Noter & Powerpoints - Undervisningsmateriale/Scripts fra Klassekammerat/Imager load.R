library(imager)

# Load billeder
test <- load.image("Untitleddesign_41.png")

# Se info om billede, hvis depth > 1 = video, colour channels = 3 = farve billede
test

# Colour channel =  1 når graysacle
grayscale(test)

# Værdier for info - alle data om billeder er tal
# bredde, højde, dybde (altid 1 hvis billede), farve-kanaler (3, hvis farver, 1 hvis gråskala)
dim(test)

plot(test)

# Checker farver for første pixcel oppe venstre, og oppe højre
# R, G, B, A
test[1,1,,]*255
test[1024,1,,]*255

# A = 0, da pixel er gennemsigtig
parrot <- load.image("parrot.png")
parrot[1,2,,]


# R, G, B - hvis billede er jpg
test2 <- load.image("Monster-Energy-Ultra-White.jpg")
test2[1,1,]*255

# Laver om til dataframe
# x, y, cc, value
billede_df <- as.data.frame(test)

# x pixel, y pixel, cc = 1,2,3,4 (255, 255, 255, 255), value = værdien af cc
colnames(billede_df)

# Indlæser billede kun med en farve
blue_image <- load.image("blue.png")

# Finder farve på første pixel (burde meget gerne være blå)
blue_image[1,1,,]*255

