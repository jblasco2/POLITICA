## Creamos el corpus de la siguiente manera: dos carpetas ('primary_set' y 'secondary_set')
## En la primera (primary_set) colocamos todos los textos de un autor.
## En la segunda (secondary_set) colocamos los textos del segundo autor
## Procedemos a comparar los léxicos

# Establecemos directorio de trabajo
setwd("~/Desktop/POLITICA/corpus_oppose")

#Cargamos 'stylo'
library(stylo)

# Ejecutamos la función
oppose(gui = TRUE, training.frequencies = NULL, 
         test.frequencies = NULL,
         training.corpus.dir = "primary_set", 
         test.corpus.dir = "secondary_set", 
         features = NULL, 
         path = NULL)


