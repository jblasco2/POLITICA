####################################################################
##### COLOCACIONES Y CORRELACIONES ###########
####################################################################

#ESTABLECEMOS DIRECTORIO
setwd("~/Desktop/politica/corpus2")
library(tidyverse)
library(tidytext)

#Creamos una tabla
ficheros <- list.files(path ="~/Desktop/politica/corpus2", pattern = ".txt")
ficheros
txts <- as.character(c(1:3))
titulo <- c(rep("Derecha", 1),rep("Izquierda", 1),rep("PNV", 1))
novelas <- tibble(txts = character(),
                   titulo = character(),
                   parrafo = numeric(),
                   texto = character())
for (i in 1:length(ficheros)){
  textos <- readLines(paste("~/Desktop/politica/corpus2",
                              ficheros[i],
                              sep = "/"))
  temporal <- tibble(txts = txts[i],
                     titulo = titulo[i],
                     parrafo = seq_along(textos),
                     texto = textos)
  novelas <- bind_rows(novelas, temporal)
}
novelas$titulo <- factor(novelas$titulo, levels = c("Derecha", "Izquierda", "PNV"))
novelas$txts <- factor(novelas$txts)
#Lo siguiente es cargar el fichero de palabras vacías 
vacias <- read_tsv("https://tinyurl.com/7PartidasVacias",
                   locale = default_locale())

#Dividir en bigramas o trigramas el texto
novelas %>%
  unnest_tokens(palabra, texto)
# Dividimos en bigramas, o trigramas
novelas %>%
  unnest_tokens(bigrama,
                texto,
                token = "ngrams",
                n = 2)
#Contabilizamos los n-gram 
novelas_bigramas <- novelas %>%
  unnest_tokens(bigrama,
                texto,
                token = "ngrams",
                n = 2)
novelas_bigramas %>%
  count(bigrama, sort = T)
# Nueva tabla de bigrams sin palabras de función
bigramas_separados <- novelas_bigramas %>%
  separate(bigrama,
           c("palabra1", "palabra2"),
           sep = " ")
bigramas_filtrados <- bigramas_separados %>%
  filter(!palabra1 %in% vacias$palabra,
         !palabra2 %in% vacias$palabra)
bigramas_filtrados
#Contamos
bigramas_filtrados %>%
  count(palabra1, palabra2, sort = T)

#Borramos todo lo que puede ser formulismo de cortesía, nombres propios, etc.
bigramas_filtrados <- bigramas_filtrados %>%
  filter(!palabra1 %in% c("3", "16"),
         !palabra2 %in% c("2019", "400", "30", "p.ej", "12", "2"))
# reunir de nuevo los dos elementos de cada bigrama.
bigramas_unidos <- bigramas_filtrados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")
#Contamos
bigramas_unidos %>%
  count(titulo, bigrama, sort = T)
#Hacemos un gráfico
bigramas_unidos %>%
  count(titulo, bigrama, sort = T) %>%
  group_by(titulo) %>%
  top_n(10) %>%
  ggplot() +
  geom_col(aes(y = n , x = reorder(bigrama,n)),
           fill = "maroon") +
  coord_flip() +
  facet_wrap(~ titulo, ncol = 2, scales = "free") +
  theme_linedraw() + 
  labs(x = "Bigramas", y = "Frecuencia") + 
  ggtitle("Bigramas más frecuentes", subtitle = "novelas")

#relaciones que existen entre las palabras que constituyen un corpus en grafos
# Poner en marcha las librer´ías
library(igraph)
library(ggraph)
library(grid)
#un recuento de los bigramas filtrados y limpios
recuento_bigramas <- bigramas_filtrados %>%
  count(palabra1, palabra2, sort = T)
recuento_bigramas
#Creamos una lista para el grafo
grafo_bigramas <- recuento_bigramas %>%
  filter(n > 15) %>%
  graph_from_data_frame()
#Dibujamos el grafo
ggraph(grafo_bigramas, layout = "nicely") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
######COOCURRENCIAS#############
#Instalamos nuevo paquete para coocurrencias y cargamos librería
library(widyr)
#Trabajamos sólo con una novela y eliminamos palabras vacías
novelas_titulo <- novelas %>%
  filter(titulo =="Izquierda") %>%
  mutate(seccion = row_number()) %>%
  unnest_tokens(palabra, texto) %>%
  filter(!palabra %in% vacias$palabra)
novelas_titulo
#Dividmos en pares de palabras y contamos
pares_palabras <- novelas_titulo %>%
  pairwise_count(palabra,
                 seccion,
                 sort = T)
pares_palabras
##Borramos todo lo que puede ser formulismo de cortesía, nombres propios, etc.
pares_filtrados <- pares_palabras %>%
  filter(!item1 %in% c("2019", "3", "16"),
         !item2 %in% c("2019", "4", "2015", "400", "30", "p.ej", "12", "2"))
pares_filtrados
#Podemos ver, palabra por palabra, de cuales se acompaña con mayor frecuencia
pares_palabras %>%
  filter(item1 == "españa")
# Porcentaje phi (coeficiente de correlación, entre 0 (nada) y 1(absoluto)): 
# examinar la correlación que existe entre las palabras, lo cual te 
#indicará cuán a menudo aparecen juntas con referencia a cuántas veces 
#están separadas
palabras_correlacion <- novelas_titulo %>%
  group_by(palabra) %>%
  filter(n() >= 5) %>%
  pairwise_cor(palabra,
               seccion,
               sort = TRUE)
palabras_correlacion
#examinar cuáles son las palabras que pueden aparecer más correlacionadas
palabras_correlacion %>%
  filter(item1 == "ley")
#Obtenemos un gráfico
palabras_correlacion %>%
  filter(item1 %in% c("social",
                      "ley",
                      "sistema",
                      "libertad",
                      "derecho",
                      "españa")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()
#Podemos hacer un grafo
palabras_correlacion %>%
  filter(correlation > .55) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "nicely") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
