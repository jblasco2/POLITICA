library(tm)
library(quanteda)
library(readtext)
#leemos archivos del directorio elegido
textos <- readtext("~/Desktop/politica/corpus")
cor_textos <- corpus(textos)
summary(cor_textos, 4)
#Grafico
tokeninfo <- summary(cor_textos)
if (require(ggplot2))
  ggplot(data = tokeninfo, aes(x = Text, y = Types, group = 1)) +
  geom_line() +
  geom_point() 
##################################
# Unimos varias obras de un corpus que son del mismo autor
corp1 <- corpus(cor_textos[1:3])
corp2 <- corpus(cor_textos [4:6])
corp3 <- corpus(cor_textos [7])
corp4 <- corp1 + corp2 + corp3
summary(corp4)
#########################################

#Buscamos en el corpus una palabra  y los contextos en que aparece
kwic(cor_textos, pattern = "nacional")
#Buscamos en el corpus una palabra y sus derivados
kwic(cor_textos, pattern = "nacional", valuetype = "regex")

#Tokenizar
dfmat_cor_textos <- dfm(cor_textos)
dfmat_cor_textos[, 1:8]

#Removemos puntuación y palabras vacías
dfmat_cor_textos <- dfm(dfmat_cor_textos,
                            remove = stopwords("es"),
                            stem = TRUE, remove_punct = TRUE)
dfmat_cor_textos[, 1:4]

#Ver las palabras más frecuentes <- dfm(data_char_ukimmig2010, remove = stopwords("spanish"), remove_punct = TRUE)
cor_textos_limpio <- dfm(cor_textos, remove = stopwords("es"), remove_punct = TRUE)
cor_textos_limpio
topfeatures(cor_textos_limpio, 20) # 20 most frequent words

#Gráfico en nube de palabras
set.seed(100)
textplot_wordcloud(cor_textos_limpio, min_freq = 6, random_order = FALSE,
                   rotation = .25,
                   colors = RColorBrewer::brewer.pal(8, "Dark2"))


#Queremos observar cómo palabras asociadas a la religión y palabras 
#asociadas con la economía varían por obra en el corpus 
#Creamos diccionario
dict <- dictionary(list(derecha = c("nacional", "ley", "España", "seguridad"),
                        izquierda = c("trabajo", "derechos", "mujeres", "social")))
#Lo aplicamos a los textos del corpus
dfmat_cor_textos_dict <- dfm(cor_textos, dictionary = dict)
dfmat_cor_textos_dict

#Diccionario de LIWC
dictliwc <- dictionary(file = "~/Desktop/diccionarios/Spanish_LIWC2021_Dictionary.dic",
                       format = "LIWC")
dfmat_cor_textos <- dfm(cor_textos[1:7], dictionary = dictliwc)
dfmat_cor_textos[, 33:46]
