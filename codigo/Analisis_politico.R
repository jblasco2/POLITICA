############################################################
###  ANÁLISIS 
## https://www.content-analysis-with-r.com/0-introduction.html
### https://quanteda.io/articles/pkgdown/replication/digital-humanities.html
## https://www.tidytextmining.com/topicmodeling.html
############################################################


# Poner en marcha las librerías
library(tm)
if(!require("quanteda")) {install.packages("quanteda"); library("quanteda")}
if(!require("readtext")) {install.packages("readtext"); library("readtext")}
if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if(!require("scales")) {install.packages("scales"); library("scales")}
if(!require("lubridate")) {install.packages("lubridate"); library("lubridate")}
if(!require("RColorBrewer")) {install.packages("RColorBrewer"); library("RColorBrewer")}
theme_set(theme_bw())


#Cargamos datos
politica <- readtext("~/Desktop/politica2/corpus/*.txt") 
politica$doc_id <- str_sub(politica$doc_id, start = 1, end = -1)
my.corpus <- corpus(politica)
docvars(my.corpus, "Textno") <- sprintf("%02d", 1:ndoc(my.corpus))
my.corpus


# Generamos estadísticas
my.corpus.stats <- summary(my.corpus)
my.corpus.stats$Text <- reorder(my.corpus.stats$Text, 1:ndoc(my.corpus), order = T)
my.corpus.stats


#Tokenización TOTAL
my.tokens <- tokens(my.corpus)
my.tokens

#Tokenización POR PARTIDOS
my.tokens <- tokens(my.corpus) %>% as.list()
head(my.tokens$`PSOE`, 10)


#Limpiar texto

#Quitar stopword
my.tokens <- tokens(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>% 
  tokens_remove(c(stopwords("spanish"), "electoral", "2019", "1", "elecciones", "psoe", "hacia")) %>% 
  as.list()
head(my.tokens)
head(my.tokens$`PSOE`, 20)

######
#Vemos palabras suprimidas
my.tokens <- tokens(my.corpus) 
  tokens_removed <- tokens_remove(my.tokens, c("electoral", "2019", "1", "elecciones", "psoe", "hacia"))  
head(tokens.removed$`PSOE`, 20)
######
#Creamos una matriz DMF y volvemos a quitar 
my.dfm <- dfm(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("spanish"))
my.dfm

#Contamos documentos
ndoc(my.dfm)

#Contamos palabras
nfeat(my.dfm)
#palabras que aparecen solo en muy pocos textos)
head(my.dfm, n = 6, nf = 10) 
# dfm ordenado
head(dfm_sort(my.dfm, decreasing = TRUE, margin = "both"), n = 6, nf = 10) 

# Más estadísticas
topfeatures(my.dfm) # cuenta las palabras de todo el  DFM
library(quanteda.textstats)
word.frequencies <- textstat_frequency(my.dfm) # proporciona además el rango, el número de documentos en los que aparece la palabra
head(word.frequencies)
#Clasificar por frecuencia
head(dfm_sort(my.dfm, decreasing = TRUE, margin = "both"), n = 6, nf = 10) 
#Seleccionar ciertas voces
dfm_select(my.dfm, pattern = "salud")

# Frecuencia relativa de palabras 
my.dfm.proportional <- dfm_weight(my.dfm, scheme = "propmax")
frecuencia_relativa<- convert(my.dfm.proportional, "data.frame")

# Términos particularmente distintivos
my.dfm.tfidf <- dfm_tfidf(my.dfm)
topfeatures(my.dfm.tfidf)

# Palabras distintivas por frecuencia 
my.dfm.trim <- dfm_trim(my.dfm, min_termfreq = 4, max_termfreq = 10)
head(my.dfm.trim, n = 6, nf = 10)

# Palabras que se encuentran en un percentil de frecuencia 85 (es decir, el 5% superior de todas las características).
my.dfm.trim <- dfm_trim(my.dfm, min_termfreq = 0.25, max_termfreq = 0.80, termfreq_type = "quantile")
head(my.dfm.trim, n = 6, nf = 10) 

#Nube de palabras
library(quanteda.textplots)
textplot_wordcloud(my.dfm, min_size = 1, max_size = 5, max_words = 100)

# Nube TF-IDF: La siguiente trama muestra los términos más 
# distintivos según TF-IDF, donde el color indica el texto respectivo.
textplot_wordcloud(my.dfm[1:4,], color = brewer.pal(6, "Set1"), min_size = 0.2, max_size = 4, max_words = 100, comparison = TRUE)
textplot_wordcloud(my.dfm[4:7,], color = brewer.pal(6, "Set1"), min_size = 0.2, max_size = 4, max_words = 100, comparison = TRUE)


# Keyness: medimos el valor distintivo de ciertas palabras para un documento
my.dfm <- dfm(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("spanish"))
my.dfm
keyness <- textstat_keyness(my.dfm, target = "PSOE.txt", measure = "lr")
textplot_keyness(keyness)

keyness <- textstat_keyness(my.dfm, target = "Vox.txt", measure = "lr")
textplot_keyness(keyness)

# Las medidas de diversidad léxica son métricas que reflejan la 
# diversidad de un texto con respecto al uso de palabras: proporciona 
# información sobre la complejidad de un texto
lexical.diversity <- textstat_lexdiv(my.dfm, measure = "all")
lexical.diversity

# Podemos exportar a excel los datos
write_delim(lexical.diversity, path = "lexicaldiversity.csv", delim = ";") # File is Excel compatible

# Legibilidad
readability <- textstat_readability(my.corpus, measure = "all")
readability

# Exportamos
write_delim(readability, path = "readability.csv", delim = ";") # file is Excel compatible


#Análisis: 
# Primer análisis: política local, nacional e internacional

bienestar_social <- scan("~/Desktop/politica2/DIC_POLITICO/aaa/bienestar_social.txt", what = "char", sep = "\n", skip = 35, quiet = T)
economia <- scan("~/Desktop/politica2/DIC_POLITICO/aaa/economia.txt", what = "char", sep = "\n", skip = 35, quiet = T)
gestion_politica <- scan("~/Desktop/politica2/DIC_POLITICO/aaa/gestion_politica.txt", what = "char", sep = "\n", skip = 35, quiet = T)
seguridad_comunicacion <- scan("~/Desktop/politica2/DIC_POLITICO/aaa/seguridad_comunicacion.txt", what = "char", sep = "\n", skip = 35, quiet = T)
sentiment.dictionary <- dictionary(list(bienestar_social = bienestar_social, economia = economia, 
                                        gestion_politica = gestion_politica, seguridad_comunicacion = seguridad_comunicacion))
str(sentiment.dictionary)



# Creamos dfm de sentimientos del corpus 
dfm.sentiment <- dfm(my.corpus, dictionary = sentiment.dictionary)
dfm.sentiment

# La siguiente trama muestra la distribución de sentimientos en las
# novelas
sentiment <- convert(dfm.sentiment, "data.frame") %>%
  gather(bienestar_social, economia, gestion_politica, seguridad_comunicacion, key = "Polarity", value = "Words") %>% 
  mutate(doc_id = as_factor(doc_id)) %>% 
  rename(Partido = doc_id)
ggplot(sentiment, aes(Partido, Words, fill = Polarity, group = Polarity)) + geom_bar(stat='identity', position = position_dodge(), size = 1) + scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Porcentaje de sentimiento en los programas (absolutos") + xlab("")

# Ponderación de los sentimientos en función de valores relativos 
# palabra/total palabras
dfm.sentiment.prop <- dfm_weight(dfm.sentiment, scheme = "prop")
dfm.sentiment.prop

# Trazamos dfm y graficamos
sentiment <- convert(dfm.sentiment.prop, "data.frame") %>%
  gather(bienestar_social, economia, gestion_politica, seguridad_comunicacion, key = "Polarity", value = "Share") %>% 
  mutate(doc_id = as_factor(doc_id)) %>% 
  rename(Partido = doc_id)
ggplot(sentiment, aes(Partido, Share, fill = Polarity, group = Polarity)) + geom_bar(stat='identity', position = position_dodge(), size = 1) + scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Ponderación de los Sentimientos en las seis novelas (relative)")


#########################
# modelado de temas
#Volvemos a crear una matriz
my.dfm <- dfm(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("spanish"))
my.dfm
dfm.trim <- dfm_trim(my.dfm, min_termfreq = 2, max_termfreq = 75)
dfm.trim
#definimos arbitrariamente un número de temas de k = 10
n.topics <- 7
dfm2topicmodels <- convert(dfm.trim, to = "topicmodels")
lda.model <- LDA(dfm2topicmodels, n.topics)
lda.model
aa<- as.data.frame(terms(lda.model, 10))
aa


# Vemos en qué textos estos tópicos son más fuertes
data.frame(Topic = topics(lda.model))

#Cuál es la proporción de fuerza de cada tema
lda.topics.chapters <- data.frame(my.corpus.stats, Topic = topics(lda.model)) %>%
  add_count(Text, Topic) %>%
  group_by(Text) %>% 
  mutate(Share = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(Topic = paste0("Topic ", sprintf("%02d", Topic))) %>% 
  mutate(Novela = as_factor(Text))
ggplot(lda.topics.chapters, aes(Novela, Share, color = Topic, fill = Topic)) + geom_bar(stat="identity") + ggtitle("LDA topics in Sherlock Holmes novels") + xlab("") + ylab("Share of topics [%]") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Conexión de términos y temas
head(as.data.frame(t(lda.model@beta), row.names = lda.model@terms)) # Terms > Topics
#Conexión tópicos documentos
head(as.data.frame(lda.model@gamma, row.names = lda.model@documents)) # Documents > Topics


# Probabilidades de palabra tema: el modelo calcula la probabilidad
# de que ese término se genere a partir de ese tema
library(tidytext)
ap_topics <- tidy(lda.model, matrix = "beta")
ap_topics

#términos que son más comunes dentro de cada tema.
library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

ap_top_terms

# Temas asociados a cada documento
doc_gmma <- tidy(lda.model, matrix = "gamma")
doc_gmma

## reordenar los documentos por tópico 1, topico 2, etc. 
doc_gmma %>%
  mutate(title = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) +
  labs(x = "topic", y = expression(gamma))

#PROBLEMAS DE VARIOS TEMAS  EN DISTINTOS PARTIDOS


# ¿Hay casos en los que el tema más asociado a un capítulo perteneciera a otro libro?
library(stringr)

doc_classifications <- doc_gmma %>%
  group_by(document, topic) %>%
  slice_max(gamma) %>%
  ungroup()

doc_classifications



# los términos que tuvieran la mayor diferencia en entre varios temas,
# por tema 1 y el tema 2, tras filtrar palabras relativamente comunes, 
# como aquellas que tienen un β mayor que 1/1000 en al menos un tema.
library(tidyr)
beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide

# LDA también modela cada documento como una mezcla de temas. 
# Podemos examinar las probabilidades de documento por tema

library(topicmodels)

ap_lda <- LDA(dfm2topicmodels, k = 7, control = list(seed = 1234))
ap_lda
# Documentos: LDA también modela cada documento como una mezcla de temas
# Cada uno de estos valores es una proporción estimada de palabras de 
# ese documento que se generan a partir de ese tema
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

# verificar cuáles eran las palabras más comunes en ese documento.
tidy(dfm2topicmodels) %>%
  arrange(desc(count))

# Palabras: modelo calcula la probabilidad de que ese término se genere a partir de ese tema
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics


#################################3
# COMPARAMOS IDEOLOGÍA
#################################
#leemos archivos del directorio elegido
textos <- readtext("~/Desktop/politica2/corpus/*.txt")
cor_textos <- corpus(textos)
summary(cor_textos, 4)
#Grafico
tokeninfo <- summary(cor_textos)
if (require(ggplot2))
  ggplot(data = tokeninfo, aes(x = Text, y = Types, group = 1)) +
  geom_line() +
  geom_point() 

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

#Ver las palabras más frecuentes <- dfm(data_char_ukimmig2010, remove = stopwords("english"), remove_punct = TRUE)
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
dictliwc <- dictionary(file = "~/Desktop/politica2/Spanish_LIWC2021_Dictionary.dic",
                       format = "LIWC")
dfmat_cor_textos <- dfm(cor_textos[1:7], dictionary = dictliwc)
dfmat_cor_textos[, 33:46]

