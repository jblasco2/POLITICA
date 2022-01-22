######################################################################
### MODELADO DE TEMAS
######################################################################
library("quanteda")
library("tidyverse")
library("topicmodels")
library("ldatuning")
library("stm")
library("wordcloud")
theme_set(theme_bw())

# ESTABLECER DIRECTORIO
setwd("~/Desktop/politica2/corpus")
#CARGAMOS LOS TEXTOS PARA ANÁLISIS
partidos <- readtext("~/Desktop/politica2/corpus/*.txt") 
partidos$doc_id <- str_sub(partidos$doc_id, start = 1, end = -5)
partidos
#CONSTRUIMOS UN CORPUS
my.corpus <- corpus(partidos)
docvars(my.corpus, "Textno") <- sprintf("%02d", 1:ndoc(my.corpus))
my.corpus
#Reordenamos
my.corpus.stats <- summary(my.corpus)
my.corpus.stats$Text <- reorder(my.corpus.stats$Text, 1:ndoc(my.corpus), order = T)
my.corpus.stats
# ELIMINAMOS PALABRAS DE FUNCIÓN Y LOCALIZAMOS AQUELLAS QUE SE 
#REPITEN MÁS FRECUENTEMENTE
my.dfm <- dfm(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = c(stopwords("spanish"), "Podemos", "PSOE", "PNV"))
my.dfm
dfm.trim <- dfm_trim(my.dfm, min_termfreq = 2, max_termfreq = 10)
dfm.trim
#Modelado real de temas
n.topics <- 8
dfm2topicmodels <- convert(dfm.trim, to = "topicmodels")
lda.model <- LDA(dfm2topicmodels, n.topics)
lda.model
# generar Los términos que están particularmente vinculados a cada uno de los temas
as.data.frame(terms(lda.model, 8))
#documentos en los que los temas son particularmente fuertes
data.frame(Topic = topics(lda.model))
##dividiendo los eventos temáticos de un programa por el número 
#total de secciones.
lda.topics.chapters <- data.frame(my.corpus.stats, Topic = topics(lda.model)) %>%
  add_count(Sentences, Topic) %>%
  group_by(Sentences) %>% 
  mutate(Share = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(Topic = paste0("Tema", sprintf("%02d", Topic))) %>% 
  mutate(Programa = as_factor(Text))
ggplot(lda.topics.chapters, aes(Programa, Share, color = Topic, fill = Topic)) + geom_bar(stat="identity") + ggtitle("LDA de temas en programas electorales") + xlab("") + ylab("Porcentajes tema [%]") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#La probabilidad de la asociación de un término con un tema
temas <- head(as.data.frame(t(lda.model@beta), row.names = lda.model@terms)) # Terms > Topics
temas

#La probabilidad de la asociación de un tema con un documento.
documentos <- head(as.data.frame(lda.model@gamma, row.names = lda.model@documents)) # Documents > Topics
documentos

#Temas STM: eliminando palabras que ocurren en menos del 7.5% y más 
# del 90% de todos los documentos.
dfm.un <- dfm(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("spanish"))
dfm.un.trim <- dfm_trim(dfm.un, min_docfreq = 0.075, max_docfreq = 0.90, docfreq_type = "prop") # min 7.5% / max 90%
dfm.un.trim

# las palabras clave más importantes para cada tema (X1-X40) y visualzación.
n.topics <- 8
dfm2stm <- convert(dfm.un.trim, to = "stm")
modell.stm <- stm(dfm2stm$documents, dfm2stm$vocab, K = n.topics, data = dfm2stm$meta, init.type = "Spectral")
as.data.frame(t(labelTopics(modell.stm, n = 10)$prob))
par(mar=c(0.5, 0.5, 0.5, 0.5))
cloud(modell.stm, topic = 1, scale = c(2.25,.5))

#términos centrales de cuatro temas relacionados
plot(modell.stm, type = "labels", topics = c(1, 2, 3, 4), main = "Temas por palabras")
plot(modell.stm, type = "labels", topics = c(5, 6, 7, 8), main = "Temas por palabras")
# los contrastes entre dos Temas relacionados.
plot(modell.stm, type = "perspectives", topics = c(5,6), main = "Contraste de temas")

#################################
# OTRO CAMINO
library(topicmodels)

# Cargamos datos
#CARGAMOS LOS TEXTOS PARA ANÁLISIS
partidos <- readtext("~/Desktop/politica2/corpus/*.txt") 
partidos$doc_id <- str_sub(partidos$doc_id, start = 1, end = -5)
#CONSTRUIMOS UN CORPUS
my.corpus <- corpus(partidos)
docvars(my.corpus, "Textno") <- sprintf("%02d", 1:ndoc(my.corpus))
my.corpus


#Reordenamos
my.corpus.stats <- summary(my.corpus)
my.corpus.stats$Text <- reorder(my.corpus.stats$Text, 1:ndoc(my.corpus), order = T)
my.corpus.stats
# ELIMINAMOS PALABRAS DE FUNCIÓN
my.dfm <- dfm(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = c(stopwords("spanish"), "Podemos", "PSOE", "PNV"))
my.dfm
dfm.trim <- dfm_trim(my.dfm, min_termfreq = 2, max_termfreq = 20)
dfm.trim
#Creamos un modelo LDA de 7 temas
ap_lda <- LDA(my.dfm, k = 7, control = list(seed = 1234))
ap_lda

# Probabilidades de palabra tema: el modelo calcula la probabilidad
# de que ese término se genere a partir de ese tema
library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
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
doc_gmma <- tidy(ap_lda, matrix = "gamma")
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
# Podemos examinar las probabilidades por documento por tema
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

