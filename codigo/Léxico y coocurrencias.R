
#direccion 
direccion <- ("~/Desktop/CRISTINA_STYLO/data4_Cervantes_Carcel_Sevilla")

# Hemos preparado los textos identificando con un numeral cada uno de los títulos
#cerv_1 =  "La cueva de Salamanca"; cerv_2 = "La guarda cuidadosa"; 
# cerv_3 = "El juez de los divorcios": cerv_4 = "El retablo de las maravillas";
# cerv_5 = "El viejo celoso"; cerv_6 = "El vizcaino fingido".

#Carga las librería necesarias

library(tidyverse)
library(tidytext)

#anno = nombres
#mensaje = textos

# Cargará todos los ficheros de los mensajes
ficheros <- list.files(path = "~/Desktop/politica/corpus", 
                       pattern = ".txt")

ficheros
nombres <- gsub("\\.txt", 
               "", 
               ficheros, perl = T)
nombres
textos <- tibble(titulo = character(),
                 parrafo = numeric(),
                 texto = character())


for (i in 1:length(ficheros)){
  entremeses <- readLines(paste("~/Desktop/politica/corpus",
                                ficheros[i],
                                sep = "/"))
  temporal <- tibble(titulo = nombres[i],
                     parrafo = seq_along(entremeses),
                     texto = entremeses)
  textos <- bind_rows(textos, temporal)
}
textos
# Regenera la tabla general con todas las palabras
textos_palabras <- textos %>%
  unnest_tokens(palabra, texto)

textos_palabras


# Crea la tabla con todas las palabras y calcula frecuencias
textos_frecuencias <- textos_palabras %>%
  count(palabra, sort = T) %>%
  mutate(relativa = n / sum(n)) %>%
  ungroup()


textos_frecuencias

#Podemos realizar los cálculos para cada obra
frecuencias_titulo <- textos_palabras %>%
  group_by(titulo) %>%
  count(palabra, sort =T) %>%
  mutate(relativa = n / sum(n)) %>%
  ungroup()

frecuencias_titulo

# Borra objetos que no sirven y que son temporales
rm(temporal,entremeses,i)


#vas a extraer la lista de palabras vacías que tiene tidytext y la vas 
# a guardar en un objeto que llamarás vacias
vacias <- get_stopwords("es")
vacias

#Le decimos a nuestra herramienta que utilizamos 'palabra' en vez de 'word'
vacias <- vacias %>%
  rename(palabra = word)

#Borramos ahora todas las palabras vacías
textos_vaciado <- textos_palabras %>%
  anti_join(vacias)

#Puedes borrar todas esas palabras que poco dicen acerca del texto.
vacias_adhoc <- tibble(palabra = c("cristina", "si", "señor", "vuesa", "pues", 
                                   "aquí", "tan", "cómo", "merced", "señora", "doña", 
                                   "bien", "lorenza", "sino", "cañizares", "voacé", 
                                   "hortigosa", "solorzano", "leonarda", "benito", 
                                   "pancracio", "seor", "alguno", "ay", "aunque", "brigida", 
                                   "aun", "barragán", "beltrana", "garay", 
                                   "solapo", "mariana", "juan", "chirinos", "castrada", "castrado", "guiomar", 
                                   "alguna", "salamanca", "solórzano", "quiñones", "cuatro", "dos", 
                                   "paisano", "así", "chanfalla", "capacho", "cristinica", "torbellina", 
                                   "agora"))
                                   
                                   
                                   
                                   
                                  
textos_vaciado <- textos_vaciado %>%
  anti_join(vacias_adhoc)


# Creamos una variable con el nombre de los autores y así manejar tan 
#solo dos variables categóricas: Cervantes y Anónimo. 
textos_vaciado <- textos_vaciado %>%
  mutate(autor = titulo) %>%
  mutate(autor = str_replace(autor, "cerv_[123456]", "Cervantes")) %>%
  mutate(autor = str_replace(autor, "\\d+", " "))

#Vamos a extraer las 15 palabras más comunes de cada uno de los dos reyes
textos_vaciado %>%
  group_by(titulo) %>%
  count(palabra, sort = T) %>%
  top_n(20) %>%
  
  ggplot(aes(reorder(palabra, n),
             n,
             fill = titulo)) +
  geom_bar(stat = "identity") +
  facet_wrap(~titulo,
             scales = "free_y") +
  labs(x = "",
       y = "Frecuencia absoluta") +
  coord_flip() +
  theme(legend.position="none")

# Repetimos la acción para ver palabras únicas y coincidentes. Las líneas en las que no 
#hay barra indican que son palabras que no existen en los discursos de uno u otro autor.
textos_vaciado %>%
  group_by(titulo) %>%
  count(palabra, sort = T) %>%
  top_n(5) %>%
  
  ggplot(aes(reorder(palabra, n),
             n,
             fill = titulo)) +
  geom_bar(stat = "identity") +
  facet_wrap(~titulo,
             scales = "fixed") +
  labs(x = "",
       y = "Frecuencia absoluta") +
  coord_flip() +
  theme(legend.position="none")

# Vemos cuáles son las palabras más frecuentes en cada texto.
textos_nombre <- textos_vaciado %>%
  select(titulo, palabra) %>%
  group_by(titulo) %>%
  count(palabra, sort =T) %>%
  ungroup()
textos_nombre

#Dibujamos la tabla
textos_nombre %>%
  filter(n > 8) %>%
  ggplot(aes(x = palabra,
             y = n)) +
  geom_col(fill = "aquamarine") +
  coord_flip() +
  facet_wrap(~ titulo,
             ncol = 3,
             scales = "free_y")

############
#COMPARATIVO MORFOLOGÍA
###############
#direccion 
direccion <- ("~/Desktop/CRISTINA_STYLO/data4_Cervantes_Carcel_Sevilla")
library(udpipe)

#El siguiente paso es bajar de internet el modelo de lengua. 
udpipe_download_model(language = "spanish-ancora")

#Ahora cargas el modelo de udpipe que necesitas. Utiliza ancora.
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.4-190531.udpipe')

# Leemos todos los textos
ficheros <- list.files(path = "~/Desktop/CRISTINA_STYLO/data4_Cervantes_Carcel_Sevilla", pattern = ".txt")
ficheros

#quitamos .txt
nombres <- gsub("\\.txt", 
                "", 
                ficheros, perl = T)
nombres

# Agrupar ficheros por autores
autor <- c(rep("Anónimo", 1),
         rep("Cervantes", 6))

autor


#Creamos tabla
textos <- tibble(titulo = character(),
                 autor = character(),
                 parrafo = numeric(),
                 texto = character())


for (i in 1:length(ficheros)){
  entremeses <- readLines(paste("~/Desktop/CRISTINA_STYLO/data4_Cervantes_Carcel_Sevilla",
                                ficheros[i],
                                sep = "/"))
  temporal <- tibble(titulo = nombres[i],
                     autor = autor[i],
                     parrafo = seq_along(entremeses),
                     texto = entremeses)
  textos <- bind_rows(textos, temporal)
}
#Para que lea updipe
textos$texto <- iconv(textos$texto, from = "Latin1", to = "UTF-8")

# Creamos nuevas columas
TituloTextos <- nombres
AutorTextos <- autor

#Ahora crea una tabla vacía que llamarás Textos_Analizado en la que 
#recopilarás el resultado de los análisis. 
Textos_Analizado <- tibble(parrafo_id = integer(),
                             enunciado_id = integer(),
                             enunciado = character(),
                             token_id = character(),
                             token = character(),
                             lema = character(),
                             upos = character(),
                             xpos = character(),
                             rasgos = character(),
                             titulo = character(),
                             autor = character())

# Extraemos los mensajes por título
for(i in 1:length(TituloTextos)){
  temporal <- textos %>%
    filter(titulo == TituloTextos[i]) %>%
    select(texto)
  analisis <- as_tibble(udpipe_annotate(modelo_ancora,
                                        temporal$texto))
  analisis <- analisis %>%
    add_column(titulo = TituloTextos[i],
               autor = AutorTextos[i]) %>%
    select(-(paragraph_id),
           -(deps),
           -(misc),
           -(head_token_id),
           -(dep_rel)) %>%
    rename(parrafo_id = doc_id,
           enunciado_id = sentence_id,
           enunciado = sentence,
           lema = lemma,
           rasgos = feats) %>%
    mutate(parrafo_id = as.numeric(str_extract(parrafo_id, "\\d+")))
  Textos_Analizado <- bind_rows(Textos_Analizado, analisis)
  rm(temporal, analisis)
}

#Reordenamos las columnas
colnames(Textos_Analizado)
Textos_Analizado <- Textos_Analizado[c(10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9)]

#Extraemos datos totales del corpus analizado
Textos_Analizado %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()

#Distinguimos ahora entre los autores
clases <- Textos_Analizado %>%
  group_by(autor) %>%
  drop_na(upos) %>%
  count(upos, sort = T) 

ggplot(clases, aes(upos, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~autor)

# Pero estas gráficas reflejan valores absolutos (el corpus de los 6 
#entremeses de Cervantes reúne seis textos frente al texto único anónimo).
# Tenemos ahora que ver las frecuencias relativas
#Totales
clases <- Textos_Analizado %>%
  group_by(autor) %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>%
  mutate(frecuencia = n/sum(n)*100)

ggplot(clases, aes(upos, frecuencia)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~autor)

# Pedimos ahora confrontar los verbos de La cárcel de Sevilla 
#con los entremeses indubitados de Cervantes

clases <- Textos_Analizado %>%
  group_by(autor) %>%
  filter(upos == "AUX" | upos == "VERB") %>%
  count(token, sort = TRUE) %>%
  mutate(frecuencia = n/sum(n)*100)%>%
  top_n(20)

ggplot(clases, aes(token, frecuencia)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~autor, scales = "free_y") +
  labs(x = "",
     y = "Frecuencia absoluta") +
  coord_flip() +
  theme(legend.position="none")

###########################3
#COLOCACIONES Y COOCURRENCIAS
##############################

#direccion 
direccion <- ("~/Desktop/CRISTINA_STYLO/data4_Cervantes_Carcel_Sevilla")


#Carga las librería necesarias

library(tidyverse)
library(tidytext)

ficheros <- list.files(path = "~/Desktop/CRISTINA_STYLO/data4_Cervantes_Carcel_Sevilla", pattern = "\\d+")

#quitamos .txt
nombres <- gsub("\\.txt", 
                "", 
                ficheros, perl = T)
nombres

# Agrupar ficheros por autores
autor <- c(rep("Anónimo", 1),
           rep("Cervantes", 6))


#Creamos tabla
textos <- tibble(titulo = character(),
                 autor = character(),
                 parrafo = numeric(),
                 texto = character())


for (i in 1:length(ficheros)){
  entremeses <- readLines(paste("~/Desktop/CRISTINA_STYLO/data4_Cervantes_Carcel_Sevilla",
                                ficheros[i],
                                sep = "/"))
  temporal <- tibble(titulo = nombres[i],
                     autor = autor[i],
                     parrafo = seq_along(entremeses),
                     texto = entremeses)
  textos <- bind_rows(textos, temporal)
}
textos$autor <- factor(textos$autor, levels = c("Anónimo", "Cervantes"))
textos$titulo <- factor(textos$titulo)


#argar el fichero de palabras vacías

vacias <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt",
                   locale = default_locale())

# Borramos objetos que ya no serán operativos
rm(temporal, nombres, entremeses, ficheros, i, autor)

#Dividimos en bigramas
textos %>%
  unnest_tokens(bigrama,
                texto,
                token = "ngrams",
                n = 2)

textos_bigramas <- textos %>%
  unnest_tokens(bigrama,
                texto,
                token = "ngrams",
                n = 2)

#Contarlos y ponerlos en orden decreciente se logra con

textos_bigramas %>%
  count(bigrama, sort = T)

# Eliminamos bigrams vacíos o irrelevantes
bigramas_separados <- textos_bigramas %>%
  separate(bigrama,
           c("palabra1", "palabra2"),
           sep = " ")
bigramas_filtrados <- bigramas_separados %>%
  filter(!palabra1 %in% vacias$palabra,
         !palabra2 %in% vacias$palabra)

#Ahora puedes comprobar el resultado, de las diez más frecuentes.

bigramas_filtrados %>%
  count(palabra1, palabra2, sort = T)

# Borra todo lo referente a personajes

bigramas_filtrados <- bigramas_filtrados %>%
  filter(!palabra1 %in% c("vuesa", "personas","garay", "solapo", "pícaro", "coplilla", "barragán", "torbellina", "beltrana", "paisano", "cristina", "doña", "soldado", "sacristán", "lorenza", "cañizares", "solorzano", "leonarda"),
         !palabra2 %in% c("vuesa", "cristina", "siguientes", "garay", "pícaro", "coplilla", "barragán", "solapo", "torbellina", "beltrana", "doña", "soldado", "sacristán", "lorenza", "cañizares", "solorzano", "leonarda"))
bigramas_filtrados


# Hacemos gráfico: uniendo primero los ds término
bigramas_unidos <- bigramas_filtrados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Vemos cuáles son
bigramas_unidos %>%
  count(autor, bigrama, sort = T) 

#Graficamos  
bigramas_unidos %>%
  count(autor, bigrama, sort = T) %>%
  group_by(autor) %>%
  top_n(10) %>%
  ggplot() +
  geom_col(aes(y = n , x = reorder(bigrama,n)),
           fill = "maroon") +
  coord_flip() +
  facet_wrap(~ autor, ncol = 2, scales = "free") +
  theme_linedraw() + 
  labs(x = "Bigramas", y = "Frecuencia") + 
  ggtitle("Bigramas más frecuentes de cada autor", subtitle = "entremeses")

#Coocurrencias
# Instalamos paquetes y reclamamos librer´ía

library("widyr")
library(igraph)
library(ggraph)
library(grid)

# Reclamamos los textos
textos_autor <- textos %>%
  filter(autor =="Cervantes") %>%
  mutate(seccion = row_number()) %>%
  unnest_tokens(palabra, texto) %>%
  filter(!palabra %in% vacias$palabra)
textos_autor


#Ahora lo dividirás en pares de palabras y las contabilizarás.
pares_palabras <- textos_autor %>%
  pairwise_count(palabra,
                 seccion,
                 sort = T)
pares_palabras


#Parece que la palabra "merced"  es la palabra con mayor número de ocurrencias y, 
#posiblemente, de coapariciones. Verlas es muy sencillo con filter().
pares_palabras %>%
  filter(item1 == "merced")

#Lo que te interesa es examinar la correlación que existe entre las palabras, 
# lo cual te indicará cuán a menudo aparecen juntas con referencia a cuántas 
#veces están separadas. Lo que aquí se busca, ¡horror!, es el coeficiente phi.
palabras_correlacion <- textos_autor %>%
  group_by(palabra) %>%
  filter(n() >= 5) %>%
  pairwise_cor(palabra,
               seccion,
               sort = TRUE)

palabras_correlacion

#Lo que has realizado en esta ocasión es el coeficiente de correlación que 
#sirve para comprobar, y medir, la fuerza y la dirección de una relación 
# lineal entre dos variables. 
#Este tipo de resultado te permite examinar cuáles son las palabras que 
#pueden aparecer más correlacionadas con un término como "amor". 
#Es algo que puedes hacer, de nuevo, con filter():

palabras_correlacion %>%
  filter(item1 == "amor")


#Podemos examinar cualquier palabra. Incluso varias a la vez
palabras_correlacion %>%
  filter(item1 %in% c("amor",
                      "amistad",
                      "verdad",
                      "salud",
                      "país",
                      "trabajo")) %>%
group_by(item1) %>%
top_n(10) %>%
ungroup() %>%
mutate(item2 = reorder(item2, correlation)) %>%
ggplot(aes(item2, correlation)) +
geom_bar(stat = "identity") +
facet_wrap(~ item1, scales = "free") +
coord_flip()


# Ahora trazaremos un grafo para ver los grupos de palabras
palabras_correlacion %>%
  filter(correlation > .40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "nicely") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

