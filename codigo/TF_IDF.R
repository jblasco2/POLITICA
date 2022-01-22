#PALABRAS DISTINTIVAS DE DIFERENTES AUTORES#
###################
#TF-IDF
# encontrar las palabras importantes para el contenido de cada 
# documento reduciendo el peso de palabras de uso común: trabaja con 
# las palabras comunes, “pero no demasiado comunes”. TF IDF es un 
# método fiable para estimar la relevancia de un documento para un 
# término. Se consigue un TF IDF alto por la combinación de una 
# frecuencia alta de un término en un documento y frecuencia baja en 
# otros documentos que mencionan ese término. Según aumenta el número 
# de documentos que incluyen el término, baja el valor de TF IDF, 
# hasta el extremo de que puede llegar a 0, si todos o casi todos 
# los documentos de una muestra amplia lo mencionan. 
######################################
#y veamos qué términos son importantes en estos trabajos, 
#medidos por tf-idf.
library(dplyr)
library(tidytext)
library(ggplot2)
#Directorio
ciudadanos <- scan("~/Desktop/politica/corpus/1_Ciudadanos.txt", what = "character", sep = "\n")
pp <- scan("~/Desktop/politica/corpus/2_pp.txt", what = "character", sep = "\n")
vox <- scan("~/Desktop/politica/corpus/3_vox.txt", what = "character", sep = "\n")
Mas_pais <- scan("~/Desktop/politica/corpus/4_Mas_pais.txt", what = "character", sep = "\n")
podemos <- scan("~/Desktop/politica/corpus/5_Podemos.txt", what = "character", sep = "\n")
PSOE <- scan("~/Desktop/politica/corpus/6_PSOE.txt", what = "character", sep = "\n")
pnv <- scan("~/Desktop/politica/corpus/7_pnv.txt", what = "character", sep = "\n")
novelistas <- function(){
  books <- list(
    "Programa Ciudadanos" = ciudadanos,
    "Programa PP" = pp,
    "Programa Vox" = vox,
    "Programa Mas_pais" = Mas_pais,
    "Programa Podemos" = podemos,
    "Programa PSOE" = PSOE,
    "Programa PNV" = pnv
  )
ret <- data.frame(text = unlist(books, use.names = FALSE), 
                    stringsAsFactors = FALSE)
  ret$book <- factor(rep(names(books), sapply(books, length)))
  ret$book <- factor(ret$book, levels = unique(ret$book))
  structure(ret, class = c("tbl_df", "tbl", "data.frame"))
}

book_words <- novelistas() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()
total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))
book_words <- left_join(book_words, total_words)
book_words

#Stopwords
library(stringr)
mystopwords <- tibble(word = c("pedro", "sumar", "ocultar", "véase", "muchas", "entrado", "sánchez", "eajpnv"))

book_words <- anti_join(book_words, mystopwords, 
                        by = "word")
book_words

#Sigamos adelante y calculemos tf-idf, 
# luego visualicemos las palabras de alto tf-idf en la Figura 3.5
plot_novelistas <- book_words %>%
  bind_tf_idf(word, book, n) %>%
  mutate(book = factor(book, levels = c("Programa Ciudadanos",
                                        "Programa PP",
                                        "Programa Vox",
                                        "Programa Mas_pais",
                                        "Programa Podemos",
                                        "Programa PSOE",
                                        "Programa PNV")))

plot_novelistas %>% 
  group_by(book) %>% 
  slice_max(tf_idf, n = 8) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~book, ncol = 2, scales = "free")

# borrar nombres propios
library(stringr)
mystopwords <- tibble(word = c("pedro", "sumar", "véase", "muchas", "entrado", "sánchez", "eajpnv"))

book_words <- anti_join(book_words, mystopwords, 
                           by = "word")
book_words


#calculemos tf-idf, luego visualicemos las palabras de alto tf-idf 
#Obtenemos todas las palabras de unos documentos
book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf

#idf recoge las palabras que son comunes en todos los documentos
#tf_idf, a la inversa, da unos porcentajes mayores para las palabras que aparecen en menos documentos 
#de la colección.

#Veamos los términos con alta tf-idf 
book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#Podemos visualizar las palabras de alta tf_idf
library(forcats)

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

