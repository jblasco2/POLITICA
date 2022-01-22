library("quanteda.sentiment")
data(data_corpus_inaugural, package = "quanteda")

# inspect the dictionary and its polarities
print(data_dictionary_geninqposneg, max_nval = 8)
## Dictionary object with 2 key entries.
## Polarities: pos = "positive"; neg = "negative" 
## - [positive]:
##   - abide, ability, able, abound, absolve, absorbent, absorption, abundance [ ... and 1,645 more ]
## - [negative]:
##   - abandon, abandonment, abate, abdicate, abhor, abject, abnormal, abolish [ ... and 2,002 more ]

# compute sentiment
tail(data_corpus_inaugural) %>%
  textstat_polarity(dictionary = data_dictionary_geninqposneg)
##           doc_id sentiment
## 1      2001-Bush 0.9233579
## 2      2005-Bush 0.9829457
## 3     2009-Obama 0.5666378
## 4     2013-Obama 0.7597420
## 5     2017-Trump 0.7724428
## 6 2021-Biden.txt 0.6018714
Para un diccionario de valencia, podemos calcular esto para la categoría de "placer" de las Normas afectivas para palabras en inglés (ANEW):
  
library("quanteda", warn.conflicts = FALSE, quietly = TRUE)
## Package version: 3.0.0
## Unicode version: 10.0
## ICU version: 61.1
## Parallel computing: 12 of 12 threads used.
## See https://quanteda.io for tutorials and examples.
library("quanteda.sentiment")

# inspect the dictionary and its valences
print(data_dictionary_ANEW, max_nval = 8)
## Dictionary object with 3 key entries.
## Valences set for keys: pleasure, arousal, dominance 
## - [pleasure]:
##   - abduction, able, abortion, absent, absurd, abundance, abuse, accept [ ... and 2,463 more ]
## - [arousal]:
##   - abduction, able, abortion, absent, absurd, abundance, abuse, accept [ ... and 2,463 more ]
## - [dominance]:
##   - abduction, able, abortion, absent, absurd, abundance, abuse, accept [ ... and 2,463 more ]
lapply(valence(data_dictionary_ANEW), head, 8)
## $pleasure
## abduction      able  abortion    absent    absurd abundance     abuse    accept 
##      2.76      6.74      3.50      3.69      4.26      6.59      1.80      6.80 
## 
## $arousal
## abduction      able  abortion    absent    absurd abundance     abuse    accept 
##      5.53      4.30      5.39      4.73      4.36      5.51      6.83      5.53 
## 
## $dominance
## abduction      able  abortion    absent    absurd abundance     abuse    accept 
##      3.49      6.83      4.59      4.35      4.73      5.80      3.69      5.41

# compute the sentiment
tail(data_corpus_inaugural) %>%
  textstat_valence(dictionary = data_dictionary_ANEW["pleasure"])
##           doc_id sentiment
## 1      2001-Bush  6.091330
## 2      2005-Bush  6.308839
## 3     2009-Obama  5.841437
## 4     2013-Obama  6.045129
## 5     2017-Trump  6.223944
## 6 2021-Biden.txt  6.018528
Podemos comparar dos medidas calculadas de diferentes formas (aunque no son comparables, en realidad, ya que son léxicos diferentes):
  
# ensure we have this package's version of the dictionary
data("data_dictionary_LSD2015", package = "quanteda.sentiment")

sent_pol <- tail(data_corpus_inaugural, 25) %>%
  textstat_polarity(dictionary = data_dictionary_LSD2015)
sent_pol <- dplyr::mutate(sent_pol, polarity = sentiment)
sent_val <- tail(data_corpus_inaugural, 25) %>%
  textstat_valence(dictionary = data_dictionary_AFINN)

data <- data_corpus_inaugural

library("ggplot2")

ggplot(data.frame(sent_pol, valence = sent_val$sentiment),
       aes(x = polarity, y = valence)) +
  geom_point()


###########################

library("quanteda")
simpledict <- dictionary(list(
  happy = c("happy", "jubilant", "exuberant"),
  sad = c("sad", "morose", "down")
))
polarity(simpledict)
polarity(simpledict) <- list(pos = "happy", neg = "sad")
polarity(simpledict)

# can list multiple keys
polarity(data_dictionary_LSD2015) <- list(
  pos = c("positive", "neg_negative"),
  neg = c("negative", "neg_positive")
)
polarity(data_dictionary_LSD2015)

library(quanteda/quanteda.sentiment)

library("quanteda")
corp <- tail(data_corpus_inaugural, n = 5)
toks <- tokens(corp)
dfmat <- dfm(toks)
polar1 <- list(pos = "positive", neg = "negative")
polar2 <- list(pos = c("positive", "neg_negative"),
               neg = c("negative", "neg_positive"))

polarity(data_dictionary_LSD2015) <- polar1
textstat_polarity(corp, dictionary = data_dictionary_LSD2015)
textstat_polarity(toks, dictionary = data_dictionary_LSD2015)
textstat_polarity(dfmat, dictionary = data_dictionary_LSD2015)

polarity(data_dictionary_LSD2015) <- polar2
textstat_polarity(corp, dictionary = data_dictionary_LSD2015)
textstat_polarity(toks, dictionary = data_dictionary_LSD2015)
textstat_polarity(corp, dictionary = data_dictionary_LSD2015)
textstat_polarity(dfmat, dictionary = data_dictionary_LSD2015)

# with a user-supplied function
sent_fn <- function(x) (x[, "pos"] - x[, "neg"]) / rowSums(x) * 100
textstat_polarity(toks, data_dictionary_LSD2015, fun = sent_fn)
