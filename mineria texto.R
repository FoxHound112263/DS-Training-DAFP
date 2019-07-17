# Paquete para leer PDFs no escaneados
library(pdftools)

# Leer CONPES Big Data (subir primero en parte inferior derecha: files, upload)
texto_crudo <- pdf_text("3920.pdf")

# Dejar solo caracteres alfanuméricos
texto_crudo <-  gsub(pattern = "[^[:alnum:][:space:]]", " ", texto_crudo)

# Paquete con funciones de minería de texto
library(tm)

# Convertir texto crudo en corpus
corpus <-  VCorpus(VectorSource(texto_crudo))

# Remover stopwords
corpus <-  tm_map(corpus, removeWords, stopwords(kind = "sp"))

# Construir matriz de términos y documentos (en este caso páginas)
texto.tdm <- TermDocumentMatrix(corpus,control = list(removePunctuation = TRUE,
                                                      removeNumbers = TRUE,
                                                      tolower = TRUE,
                                                      trimws = TRUE))


# Lista de todas las palabras
ft <- findFreqTerms(texto.tdm)

# Tabla de frecuencias
ft.tdm <- as.matrix(texto.tdm[ft,])

# Sumar palabras repetidas
palabras_frecuentes <-  as.data.frame(sort(apply(ft.tdm, 1, sum), decreasing = TRUE))

# Nombrar columnas para que quede más presentable
palabras_frecuentes <- data.frame(palabra = rownames(palabras_frecuentes) , conteo = palabras_frecuentes$`sort(apply(ft.tdm, 1, sum), decreasing = TRUE)`)

# Primeras 50 palabras
primeras <- palabras_frecuentes[1:50,]

# Paquete con muchas funciones de otros paquetes para tratamiento de datos
library(tidyverse)

# Paquete de gráficos amigables
library(esquisse)

# Generar gráfico
esquisser(primeras)

# Graficar palabras más frecuentes ordenadas
ggplot(primeras) +
  aes(x = reorder(palabra,conteo) , weight = conteo) +
  geom_bar(fill = "#0d0887") +
  coord_flip() +
  theme_minimal()


# Paquete para nube de palabras
library(wordcloud2)

# 100 palabras más frecuentes
primeras <- palabras_frecuentes[1:100,]


# Generar gráfico
wordcloud2(data = primeras)

# Paleta de colores personalizada
custom_colors <- c("#005073", "#107dac", "#189ad3", "#1ebbd7", "#71c7ec")

# Nube de palabras con más trabajo
wordcloud2(primeras, size=0.7, 
           color=rep_len( custom_colors, nrow(primeras)),backgroundColor = "white",shape = 'circle')

#---------------------------------#
# Construcción de red de bigramas
#---------------------------------#

# Paquete para encontrar bigramas
library(tidytext)

# Función que contruí para preprocesamiento de texto
preproctext <- function(x){
  require(magrittr)
  x[which(is.na(x))] <- ""
  y <- x %>% 
    iconv(.,from="utf-8",to="ASCII//TRANSLIT") %>%
    gsub("[^[:print:]]", " ", .) %>%
    tolower %>% 
    gsub("[^[:lower:]^[:space:]]", " ", .) %>%
    gsub("[[:space:]]{1,}", " ", .) %>%
    trimws
  return(y)
}

# Preprocesar texto (Volvemos a preprocesarlo porque necesitamos un vector, no un objeto corpus)
texto_limpio <- preproctext(texto_crudo)

# Remover stopwords
texto_limpio <- removeWords(texto_limpio, stopwords("sp"))
# Convertir en tabla (tibble es casi lo mismo que un dataframe, pero se necesita para la función de encontrar bigramas)
texto_limpio <- tibble(texto = texto_limpio)

# Totalidad de los bigramas
bigramas <- texto_limpio %>% unnest_tokens(bigram, texto, token = "ngrams", n = 2)

# Frecuencia de bigramas
bigramas %>% count(bigram, sort = TRUE)

# Separar cada palabra de los bigramas en columnas
bigramas_separados <- bigramas %>% separate(bigram, c("word1", "word2"), sep = " ")

# Columna con frecuencia del bigrama
bigramas_conteo <- bigramas_separados %>% count(word1, word2, sort = TRUE)


# Paquete para crear objeto graficable
library(igraph)

# Objeto a graficar
bigram_graph <- bigramas_conteo[1:50,] %>% filter(n > 2) %>% 
  graph_from_data_frame()


# Paquete para graficar red
library(ggraph)

# Gráfico básico
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Gráfico más elaborado (esta es la representación gráfica de una cadena de Markov)
set.seed(11234)
a <- grid::arrow(type = 'closed', length = unit(.15, "inches"))
x11()
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F,
                 arrow = a,linemitre = 8, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "firebrick3", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('Red de bigramas más utilizados en CONPES 3920') +
  theme_void() +
  theme(plot.title=element_text(hjust=0.5))

