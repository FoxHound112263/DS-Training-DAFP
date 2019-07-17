# ---------------- #
# Intro básica a R #
# ---------------- #

# Funciones
sqrt(144)

# Asignación
mensaje <- "hola mundo"

# Imprimir en consola
mensaje

# Cargar datos de homicidios en Bogotá 2017
homicidios <- read.csv("https://github.com/FoxHound112263/DS-Training-DAFP/raw/master/data/homicidios.txt",sep = "\t",fileEncoding = "UTF-8", encoding = "UTF-8",stringsAsFactors = FALSE)

# Esto es un dataframe
homicidios

# Dimensión: filas y columnas
dim(homicidios)

# Columnas
names(homicidios)

# Primeros datos
head(homicidios)

# Con '$' se puede obtener las columnas de la base
homicidios$Localidad

# Con los paréntesis cuadrados se puede obtener filas o columnas de la base
homicidios[46,2]

# Cambiar dato faltante
homicidios$Localidad[homicidios$Localidad == "Sin información" ] <- "Sumapaz"

# Convertir columna de frecuencias a numérico
homicidios$Total <- as.numeric(homicidios$Total)

# Paquete con funciones de otros paquetes para el tratamiento de datos
library(tidyverse)

# Nueva base
homicidios_fix <- homicidios %>%
  # Agrupar por
  group_by(Localidad) %>%
  # Dejar el valor máximo
  filter(Total == max(Total, na.rm = T) ) %>%
  # Desagrupar
  ungroup() %>% 
  # Remover duplicados
  unique()

# Eliminar última fila
homicidios_fix <-  homicidios_fix[-nrow(homicidios_fix), ]


#---------------#
# VISUALIZACIÓN #
#---------------#

# Gráfico con R base
barplot(height = homicidios_fix$Total,
        names.arg = homicidios_fix$Localidad,
        col = 1:20,
        main = "Homicidios por localidad")

# Gráfico con ggplot
ggplot(data = homicidios_fix) +
  geom_col(mapping = aes(x = factor(Localidad), y = Total, fill= Localidad)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-------#
# Mapas #
#-------#

# Paquete para tratar con datos espaciales
library(sf)

# geojson con localidades de Bogotá
localidades <- st_read('https://github.com/FoxHound112263/DS-Training-DAFP/raw/master/data/bta_localidades.json')

names(localidades)

localidades$NOMBRE

# Mapa rápido
ggplot(localidades) + 
  geom_sf()


# Mapa con homicidios
ggplot(localidades) +
  geom_sf(aes(fill = NOMBRE))

# Agregar datos a localidades
nueva_columna <- c("Norte", "Sur", "Sur", "Norte", "Norte", "Sur", "Centro", "Occidente", "Sur", "Sur", "Norte", "Norte", "Norte", "Sur", "Sur","Sur","Sur", "Centro", "Sur", "Sur")

# Pegar nueva columna
localidades <- mutate(localidades, ubicación = nueva_columna)

# En el mapa
ggplot(localidades) +
  geom_sf(aes(fill = ubicación))

# Colorear por homicidios
ggplot(localidades) +
  geom_sf(aes(fill = homicidios_fix$Total)) +
  scale_fill_distiller(palette = "Spectral")

# Hacer igual ambas columnas
homicidios_fix$Localidad <- toupper(homicidios_fix$Localidad) 

# Verificar que todas las localidades estén
homicidios_fix$Localidad %in% localidades$NOMBRE

# Igualar nombre faltante
homicidios_fix$Localidad[homicidios_fix$Localidad == "LA CANDELARIA" ] <- "CANDELARIA"

# Ordenar uno con base en otro
homicidios_fix <-  homicidios_fix[order(match(homicidios_fix$Localidad,localidades$NOMBRE )), ]

# Agregar ubicación a gráfico de barras
homicidios_fix <- mutate(homicidios_fix, ubicación = nueva_columna)

# Gráfico full
ggplot(homicidios_fix) +
  geom_col(aes(x = reorder(Localidad,Total), y = Total, fill = ubicación)) +
  geom_text(aes(x = Localidad, y = Total, label = Total), nudge_y = 5) +
  labs(title = "Homicidios en Bogotá por localidad",
       subtitle = "Año 2017",
       y = "Total") +
  coord_flip() +
  # Fondo y panel blanco
  theme(panel.grid = element_blank(), panel.background = element_blank())

# Paquete para gráficos amigables
library(esquisse)

# Generar gráfico
esquisser(data = homicidios_fix)

# Otro ejemplo con base genérica
data(iris)
esquisser(iris)


# Mapa mundial rápido
mundo <- map_data(map = "world")

ggplot(data = mundo,mapping = aes(x = long,y=lat,group=group))+
  geom_polygon(fill='white',color='black')

ggplot(data = mundo,mapping = aes(x = long,y=lat,group=group))+
  geom_polygon(fill=mundo$group,color='black') +
  coord_quickmap()

  