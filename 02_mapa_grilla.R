rm(list=ls())

# Mapa a partir de una grilla de presencias

# paquetes que voy a usar
library(rgbif)
library(tidyverse)
library(sp)
library(letsR)

# para bajar datps de gbif necesito buscar en la página cuáles son los id de cada especie
# lo lindo es que se puede hacer a cualquier nivel
# por ejemplo, voy a bajar todas las especies de falcónidos de África

#Bajar datos de GBIF
falconidae <- 5240 #taxonKey

# como es sólo un ejemplo, vamos a bajar los datos de un sólo año
registros <- occ_data(taxonKey = falconidae, # Key from GBIF 
                          year = 2018,
                          continent= "africa",
                          hasCoordinate = T, # solo datos georreferenciados
                          limit = 100) # limite de datos a buscar (importante porque nunca sabemos cuánto puede tardar)

# Ahora, las columnas de la base de datos esta son muchas; vamos a elegir las que nos interesan
# posición geográfica y especie, de manera de tener nuestra tabla de registros
variables <- c("decimalLatitude", "decimalLongitude", "scientificName")


# y ahora genero mi base de datos seleccionando sólo lo que quiero
falconidae_africa <- select(registros$data, variables)

# a ver cómo se ve
head(falconidae_africa) # armoso
# puedo ver cuántos registros tengo de cada especie con este comando
table(falconidae_africa$scientificName)

# si quisieran guardar la base de datos para la próxima
write.csv(falconidae_africa, file = "archivo.csv")

# Esto es importante! HAY que citar toda base de datos que obtenga de GBIF
# y el paquete de R, y cualquier base de datos que se haya usado para obtenerla!
# con este comando obtienen esas citas
gbif_citation(x= registros)


# Ahora genero una matriz de 5 grados x 5 grados con presencias/ausencias de cada especie

xy <- falconidae_africa %>% 
  dplyr::select(decimalLongitude, decimalLatitude) 

falconidae_grid <- lets.presab.points(xy, falconidae_africa$scientificName,
                                    xmn = -20,
                                    xmx = 50,
                                    ymn = -40,
                                    ymx = 45,
                                    remove.cells = TRUE,
                                    resol = 2.5,)

# podría graficar una especie en particular, por ejemplo esta
plot(falconidae_grid, main="Falco tinnunculus", name = "Falco tinnunculus Linnaeus, 1758") 

# pero, si no le digo una especie, por default obtengo la superposición
plot(falconidae_grid, main="Riqueza de falcónidos en África (# de especies)")
