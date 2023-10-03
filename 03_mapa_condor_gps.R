rm(list=ls())

# Movimientos de cóndor en Ecuador

library(rgbif)
library(rnaturalearth)
library(ggplot2)
library(paletteer)
library(sp)
library(sf)
library(cowplot) 
library(ggspatial)
library(ggpubr)
library(marmap)
library(ggnewscale) 

### Para bajar esta base de datos tenemos que conocer su código
data_condor <- occ_data(datasetKey = "a20fc223-7e35-40f7-b6b9-c5d2bd7c119c") 

# cómo citarlo se obtiene así, muy fácil!
gbif_citation(x= data_condor)


# Ahora, las columnas de la base de datos esta son muchas; vamos a elegir las que nos interesan
# posición geográfica y especie, de manera de tener nuestra tabla de registros
variables <- c("decimalLatitude", "decimalLongitude", "eventDate", "eventID")

# y ahora genero mi base de datos seleccionando sólo lo que quiero
condor <- select(data_condor$data, variables)

# a ver cómo se ve
head(condor)

# podemos ver cuantos datos hay por cada individuo
table(condor$eventID)

# necesitamos que la fecha de cada registro sea en un formato específico
# (esto es todo un mundo en análisis de datos de movimiento)
condor$fecha <- as.POSIXct(condor$eventDate, origin = "1960-01-01") 

# Para este análisis, voy a crear una variable que sea 
# fecha desde la liberación de cada individuo -no sé si esto es preciso, es un ejemplo-
# formalmente sería "fecha desde el primer registro"
condor <- condor %>% arrange(fecha) %>% 
  group_by(eventID) %>% mutate(
  fecha = as.Date(fecha, format = "%Y-%m-%d"),
  dia_monitoreo = as.integer(fecha - fecha[1] + 1))

# ver el rango de días de monitoreo
range(condor$dia_monitoreo)

# tengo que convertir los datos de esta tabala a objetos espaciales, 
# especificando que columnas son posiciones y qué proyección voy a usar
crs_mercator <- "+proj=longlat +datum=WGS84 +no_defs"

data_sp <- SpatialPointsDataFrame(coords = cbind(condor$decimalLongitude, 
                                                 condor$decimalLatitude), 
                                  data = condor,
                                  proj4string = CRS(crs_mercator))
data_sp <- st_as_sf(data_sp) # necesito este formato para lo que sigue

# vamos a armar el mapa base como en el script 01
extent_ecuador <- as(extent(-83,-75,-6, 2), 'SpatialPolygons')

ecuador <- rnaturalearth::ne_countries(scale = "large", country = "Ecuador", returnclass = "sf")
ecuador_sin_galapagos <- st_crop(ecuador, extent_ecuador) #recortar a la extensión

alt <- raster(x= "data/alt.bil") 
alt_ecuador <- crop(alt, ecuador_sin_galapagos) 
alt_ecuador <- as.data.frame(alt_ecuador, xy=T) 

base <- ggplot(data = ecuador_sin_galapagos) +
  geom_raster(data = alt_ecuador , aes(x = x, y = y, fill = alt), show.legend=F) +
  scale_fill_etopo() + 
  geom_sf(color = "black", fill = NA) +
  xlab("") + ylab("") +
  theme_bw()

base

# plot de tres individuos distintos (los que más datos tienen)

morro <- base + 
  geom_path(data = data_sp %>% filter(eventID %in% "MORRO"), aes(x = decimalLongitude, y = decimalLatitude)) +
  new_scale_fill() +
  geom_sf(data = data_sp %>% filter(eventID %in% "MORRO"), aes(fill = dia_monitoreo), 
          size=4, alpha = 0.8, shape=21) +
  scale_fill_continuous("Días post-liberación", type = "viridis") +
  draw_image("data/vultur_fly.png", x = -77, y = -4, scale = 3) +
  scale_x_continuous(name="Longitude") +
  scale_y_continuous(name="Latitude") +
  theme(axis.text = element_text(size = 10)) + 
  annotation_scale()

morro
