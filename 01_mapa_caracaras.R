rm(list=ls())

# PAQUETES necesarios (se bajan con el comando "install.packages")
library(marmap)
library(cowplot)
library(ggplot2)
library(rnaturalearth) 
library(ggspatial)
library(raster)
library(sf)
library(rgdal)

# PROYECCIONES que pueden ser útiles (siempre está bueno tener a mano un par)
# South America Albers Equal Area
crs_102033 <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
#  mercator
crs_mercator <- "+proj=longlat +datum=WGS84 +no_defs"

# Extensión del proyecto 
# Creamos un objeto espacial para luego delimitar otros objetos a esa extensión
# (importante para acotar cualquier procesamiento si se bajaron capas globales)
extent_sudamerica <- as(extent(-80,-40,-60, -15), 'SpatialPolygons')

### DATOS ####

# 1. Shapefiles, en este caso distribución de algunas especies (IUCN)
albogularis <- st_read("data/phalcoboenus_albogularis.shp") #######
megalopterus <- st_read("data/phalcoboenus_megalopterus.shp") #######

# si quisiéramos cambiar la proyección de los shapefiles, sería con este comando
# en este caso la proyección de entrada y de salida son las mismas, correr o no estas líneas no cambia nada
albogularis <- st_as_sf(albogularis, 
                        crs = 4326, agr = "constant") %>% #le tengo que decir en el que está (4326 es otra forma de decir mercator)
  st_transform(crs = crs_mercator)

megalopterus <- st_as_sf(megalopterus, 
                         crs = 4326, agr = "constant") %>% 
  st_transform(crs = crs_mercator)

# 2. Puntos a partir de un csv (por ejemplo, sitios de muestreo)
data <- read.csv("data/sitios_caracara.csv", sep = ";") 

# la tabla es fea a propósito, sólo voy a sacarle la información espacial
data

# con esta línea genero el objeto espacial de los datos 
# (le digo qué columnas son coordenadas y cuál es la proyección)
sitios <- st_as_sf(data, coords = c("x", "y"), 
                               crs = crs_mercator, agr = "constant") %>% 
  st_transform(crs = crs_mercator)

# 3. Rasters, como una capa de altura
alt <- raster(x= "data/alt.bil") # (World Clim)
plot(alt) # ver cómo se ve
alt_sudamerica <- crop(alt, extent_sudamerica) #recortar según extensión del proyecto
plot(alt_sudamerica) # ver si está bien
alt_sudamerica <- as.data.frame(alt_sudamerica, xy=T) #muchos análisis lo requieren como dataframe
 
# bajar fronteras de países a nivel global usando rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")
sf::sf_use_s2(FALSE)
cropped_map <- st_crop(world, extent_sudamerica) #recortar a la extensión
plot(cropped_map$geometry) # a ver cómo se ve...
# esto es por comodidad también, sé que para tener consistencia luego me conviene
# recortar el shapefile de una especie cuya distribución se extiende más allá
# de la extensión del proyecto
megalopterus <- st_crop(megalopterus, extent_sudamerica)


# La estrategia con ggplot2 suele ser armar un mapa base que nos guste y a ese agregarle los datos
# prueben también con "show.legend=T"
base <- ggplot(data = cropped_map) +
  geom_raster(data = alt_sudamerica , aes(x = x, y = y, fill = alt), show.legend=F) +
  scale_fill_etopo() + #esta escala de colores es lo más
  geom_sf(color = "black", fill = NA) +
  xlab("") + ylab("") +
  theme_bw()

base

# luego, para hacer el mapa que quiero, sumo capas al mapa base

base +
  geom_sf(data = megalopterus, color="darkorange4", fill="orange", lwd=1, alpha = 0.3) +
  geom_sf(data = albogularis, color="red4", fill="tomato4", lwd=1, alpha = 0.3) +
  geom_sf(data = sitios, linewidth=3, col = "yellow") + 
  draw_image("data/carancho_austral.png", x = -50, y = -40, scale = 10) + # agregar una imagen
  annotate(geom = "rect", ymax = -39, ymin = -42, xmax = -72.5, xmin = -70, # hacer un recuadro en el mapa
           colour = "blue", size=1, fill = NA) +
  annotate(geom = "text", x = -75, y = -55.7, label = "P. albogularis", # escribir 
           fontface = "italic", color = "grey22", size = 4) +
  annotate(geom = "text", x = -69, y = -38, label = "Contact zone", 
           fontface = "bold.italic", color = "black", size = 4) +
  annotate(geom = "text", x = -70, y = -24, label = "P. megalopterus", 
           fontface = "italic", color = "grey22", size = 4) +
  annotation_scale(location = "bl", width_hint = 0.2) + # escala espacial
  annotation_north_arrow(location = "tr", which_north = "true", # Rosa de los vientos
                         style = north_arrow_fancy_orienteering) +
  theme(rect = element_rect(fill = "transparent"), # estilo general del mapa
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))

