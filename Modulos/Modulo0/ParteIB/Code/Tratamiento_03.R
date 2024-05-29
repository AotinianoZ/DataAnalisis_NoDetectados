#### TRABAJANDO CON SHAPEFILES ####
# ALGORITMOS BASICOS:

# Upload data Amazonas:

amazonas <- sf::st_read(dsn="Modulos/Modulo0/ParteIB/Geodatabase/Shapefile/Amazonas/litoutm.shp")
plot(amazonas)
mapview::mapview(amazonas, zcol="formacion")
str(amazonas)
head(amazonas)
tail(amazonas)

# rename and summary:
amazonas <- amazonas %>% dplyr::rename(litologia=formacion) %>% dplyr::select(litologia)
str(amazonas)
head(amazonas)
tail(amazonas)
summary(as.factor(amazonas$litologia))
levels(as.factor(amazonas$litologia))
nlevels(as.factor(amazonas$litologia))

# Upload data Ancash:

ancash <- sf::st_read(dsn="Modulos/Modulo0/ParteIB/Geodatabase/Shapefile/Ancash/lito.shp")
str(ancash)
head(ancash)
# plot(ancash) : Mucho procesamiento computacional.
mapview::mapview(ancash, zcol="Subunid")
ancash <- ancash %>% dplyr::rename(litologia=Subunid) %>% dplyr::select(litologia)

# Visualizacion de varias capas y merge:
mapview(amazonas) + mapview(ancash)

uniones <- rbind(amazonas, ancash)
uniones
str(uniones)
uniones <- uniones %>% dplyr::mutate_if(is.character, as.factor)
uniones <- na.omit(uniones)
summary(uniones)
sort(summary(uniones$litologia))
plot(uniones)
mapview(uniones)

# Escribir capa final:
st_write(uniones, dsn="union.shp")

Sys.time()
df <- uniones %>% group_by(litologia) %>% summarise(conteo = n()) %>% arrange(desc(conteo))
df # Es regularmente pesado tratar información con su CRS y la informacion interna
Sys.time()
# Setear geometria a NULL para hacerlo ligero:
Sys.time()
df <- uniones %>% st_set_geometry(NULL) %>% group_by(litologia) %>% summarise(conteo = n()) %>% arrange(desc(conteo))
df
Sys.time()
nrow(df)
etiqueta_conteo <- sum(df$conteo)

# Finalmente carga data shapefile de peligros geologicos en el Peru:

peligros <- st_read(dsn="Modulos/Modulo0/ParteIB/Geodatabase/Shapefile/Peligros_shapefile/peligrosgeo.shp")
str(peligros)
peligros
head(peligros)
View(peligros)
mapview(peligros)

# setear CRS:
peligros <- st_transform(peligros, crs = st_crs(uniones))

mapview(uniones) + mapview(peligros)

# Generate Intersection
Sys.time()
points_within_polygon <- st_intersection(peligros, uniones)
Sys.time()
m <- mapview(points_within_polygon)
library(leafem)
leafem::addLogo(m, "https://jeroenooms.github.io/images/banana.gif",
                position = "bottomleft",
                offset.x = 5,
                offset.y = 40,
                width = 100,
                height = 100)
plot(st_geometry(uniones), col = 'lightblue', main = "Points within Polygon")
plot(st_geometry(points_within_polygon), col = 'red', add = TRUE)
plot(st_geometry(peligros), col = 'blue', add = TRUE)


library(leafsync)
m1 <- mapview(uniones)
m2 <- mapview(points_within_polygon)
sync(m1,m2, ncol = 1)
