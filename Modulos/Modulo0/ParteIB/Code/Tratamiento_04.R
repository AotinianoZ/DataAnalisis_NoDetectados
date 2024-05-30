#### Geoanálisis de información con data RASTER ####
# ALGORITMOS BASICOS:

# carga de datos:

# Elevacion
elevation <- terra::rast(x = "Modulos/Modulo0/ParteIB/Geodatabase/Raster/DEM.tif")
elevation
str(elevation)
summary(elevation)
plot(elevation)

# dem - pendiente (wailung - fill sinks omitido):

slope <- terra::terrain(x = elevation, v="slope", neighbors=8, unit="degrees")
slope
str(slope)
summary(slope)
plot(slope)

par(mfrow=c(1,2))
plot(elevation)
plot(slope)

mapview(slope)

# Operaciones basicas
#plot de pendiente
summary(slope)
cortes <- c(13.90, 22.66, 22.38 , 31.00, 63.22)
colores <- c("green", "yellow", "orange","red")
plot(slope, 
     breaks = cortes, 
     col = colores,
     main="Slope")
# Recorte de un area
cropbox1 <- terra::draw()
# delimitar el nuevo corte
DEMcrop1 <- terra::crop(slope, cropbox1)
# plotear recorte con valores
plot(DEMcrop1,
     breaks = cortes, 
     col = colores,
     main ="Slope_crop")
text(DEMcrop1)

# Stack raster data:
tratamiento <- c(elevation, slope)

library(leafsync)
m1 <- mapview(elevation)
m2 <- mapview(slope)
sync(m1,m2, ncol = 1)

names(tratamiento) <- c("elevacion","pendiente")
tratamiento
plot(tratamiento)

cor(values(tratamiento)[ ,2], values(tratamiento)[ ,1], use = "na.or.complete")

lm1 <- lm(values(tratamiento)[ ,2] ~ values(tratamiento)[ ,1])
lm1
summary(lm1)

# Correlacion Geoespacial:
dir.create(path="Modulos/Modulo0/ParteIB/Geodatabase/Raster/Correlacion")

x <- terra::rast(tratamiento, 1) # elevacion
values(x) <- 1:ncell(tratamiento) # ajuste de celda (precaucion)
matriz_calculo <- values(tratamiento)

start <- Sys.time()
start
focal_cor <- terra::focal(
  x = x, 
  w = matrix(1,3,3),
  
  fun = function(x , y=matriz_calculo){
    cor(y[x, 1], y[x, 2], method = "spearman",
        use = "na.or.complete")
  },
  
  filename = file.path("Modulos/Modulo0/ParteIB/Geodatabase/Raster/Correlacion/focal_spearman.tif"),
  overwrite = TRUE
)
finish <- Sys.time()
finish

cor_map_pearson <-  terra::rast(x = "Modulos/Modulo0/ParteIB/Geodatabase/Raster/Correlacion/focal_pearson.tif")
cor_map_spearman <- terra::rast(x = "Modulos/Modulo0/ParteIB/Geodatabase/Raster/Correlacion/focal_spearman.tif")
plot(cor_map_pearson)
cropbox2 <- terra::draw()
DEMcrop2 <- terra::crop(cor_map_pearson, cropbox2)
plot(DEMcrop2)
plot(cor_map_spearman)
a <- mapview::mapview(cor_map_pearson, maxpixels =  4155072) 
b <- mapview::mapview(cor_map_spearman, maxpixels =  4155072)

leafsync::sync(a, b, ncol = 1)















