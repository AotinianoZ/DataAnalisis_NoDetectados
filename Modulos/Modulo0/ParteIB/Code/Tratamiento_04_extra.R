#### RASTER DATA EXTRA: ####

# Carga de data raster:
elevation <- rast(x = "Modulos/Modulo0/ParteIB/Geodatabase/Raster/DEM.tif")
slope <- rast(x="Modulos/Modulo0/ParteIB/Geodatabase/Raster/SLOPE.tif")
# generando aspecto
aspect <-  terra::terrain(x = slope, v="aspect", neighbors=8, unit="degrees")
aspect
rugosidad <- terra::terrain(x = elevation, v ="roughness", neighbors = 8, unit = "degrees")
rugosidad
flowdirection <- terra::terrain(x = elevation, v ="flowdir")
flowdirection

par(mfrow=c(3,2))
plot(elevation)
plot(slope)
plot(aspect)
plot(rugosidad)
plot(flowdirection)

# algunas caracteristicas:
elevation
str(elevation)
hist(elevation[])

# revisar atributos y extension de projeccion
terra::ext(elevation)
terra::ext(slope)
terra::ext(aspect)

# en caso tenemos diferentes extensiones, luego tratar resample usando la menor area:
elevation_r <- resample(elevation, slope, method="bilinear")
aspect_r <- resample(aspect, slope, method="bilinear")

# revisar:
ext(elevation_r) # check the new extent
ext(aspect_r)
ext(slope)

# escribir nuevos raster:

dir.create(path = "Modulos/Modulo0/ParteIB/Geodatabase/Resample/")
writeRaster(x = elevation_r, filename = "Modulos/Modulo0/ParteIB/Geodatabase/Resample/elevation_r.tif", overwrite=TRUE)
writeRaster(x = aspect_r, filename = "Modulos/Modulo0/ParteIB/Geodatabase/Resample/aspect_r.tif", overwrite=TRUE)
writeRaster(x = slope, filename = "Modulos/Modulo0/ParteIB/Geodatabase/Resample/slope.tif", overwrite=TRUE)
writeRaster(x = rugosidad, filename = "Modulos/Modulo0/ParteIB/Geodatabase/Resample/rugosidad.tif", overwrite=TRUE)
writeRaster(x = flowdirection, filename = "Modulos/Modulo0/ParteIB/Geodatabase/Resample/flowdirection.tif", overwrite=TRUE)

# stack informacion raster:

stack_list <- list.files(path="Modulos/Modulo0/ParteIB/Geodatabase/Resample/", pattern = "tif$", full.names = TRUE)
raster_stack <- rast(stack_list)
raster_stack
names(raster_stack)

# Convertir el raster a dataframe con long-lat ("coordenadas geograficas")

Rasters.df <- as.data.frame(raster_stack, xy=TRUE, na.rm=TRUE)
head(Rasters.df, 5)

# Trabajando con Aspecto:

Rasters.df_N <- rasters.df[ ,c(-1,-2)] # remover x,y
Aspectreclas <- cut(Rasters.df_N$aspect, seq(0,361,45), right=FALSE,
                    labels =c("45","90","135","180","225","270","315","360"))
table(Aspectreclas)
Aspectreclas <- factor(Aspectreclas)
flagsras = data.frame(Reduce(cbind, 
                             lapply(levels(Aspectreclas), function(x){(Aspectreclas == x)*1}) # one hot encoding.
))
names(flagsras) = levels(Aspectreclas)

Rasters.df_N <- cbind(Rasters.df_N, flagsras)
# Rasters.df_N <- drop_na(Rasters.df_N) # Comodidad


# Remover aspecto original
colnames(Rasters.df_N)
Rasters.df_N <- Rasters.df_N[,-1]
str(Rasters.df_N)


# En caso de modelado necesitamos escalar-estandarizar variables (es solo uno de varios pasos):
maxss <- apply(Rasters.df_N[ ,1:4], 2, max)
minss <- apply(Rasters.df_N[ ,1:4],2, min)

Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df_N[ ,1:4], 
                                           center = minss, 
                                           scale = maxss - minss))
head(Rasters.df_N)
head(Rasters.df_N_scaled)
summary(Rasters.df_N_scaled)

Rasters.df_final <- data.frame(cbind(Rasters.df[ ,c(1,2)], # x e y
                                     Rasters.df_N[ ,c(5:12)], # aspecto reclasificado
                                     Rasters.df_N_scaled)) #,  # variables escaladas
head(Rasters.df_final)

#### Modelamiento predictivo (Simulacion)####

compute.Cu <- rnorm(n = nrow(Rasters.df_final), mean = 2.45, sd =1)
str(compute.Cu)
out_Cu <- data.frame(cbind(Rasters.df_final, compute.Cu))
str(out_Cu)
colnames(out_Cu)[15] <- "Cu_modelado"

out_M <- out_Cu[ ,c(1,2,15)]
head(out_M)

# transformar a raster
out_sm_raster <- raster::rasterFromXYZ(out_M)
library(rasterVis)
levelplot(out_sm_raster)

# Dandole sistema de referencia coordenado:
terra::crs(slope, describe=TRUE, proj=TRUE)
terra::crs(out_sm_raster) <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs"
out_sm_raster
plot(out_sm_raster)
library(rasterVis)
levelplot(out_sm_raster)

# escribir el nuevo geotiff:
writeRaster(out_sm_raster, filename="Modulos/Modulo0/ParteIB/Geodatabase/Resample/cu_model.tif", overwrite=TRUE) 
mapview::mapview(out_sm_raster)





