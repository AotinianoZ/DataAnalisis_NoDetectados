# Caso 1 : Data hidrogeologica (No spacial) ----

# Formato de dato : tablas (.csv, .txt, .xlsx, .xls, .dat, .tab entre otros)
# Informacion de recursos hídricos del INGEMMET sobre análisis de aguas.

# opcion
# aguas <- read.csv(file=file.choose(), header = TRUE, sep=",")# Esto es en caso se desee escoger un archivo.

# Carga y revisión de la estructura:
aguas <- read.csv(file="Modulos/Modulo0/ParteIB/Geodatabase/Datatable/agua_data.csv", header = TRUE, sep=",")
str(aguas)
summary(aguas)

#aguas$Codigo <- as.factor(aguas$Codigo) #ejemplo en una variable (no para automatizacion)
aguas <- aguas %>% mutate_if(is.character, as.factor)
str(aguas)
summary(aguas)
head(aguas, n=5)
tail(aguas, n=5)
colnames(aguas)
rownames(aguas)
rownames(aguas) <- paste("leyva", 1:353, sep = "_") # paste("leyva", 1:ncol(aguas), sep="_")
rownames(aguas)

# Filtrar informacion

# Valores de NA de calcio:
aguas %>% filter(is.na(Ca)) %>% select(Codigo, pH)
aguas %>% filter(!is.na(Ca), pH > 7.00) %>% select(Codigo, pH, Temporada)

# Filtrar pH mayor a la media aritmetica de pH,
# que no tenga vacios en Ca ni en SO4, y lo agrupe por
# temporada y tipo de fuente. Después, que sintice
# la cantidad de datos con las estadísticas de
# conductividad elétrica CE (minimo, media aritmetica, mediana y maximo)
# ponerlo ordenado por conteo.

aguas %>% 
  filter(pH > mean(pH), !is.na(Ca), !is.na(SO4)) %>%
  group_by(Temporada, Tipo_Fuente) %>%
  summarise(
    conteo = n(),
    Ce_min = min(CE),
    Ce_media = mean(CE),
    Ce_mediana = median(CE),
    Ce_max = max(CE)) %>%
  arrange(desc(conteo))

# Pre-tratamiento de los NA o data incompleta
# (tratamiento: imputación, aproximación ML, semiparmetricos)
filtro1 <- aguas %>% 
  filter(pH > mean(pH, na.rm=TRUE), !is.na(Ca), !is.na(SO4)) %>%
  group_by(Temporada, Tipo_Fuente) %>%
  summarise(
    conteo = n(),
    Ce_min = min(CE, na.rm=TRUE),
    Ce_media = mean(CE, na.rm=TRUE),
    Ce_mediana = median(CE, na.rm=TRUE),
    Ce_max = max(CE, na.rm=TRUE)) %>%
  arrange(desc(conteo))

print(as_tibble(aguas %>% 
                  select(Codigo, Tipo_Fuente, pH) %>%
                  filter(pH<6) %>%
                  arrange(desc(!pH))))

# escribir archivo:
write.csv(filtro1, file = "Primerfiltro.csv")
head(aguas)
colnames(aguas)

# Convirtiendo a data espacial:
# Plotear con sistema de referencia coordenado (long, lat) - geograficas
# Transformar la informacion en data espacial - forma antigua
# El CRS(Coordinate Reference System) es "WGS Zone 19S"

data_coor <- aguas[ ,c("Norte","Este")]
# Otras formas
# aguas[ ,2:3]
# aguas %>% dplyr::select(Norte, Este)

data_coor <- data_coor[ ,order(c(names(data_coor)))]
sputm <- SpatialPoints(coords = data_coor, 
                       proj4string = CRS("+proj=utm +zone=19 +south +datum=WGS84"))
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo <- as.data.frame(spgeo)
colnames(spgeo) <- c("longitud","latitud")
aguas2 <- cbind(aguas, spgeo)
str(aguas2)
plot(aguas2)

# Transformacionespacial sintetizada
aguas_espacial <- aguas2 %>% # Para Perú el CRS:4326 es el WGS84.
  sf::st_as_sf(coords = c("longitud","latitud"), crs = 4326)
aguas_espacial
write.csv(x = aguas_espacial, file="agua_espacial.csv")

# Visualización - filtrado
mapview::mapview(aguas_espacial)
filtrado_espacial <- aguas_espacial %>% filter(is.na(Ca)) %>% select(Codigo, pH)
mapview::mapview(filtrado_espacial)

variables <- c("Na","Tipo_Fuente")
agua2 <- aguas %>% select(any_of(variables), starts_with("C"))
head(agua2)

agua3 <- aguas %>% drop_na(any_of(variables)) # eliminar vacios
head(agua2)


####Caso2: Data hidrogeologica pozos - semi-automatización (Generar graficos en pozos) ----

# Import dataframe
df <- read.csv("Modulos/Modulo0/ParteIB/Geodatabase/Datatable/wells_dataset.csv", sep = "\t", dec = ".")

# El formato de data de tiempo: day/month/year hour/minutes
library(lubridate) # lubridate es muy eficeinte para trabajar con data tiempo.

df$DATE <- dmy_hm(df$DATE)
str(df)

df <- df %>% mutate_if(is.character, as.factor) # To change automatically all characters to factors
str(df)

# Dataframe functions
str(df)
colnames(df)
rownames(df)
summary(df)
head(df)

unique_piezometers <- unique(df$PIEZOMETER)

# Data for piezometer
for (piezometer in unique_piezometers) {
  piezometer_data <- df %>% # I changed the code to make it tidy
    filter(PIEZOMETER == piezometer, STATUS == "Operative" & WATER < TERRAIN, !is.na(WATER)) 
  
  # Verify data for piezometer
  if (nrow(piezometer_data) > 0) {
    # Statistical calculations
    mean_value <- mean(piezometer_data$WATER, na.rm = TRUE)
    median_value <- median(piezometer_data$WATER, na.rm = TRUE)
    sd_value <- sd(piezometer_data$WATER, na.rm = TRUE)
    
    cat("Piezometer:", piezometer, "\n")
    cat("WATER mean:", mean_value, "\n")
    cat("WATER median:", median_value, "\n")
    cat("WATER sd:", sd_value, "\n")
    
    # Create and save results
    p <- ggplot(piezometer_data, aes(x = DATE, y = WATER, group = 1)) +
      geom_line() +
      labs(title = paste("Hydrogram - ", piezometer),
           x = "DATE", y = "WATER")
    
    dir.create("Modulos/Modulo0/ParteIB/Geodatabase/resultados/") # Cabiar para crear un directorio en rstudio
    save_path <- file.path("Modulos/Modulo0/ParteIB/Geodatabase/resultados/", paste("Hydrogram_", piezometer, ".png", sep = ""))
    
    ggsave(filename = save_path, plot = p, width = 10, height = 6, dpi = 300)
    
    cat("endddd\n")
  } else {
    cat("Piezometer:", piezometer, "no valid data for 'Operative' with 'WATER < TERRAIN'.\n")
  }
}

# Caso 3: Información de peligros geológicos en el Peru (Non-spatial)  ----

Peligros <- read.csv("Modulos/Modulo0/ParteIB/Geodatabase/Datatable/Peligros_geologicos.csv",header=TRUE)

View(Peligros)
Peligros <- Peligros %>% mutate_if(is.character, as.factor)
str(Peligros)
colnames(Peligros)
rownames(Peligros)
head(Peligros, 5)
tail(Peligros, 5)

summary(Peligros)

Peligros[Peligros==""] <- NA # estaba la dato (celda) vacia
str(Peligros)
summary(Peligros)
Peligros2 <- Peligros %>% drop_na(any_of(c("TIPO_PELIG","NOMBRE")))
summary(Peligros2)

colnames(Peligros2)
Peligros2 <- Peligros2 %>% 
  dplyr::rename(code = "CODIGO_INT", Dpto="NOM_DPTO",
                Prov = "NOM_PROV", T_Pel = "TIPO_PELIG",
                N_esp = "NOMBRE_ESP", m = "Reclass_Sl",
                h = "MapaDelPer", Ab_Fm = "UNIDAD",
                subcuenca = "Nom_SubCu", cuenca = "NOMBRE" )
colnames(Peligros2)

# Asegurar que la variable cuenca no esta vacia y que la pendiente
# no es 0.
Peligros2 <- Peligros2 %>% filter(!is.na(cuenca), !m=="0")
summary(Peligros2)

# Generar filtros:
# a) Agrupar por T_Pel y N_Esp en ese orden,
# y sintetizra el conteo, min, quantiles y maximo de 
# la informacion. Adicionalmente, ordenar acorde al conteo.

Filtro1 <- Peligros2 %>%
  group_by(T_Pel, N_esp) %>%
  summarise(
    conteo = n(),
    min.h = min(h, na.rm = FALSE),
    h.25 = quantile(h, probs = 0.25, na.rm=FALSE),
    h.50 = quantile(h, probs = 0.50, na.rm=FALSE), #mediana
    h.75 = quantile(h, probs = 0.75, na.rm=FALSE),
    max.h = max(h, na.rm = FALSE)
  ) %>%
  arrange(desc(conteo))
Filtro1

datatable(Filtro1,
          extensions = c(
            "Buttons",
            "Scroller"
          ), 
          rownames = FALSE,
          filter = "top",
          options = list(
            dom = "Blrtip",
            deferRender = TRUE,
            buttons = list(I("colvis"),'copy', 'csv', 'excel', 'pdf', 'print')
          ))

# Buscando correlacion entre variables altura y pendiente:
Correlation <- Peligros2 %>% dplyr::select(h, m)
str(Correlation)
summary(Correlation)
head(Correlation)
levels(Correlation$m)
# d2 <- xtabs(~ m + h, data = Correlation) #Genera tabla
Correlation$m <- as.factor(Correlation$m) # debido a que fue clasificado el valor de pendiente
summary(Correlation)

one.way <- aov(h~m, data=Correlation)
summary(one.way)

# La pendiente hallada tiene una relación con la altura calculada.
par(mfrow=c(2,2))
plot(one.way)

str(Peligros2)
colnames(Peligros2)

Peligros2 %>%
  group_by(N_esp, Dpto, m) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

unique(Peligros2$cuenca)
unique(Peligros2$Ab_Fm)
Peligros2 %>% 
  filter(cuenca == "Cuenca Pampas", Ab_Fm =="Deposito fluvial") %>%
  group_by(T_Pel, m) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(m))

Peligros2 %>%
  mutate(N_esp = str_replace(N_esp, "Caída de roc", "Rock Fall"))  %>%
  group_by(N_esp, Dpto, m) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

Peligros2 %>%
  group_by(Dpto) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

