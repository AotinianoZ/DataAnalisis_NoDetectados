#### Caso 4: Conexion API de Nasa (NASAPOWER) -----

library(nasapower)

ag_d <- get_power(
  community = "ag",
  lonlat = c(-77.02824, -12.04318),
  pars = c("RH2M", "T2M", "PRECTOTCORR"),
  dates = c("2021-01-01", "2021-12-31"),
  temporal_api = "hourly"
)
ag_d2 <- ag_d # conservar data inicial

str(ag_d)

ag_d <- ag_d %>% 
  rename(precipitacion = PRECTOTCORR,
         temperatura = T2M,
         humedad_relativa = RH2M)
head(ag_d)

estadistica <- ag_d %>%
  group_by(YEAR, MO, DY) %>%
  summarise(conteo = n(),
            minimo = min(precipitacion, na.rm=TRUE), 
            media = mean(precipitacion, na.rm=TRUE),
            maximo = max(precipitacion, na.rm=TRUE),
            acumulado_diario = sum(precipitacion, na.rm = TRUE))

# lubridate (tiempo)
estadistica$time <- make_datetime(year = estadistica$YEAR,
                                  month = estadistica$MO,
                                  day = estadistica$DY)
str(estadistica)
estadistica %>% 
  ggplot()+
  geom_line(aes(x=time, y =acumulado_diario))+
  geom_point(aes(x=time, y=acumulado_diario))

ag_d2$time <- make_datetime(year =ag_d2$YEAR,
                            month = ag_d2$MO,
                            day = ag_d2$DY,
                            hour = ag_d$HR)

dataRange <- c(as.Date("2021-04-01"), as.Date("2021-04-20"))

ag_d2 %>%
  filter(time >= dataRange[1] & time <= dataRange[2]) %>%
  ggplot()+
  geom_line(aes(x=time, y =PRECTOTCORR))+
  geom_point(aes(x=time, y=PRECTOTCORR))

ag_d2 %>%
  filter(time >= dataRange[1] & time <= dataRange[2]) %>%
  ggplot()+
  geom_line(aes(x=time, y =T2M))+
  geom_point(aes(x=time, y=T2M))

#### Caso 5: Compilación de información .xlsx para modelamiento ----

data_train <- readxl::read_xlsx(path = "Modulos/Modulo0/ParteIB/Geodatabase/Datatable/MachineLearning/Training2.xlsx", col_names = TRUE)
str(data_train)
head(data_train)

data_train <- data_train %>% dplyr::select(MM, Codi_Geo_Reclas,
                                           Area_Geo_Cuen, Area_Cuen,
                                           NAME)
summary(data_train)
data_train <- data_train %>% mutate_if(is.character, as.factor)
summary(data_train)
(data_train <- na.omit(data_train))

head(data_train)
nrow(data_train)
head(data_train)

asignation <- readxl::read_xlsx(path = "Modulos/Modulo0/ParteIB/Geodatabase/Datatable/MachineLearning/tabla_de_unid_lito.xlsx",col_names = TRUE)
head(asignation)
asignation <- asignation[!duplicated(asignation), ]

# revisar nombres comunes
common_names <- intersect(data_train$NAME, asignation$NAME)

asignation <- asignation %>% filter(NAME %in% common_names)
asignation

merge_df <- merge(data_train, asignation, by = "NAME", all.x=FALSE)
str(merge_df)

#Preparación la informacion para correr un modelo ----
data_train2 <- merge_df

str(data_train2)
data_train2 <- data_train2 %>% mutate_if(is.character, as.factor)
summary(data_train2)
data_train2 <- na.omit(data_train2)
colnames(data_train2)
summary(data_train2$Cronoestatigrafica)

# sugerencia
data_train2$Cronoestatigrafica <- as.factor(data_train2$Codi_Geo_Reclas,
                                            levels=c("Cámbrico","CámbricoOrdovícico"))

#### Caso 6: Analisis de data GNNS_CORS ----

mgal <- read.table(file = "Modulos/Modulo0/ParteIB/Geodatabase/Datatable/MGAL_mo_Data_GNSS_CORS.txt", header=TRUE)
str(mgal)
mgal$YYYYMMDD <- ymd(mgal$YYYYMMDD)

a <- plot_ly(mgal, type = 'scatter', mode = 'markers')%>%
  add_trace(x = ~YYYYMMDD, y = ~dN)%>%
  layout(showlegend = F)
a <- a %>%
  layout(showlegend = F, title='Series de Tiempo Desplazamiento - Norte',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=1, label="1m", step="month", stepmode="backward"),
                          list(count=6, label="6m", step="month", stepmode="backward"),
                          list(count=1, label="YTD", step="year", stepmode="todate"),
                          list(count=1, label="1y", step="year", stepmode="backward"),
                          list(step="all")
                        ))))
b <- plot_ly(mgal, type = 'scatter', mode = 'markers')%>%
  add_trace(x = ~YYYYMMDD, y = ~dE)%>%
  layout(showlegend = F)
b <- b %>%
  layout(showlegend = F, title='Series de Tiempo Desplazamiento - Este',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=1, label="1m", step="month", stepmode="backward"),
                          list(count=6, label="6m", step="month", stepmode="backward"),
                          list(count=1, label="YTD", step="year", stepmode="todate"),
                          list(count=1, label="1y", step="year", stepmode="backward"),
                          list(step="all")
                        ))))

c <- plot_ly(mgal, type = 'scatter', mode = 'markers')%>%
  add_trace(x = ~YYYYMMDD, y = ~dU)%>%
  layout(showlegend = F)
c <- c %>%
  layout(showlegend = F, title='Series de Tiempo Desplazamiento - Vertical',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=1, label="1m", step="month", stepmode="backward"),
                          list(count=6, label="6m", step="month", stepmode="backward"),
                          list(count=1, label="YTD", step="year", stepmode="todate"),
                          list(count=1, label="1y", step="year", stepmode="backward"),
                          list(step="all")
                        ))))

subplot(a, b, c, nrows=1)


#### Caso Opcional (Unir informacion de sensores) ----

SRM <- read_excel(path = "Modulos/Modulo0/ParteIB/Geodatabase/Datatable/SRM.xlsx", col_names = TRUE)
str(SRM)
SRM$Tiempo <- mdy_hms(SRM$Tiempo)
colnames(SRM)


SRU <- read_excel(path = "Modulos/Modulo0/ParteIB/Geodatabase/Datatable/SRU.xlsx", col_names = TRUE)
str(SRU)
SRU$Tiempo <- mdy_hms(SRU$Tiempo)
colnames(SRU)

STT <- read_excel(path = "Modulos/Modulo0/ParteIB/Geodatabase/Datatable/STT.xlsx", col_names = TRUE)
str(STT)
STT$Tiempo <- mdy_hms(STT$Tiempo )
colnames(SRM)

Water <- rbind(SRM, SRU, STT)
str(Water)
Water$Tiempo <- ymd_hms(Water$Tiempo)

Water2 <- Water %>% filter(CE_datalogger_uS_cm<mean(Water$CE_datalogger_uS_cm))
library(hrbrthemes)
m <- Water2 %>%
  ggplot(aes(x=Tiempo, y = CE_datalogger_uS_cm))+
  geom_area(fill = "#69b3a2", alpha = 0.5)+
  geom_line(aes(y= 100*Q_m3_s), colour = "red")+
  geom_line(color = "#69ba32")+
  theme_ipsum()
m
ggplotly(m)

n <- Water2 %>% ggplot(aes(x=Tiempo, y = CE_datalogger_uS_cm))+
  geom_point(aes(color=Nombre), size = 0.5)
n
ggplotly(n)

o <- Water2 %>% ggplot(aes(x = Tiempo, y =Nivel_comp_m))+
  geom_point(aes(color = Nombre), size = 0.5)+
  geom_hline(yintercept = mean(Water$Ca_plus_Mg_mg.l))
o
ggplotly(o)

data01 <- Water2[ ,c("Este","Norte")]
data01 <- data01[ ,order(c(names(data01)))]
sputm <- SpatialPoints(data01, proj4string = CRS("+proj=utm +zone=18 +south +datum=WGS84")) ; str(sputm)
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo <- as.data.frame(spgeo)
colnames(spgeo) <- c("long","lat")
prueba <- cbind(Water2, spgeo)
str(prueba)
prueba$Nombre <- as.factor(prueba$Nombre); prueba$Codigo <- as.factor(prueba$Codigo); prueba$Tiempo <- ymd_hms(prueba$Tiempo)
str(prueba)

leaflet(prueba) %>%
  addTiles(group = "Street View") %>%
  addCircleMarkers()
