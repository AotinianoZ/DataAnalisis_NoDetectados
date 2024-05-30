#### Tarea1: (Replicar el siguiente proceso cambiando las variables y condiciones - explorar la data) ----
(nms <- names(read_excel(path="data_compilada/PG.xlsx")))
patterns <- c("031103_H_EMBALSE","050503_V_DPST_F", "F_run_up", "050603_C_AFEC_F",
              "050604_EROS_PUEN",
              "050605_EMBLS_CAUC","050606_EROS_TC")

# variables estaban como logicas corregir para poner numericas

(ct <- ifelse(grepl(paste(patterns, collapse="|"), nms), "numeric", "guess"))

base_datos <- read_xlsx(path="data_compilada/PG.xlsx", col_names = TRUE, col_types = ct)

base_datos <- base_datos %>% mutate_if(is.character, as.factor)

### Sumario 

summary(base_datos)
# (ct <- ifelse(grepl("^F_", nms), "text", "guess")) #cambiar por patron inicio.
#otra alternativa pero hay que cambiar despues mucho.
# base_datos <- read.csv(file = "PG.csv", header = TRUE)
str(base_datos, list.len = ncol(base_datos))
ncol(base_datos)

vector_inicios <- as.character(c(00,01,03,14:28))
bd_an <- base_datos %>% select(starts_with(vector_inicios))
colnames(bd_an)

bd_an1 <- bd_an %>% select(starts_with(c("01","25")))
str(bd_an1)

base_datos %>% select(contains("0"))
ncol(base_datos %>% select(contains("0")))

View(base_datos %>%
       group_by(`010100_T_PELIGRO`,`260100_GRADO_PE`) %>%
       summarise(conteo = n()) %>%
       arrange(desc(conteo)))

base_datos %>%
  group_by(`010100_T_PELIGRO`, `010200_ST_PELIGRO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

#### Analizando Deslizamientos 

bd_deslizamiento <- base_datos %>% select(starts_with('00'), starts_with('01'),
                                          starts_with('03'),starts_with('17'))
bd_deslizamiento <- bd_deslizamiento %>% filter(`010100_T_PELIGRO` =="Deslizamiento")
bd_deslizamiento <- bd_deslizamiento %>% mutate_if(is.character, as.factor)
bd_deslizamiento$ZONA <- as.factor(bd_deslizamiento$`000500_ZONA`)
str(bd_deslizamiento)
summary(bd_deslizamiento)

bd_deslizamiento %>%
  filter(!is.na(`031001_AGRTM`)) %>%
  ggplot() +
  geom_boxplot(mapping=aes(x=`031101_D_RECORR`, y = `031001_AGRTM`))+
  xlab(label = "Distancia Recorrida del Deposito de Flujo (m)")+
  ylab(label="Agritamientos")

bd_deslizamiento %>%
  group_by(`010200_ST_PELIGRO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

bd_deslizamiento %>%
  group_by(`030100_ESTILO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

bd_deslizamiento %>%
  group_by(`030200_F_ESCRP`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))


#### Analizando Flujos - Corregir errores!! (vamos)

bd_flujos <- base_datos %>% select(1:21, starts_with('21'))
bd_flujos <- bd_flujos %>% filter(`010100_T_PELIGRO` =="Flujo")
bd_flujos <- bd_flujos %>% mutate_if(is.character, as.factor)

summary(bd_flujos)

bd_flujos %>%
  filter(`001400_FECHA` >= as.Date("2020-01-01"))

bd_flujos %>%
  filter(!is.na(`210300_PR_GRAVAS`)) %>%
  ggplot() +
  geom_point(mapping=aes(x=`210200_PR_BOLONES`, y = `210300_PR_GRAVAS`))+
  xlab(label = "Bloques y Gravas (%)")+
  ylab(label="Material del Flujo")

bd_flujos %>%
  group_by(`010200_ST_PELIGRO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

bd_flujos %>%
  group_by(`020104_FAC_SITIO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

bd_flujos %>%
  group_by(`020103_FAC_SITIO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

########Tarea2: ----

demo_data <- read.csv(file="data_compilada/demo_data.csv", header = TRUE,
                        sep=",")
demo_data <- read.csv(file=file.choose(), header = TRUE,
                      sep=",")
str(demo_data)
head(demo_data)
tail(demo_data)
colnames(demo_data)

summary(demo_data)
# convirtiendo a factores:
demo_data <- demo_data %>% mutate_if(is.character,
                                     as.factor)
str(demo_data)
summary(demo_data)

head(demo_data)
rownames(demo_data)
View(demo_data)

paste("leonard","sebastian", sep="*.*")
rownames(demo_data)
rownames(demo_data) <- paste("leonard", 1:nrow(demo_data), sep="_")
rownames(demo_data)

#tidyverse:
# seleccion y filtro:
colnames(demo_data)
str(demo_data)
summary(demo_data)

colnames(demo_data)
head(demo_data)
demo_data[4,4]
demo_data[ ,4]
demo_data[4, ]0
subset <- demo_data[ ,c(1,2,3)]
head(subset)
subset$pH
subset[ ,-1]

demo_data2 <- demo_data %>% filter(pH>mean(pH), Temporada=="Avenida") %>%
  select(pH, Temporada, Subcuenca, Codigo) %>%
  group_by(Subcuenca) %>%
  summarise(
    conteo = n()
  ) %>% arrange(desc(conteo))

save(demo_data2, file = "demo.RData")

load(file = "demo.RData")
write.csv(x = demo_data2, file = "demo_data2.csv") 