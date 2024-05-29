# upload data:
library(readxl)
database <- read_excel("D:/WaterPaper_AOtiniano/Modelo/data/database.xlsx")
head(database)
colnames(database)

# Analisis:

str(database)
library(tidyverse)
database <- database %>% select(Codigo, Temporada, Subcuenca, Microcuenca,
                                pH, CE_uS_cm, 
                                Al_dis, Al_tot, Cu_dis, Cu_tot, Fe_dis, Fe_tot,
                                Pb_dis, Pb_tot, Zn_dis, Zn_tot)
database
View(database)
database  <- database %>% mutate_at(c('Codigo', 'Temporada', "Subcuenca", "Microcuenca"), as.factor)


# Create new columns for data analysis:

for (element in colnames(database[ ,7:16])){
  col <- element
  col_mod <- paste0(element, "_mod")
  col_mod_cen <- paste0(element, "_mod_cen")
  
  database[[col_mod]] <- as.numeric(str_remove(database[[col]], pattern = "<"))
  database[[col_mod_cen]] <- ifelse(str_detect(database[[col]], pattern = "<"), TRUE, FALSE)
}

# check data:
colnames(database)
head(database %>% select(contains("Al_")))
summary(database %>% select(c("Al_dis","Al_tot","Al_dis_mod","Al_dis_mod_cen")))

id <- which(database$Al_dis_mod > database$Al_tot_mod)
database[id, ]

