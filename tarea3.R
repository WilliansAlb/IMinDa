library(readxl)
library(arules)
library(dplyr)

data <- read_excel("C:\\Users\\willi\\OneDrive\\Escritorio\\Maestría\\IMD\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")

data_fp <- data[, c("TOTAL_HIJOS",    
                    "VIC_SEXO",       
                    "VIC_EDAD",        
                    "VIC_EST_CIV",     
                    "VIC_TRABAJA",    
                    "VIC_REL_AGR")]   

data_fp$VIC_EST_CIV <- factor(data_fp$VIC_EST_CIV,
                              levels = c(1, 2, 3, 4, 5, 9),
                              labels = c("Soltero", "Casado", "Unido", "Viudo", "Otro", "Ignorado"))

data_fp$VIC_SEXO <- factor(data_fp$VIC_SEXO,
                           levels = c(1, 2),
                           labels = c("Hombre", "Mujer"))

data_fp$VIC_TRABAJA <- factor(data_fp$VIC_TRABAJA,
                              levels = c(1, 2, 9),
                              labels = c("Si", "No", "Ignorado"))

data_fp$VIC_REL_AGR <- factor(data_fp$VIC_REL_AGR,
                              levels = c(1,2,3,4,5,6,7,8,9,10),
                              labels = c("Esposo", "Conviviente", "Exconviviente", "Hijo",
                                         "Hijastro", "Padre/Madre", "Nieto", "Suegro",
                                         "Hermano", "Otro pariente"))

reglas_fp <- fim4r(data_fp,
                   method = "fpgrowth",
                   target = "rules",
                   supp = 0.2,
                   conf = 0.5)

rf <- as(reglas_fp, "data.frame")


rf_filtrado <- rf %>%
  filter(grepl("Esposo|Conviviente|Exconviviente", LHS) |
           grepl("Esposo|Conviviente|Exconviviente", RHS))

