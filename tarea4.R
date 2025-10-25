install.packages("arules", type = "binary")
library(arules)
library(readxl)
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

data <- read_excel("C:\\Users\\willi\\OneDrive\\Escritorio\\Maestría\\IMD\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")

data_fp <- data[, c("TOTAL_HIJOS",    
                    "VIC_SEXO",       
                    "VIC_EDAD",        
                    "VIC_EST_CIV",     
                    "VIC_TRABAJA",
                    "VIC_REL_AGR",
                    "HEC_DEPTO")]

#data_fp <- subset(data_fp, HEC_DEPTO==1)
data_fp[is.na(data_fp)] <- -1
data_fp <- subset(data_fp, TOTAL_HIJOS>=0 & TOTAL_HIJOS < 99)

cluster <- kmeans(data_fp, centers = 3)
data_fp$cluster <- as.factor(cluster$cluster)

ggplot(data_fp, aes(x = TOTAL_HIJOS, y = VIC_EDAD, color = cluster)) +
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x = TOTAL_HIJOS, y = VIC_EDAD), color="black", size=4, shape=17)+
  labs(title = "Total de Hijos vs Edad Victima") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

