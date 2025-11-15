install.packages("arules")
install.packages("genero")
install.packages("rpart")
install.packages("rpart.plot")
library(readxl)
library(arules)
library(genero)
library(rpart)
library(rpart.plot)


data = read_excel("C:\\Users\\willi\\OneDrive\\Escritorio\\Maestría\\IMD\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")
data_research <- data[, c("VIC_REL_AGR",
                          "VIC_SEXO",
                          "VIC_EDAD",
                          "AGR_SEXO",
                          "AGR_EDAD",
                          "QUIEN_REPORTA",
                          "VIC_EST_CIV"
)]
data_research <- na.omit(data_research)
arbol_vif <- rpart(
  VIC_REL_AGR ~ VIC_SEXO + VIC_EDAD + AGR_SEXO + AGR_EDAD + QUIEN_REPORTA + VIC_EST_CIV,
  data = data_research,
  method = "class"
)

rpart.plot(arbol_vif,
           type = 2,
           extra = 0,
           under = TRUE,
           fallen.leaves = TRUE,
           box.palette = "BuGn",
           main = "Árbol de decisión: Relación Víctima–Agresor",
           cex = 0.5)

nueva_persona <- data.frame(
  VIC_SEXO = c(2),        
  VIC_EDAD = c(28),
  AGR_SEXO = c(1),        
  AGR_EDAD = c(32),        
  QUIEN_REPORTA = c(3),   
  VIC_EST_CIV = c(2)     
)

predict(arbol_vif, nueva_persona, type = "prob")
