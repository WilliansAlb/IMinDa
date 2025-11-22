install.packages("randomForest")
install.packages("caret")
library(readxl)
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(stringr)

data = read_excel("C:\\Users\\willi\\OneDrive\\Escritorio\\Maestría\\IMD\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")
# 1. Limpieza y selección
df <- data %>%
  select(
    VIC_REL_AGR,
    VIC_SEXO,
    VIC_EDAD,
    AGR_SEXO,
    AGR_EDAD,
    QUIEN_REPORTA,
    VIC_EST_CIV
  ) %>%
  mutate(across(everything(), ~ na_if(., 99))) %>%
  mutate(across(everything(), ~ na_if(., 9)))

df <- na.omit(df)

# Convertir a factores
df <- df %>%
  mutate(
    VIC_SEXO = as.factor(VIC_SEXO),
    AGR_SEXO = as.factor(AGR_SEXO),
    QUIEN_REPORTA = as.factor(QUIEN_REPORTA),
    VIC_EST_CIV = as.factor(VIC_EST_CIV),
    VIC_REL_AGR = as.factor(VIC_REL_AGR) 
  )
# 2. Entrenamiento
set.seed(123)
index <- sample(1:nrow(df), 0.8 * nrow(df))
train <- df[index, ]
test  <- df[-index, ]
# 3. Random Forest
rf_rel <- randomForest(
  VIC_REL_AGR ~ VIC_SEXO +
  VIC_EDAD +
  AGR_SEXO +
  AGR_EDAD +
  QUIEN_REPORTA +
  VIC_EST_CIV,
  data = train,
  ntree = 300,
  importance = TRUE
)
print(rf_rel)
pred_rel <- predict(rf_rel, test)
confusionMatrix(pred_rel, test$VIC_REL_AGR)
# 4. Importancia de variables
varImpPlot(rf_rel, main="Importancia de variables para predecir VIC_REL_AGR")
# 5. Error vs árboles
plot(rf_rel, main="Error vs Número de Árboles")
