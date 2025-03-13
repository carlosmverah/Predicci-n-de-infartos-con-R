# Cargar librerías necesarias
library(readr)

# Cargar datos
healthcare_dataset_stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")
datos <- healthcare_dataset_stroke_data

# Convertir variables categóricas a factor con niveles bien definidos
datos$gender <- factor(datos$gender)
datos$smoking_status <- factor(datos$smoking_status)
datos$stroke <- factor(datos$stroke, levels = c(0, 1))  # Definir niveles explícitamente

# Ver estructura de los datos
str(datos)
summary(datos)
table(datos$stroke)  # Verificar distribución de la variable objetivo

# Entrenar el modelo con variables categóricas
modelo <- glm(stroke ~ age + hypertension + gender + smoking_status, data = datos, family = binomial)

# Mostrar resultados
summary(modelo)

# Crear un nuevo paciente asegurando que los factores tengan los mismos niveles que en el dataset
nuevo_paciente <- data.frame(
  age = 55, 
  hypertension = 1, 
  gender = factor("Male", levels = levels(datos$gender)), 
  smoking_status = factor("smokes", levels = levels(datos$smoking_status))
)

# Hacer la predicción
predicción <- predict(modelo, nuevo_paciente, type = "response")

# Mostrar el resultado. Si el resultado es mayor a 0.5, indica un alto riesgo de infarto.
riesgo_infarto <- ifelse(predicción > 0.5, "Alto riesgo de infarto", "Bajo riesgo de infarto")
print(riesgo_infarto)

