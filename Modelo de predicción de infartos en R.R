# Cargar librerías necesarias
library(readr)

# Cargar datos
healthcare_dataset_stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")
datos <- healthcare_dataset_stroke_data

# Convertir variables categóricas a factor con niveles bien definidos
datos$gender <- factor(datos$gender)
datos$smoking_status <- factor(datos$smoking_status)
datos$stroke <- factor(datos$stroke, levels = c(0, 1))
datos$hypertension <- factor(datos$hypertension, levels = c(0, 1))  # Asegurar niveles binarios

# Ver estructura de los datos
str(datos)
summary(datos)
table(datos$stroke)  # Verificar distribución de la variable objetivo

# Entrenar el modelo con variables categóricas
modelo <- glm(stroke ~ age + hypertension + gender + smoking_status, data = datos, family = binomial)

# Mostrar resultados del modelo
summary(modelo)

# Crear un nuevo paciente. La variable "age" debe ser un número, la variable "hypertension" debe ser 0 si no es hipertenso y 1 si es hipertenso, la variable "gender" debe ser "Male" o "Female", la variable "smoking_status" debe ser "never smoked", "formerly smoked", "smokes" o "Unknown". 
nuevo_paciente <- data.frame(
  age = 55, 
  hypertension = factor(1, levels = levels(datos$hypertension)), 
  gender = factor("Male", levels = levels(datos$gender)), 
  smoking_status = factor("smokes", levels = levels(datos$smoking_status))
)

# Hacer la predicción
predicción <- predict(modelo, nuevo_paciente, type = "response")

# Mostrar el resultado
riesgo_infarto <- ifelse(predicción > 0.5, "Alto riesgo de infarto", "Bajo riesgo de infarto")
print(riesgo_infarto)


