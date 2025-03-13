# Cargar datos
healthcare_dataset_stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")
datos <- healthcare_dataset_stroke_data

# Convertir variables categóricas a factor
datos$gender <- as.factor(datos$gender)
datos$smoking_status <- as.factor(datos$smoking_status)
datos$stroke <- as.factor(datos$stroke)  # Variable objetivo también como factor

# Ver los primeros datos
head(datos)

# Entrenar el modelo con variables categóricas
modelo <- glm(stroke ~ age + hypertension + gender + smoking_status, data = datos, family = binomial)

# Mostrar resultados
summary(modelo)

# Crear un nuevo paciente con variables categóricas; la edad, hipertensión, género y estatus de fumador pueden variar.
nuevo_paciente <- data.frame(age = 55, hypertension = 1, gender = factor("Male", levels = levels(datos$gender)), smoking_status = factor("smokes", levels = levels(datos$smoking_status)))

# Hacer la predicción
predicción <- predict(modelo, nuevo_paciente, type = "response")

# Mostrar el resultado
print(predicción)

#Si el resultado es mayor a 0.5, indica un alto riesgo de infarto.
