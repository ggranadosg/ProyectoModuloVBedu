#Análisis del conjunto de datos
# Cargar datos
carros <- read.csv("D:/BajadoEnD/20231218Módulo05Estadística&ProgramaciónConR/carros/carros.csv")

# Visualizar las primeras filas de los datos
head(carros)

# Resumen de las características numéricas
summary(carros)

# Histogramas de las variables numéricas
par(mfrow = c(2, 2))
hist(carros$precio, main = "Distribución del precio", xlab = "Precio")
hist(carros$edad_del_modelo, main = "Distribución de la edad del modelo", xlab = "Edad del modelo")
hist(carros$km_por_anio, main = "Distribución de los kilómetros por año", xlab = "Kilómetros por año")

"Este código ajusta 4 modelos diferentes (regresión lineal, regresión polinómica de grado 2, 
árbol de decisión y bosque aleatorio) al conjunto de datos de entrenamiento y evalua su rendimiento 
en el conjunto de prueba utilizando el error cuadrático medio (MSE)"

# Dividir los datos en conjunto de entrenamiento y prueba (80% train, 20% test)
set.seed(123)  # Para reproducibilidad
train_index <- sample(1:nrow(carros), 0.8 * nrow(carros))
train_data <- carros[train_index, ]
test_data <- carros[-train_index, ]

# Modelo de regresión lineal
lm_model <- lm(precio ~ edad_del_modelo + km_por_anio, data = train_data)
lm_pred <- predict(lm_model, newdata = test_data)
lm_mse <- mean((lm_pred - test_data$precio)^2)
cat("Regresión Lineal MSE:", lm_mse, "\n")

# Modelo de regresión polinómica de grado 2
poly_model <- lm(precio ~ poly(edad_del_modelo, 2) + poly(km_por_anio, 2), data = train_data)
poly_pred <- predict(poly_model, newdata = test_data)
poly_mse <- mean((poly_pred - test_data$precio)^2)
cat("Regresión Polinómica de Grado 2 MSE:", poly_mse, "\n")

# Modelo de árbol de decisión
library(rpart) 
tree_model <- rpart(precio ~ edad_del_modelo + km_por_anio, data = train_data)
tree_pred <- predict(tree_model, newdata = test_data)
tree_mse <- mean((tree_pred - test_data$precio)^2)
cat("Árbol de Decisión MSE:", tree_mse, "\n")

# Modelo de bosque aleatorio
library(randomForest)
rf_model <- randomForest(precio ~ edad_del_modelo + km_por_anio, data = train_data)
rf_pred <- predict(rf_model, newdata = test_data)
rf_mse <- mean((rf_pred - test_data$precio)^2)
cat("Bosque Aleatorio MSE:", rf_mse, "\n")

"Basándonos en estos resultados, podemos determinar que la regresión polinómica de grado 2 tiene el MSE más bajo, 
lo que indica que tiene un mejor rendimiento en la predicción del precio en comparación con los otros 
modelos probados. 
Por lo tanto, podemos concluir que el mejor modelo de machine learning para predecir el precio 
utilizando las variables edad_del_modelo y km_por_anio en este conjunto de datos es la regresión polinómica 
de grado 2."

