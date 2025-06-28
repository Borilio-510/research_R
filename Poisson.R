# Paso 1: Cargar los datos desde el archivo CSV
datos_poisson <- read.csv("datos_poisson.csv")

# Paso 2: Explorar los datos (opcional pero recomendado)
head(datos_poisson) # Muestra las primeras filas
summary(datos_poisson) # Resumen estadístico para cada columna

# Paso 3: Análisis para cada conjunto de datos (columna)
# Calcular la media (lambda estimado)
lambda_est_llamadas <- mean(datos_poisson$Llamadas_Lambda_5)
print(paste("Lambda estimado para Llamadas:", lambda_est_llamadas))

# Crear un histograma para visualizar la distribución
hist(datos_poisson$Llamadas_Lambda_5,
     breaks = seq(min(datos_poisson$Llamadas_Lambda_5)-0.5,
                  max(datos_poisson$Llamadas_Lambda_5)+0.5, by = 1),
     main = "Histograma de Llamadas por Hora",
     xlab = "Número de Llamadas",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "black")

# Ajustar un modelo Poisson
# Esto es para determinar si los datos se ajustan a una distribución de Poisson
library(fitdistrplus)
fit_llamadas <- fitdist(datos_poisson$Llamadas_Lambda_5, "pois")
summary(fit_llamadas)
plot(fit_llamadas) # Gráficos de diagnóstico para el ajuste


# --- Escenario 2: Errores_Lambda_1.5 ---
lambda_est_errores <- mean(datos_poisson$Errores_Lambda_1.5)
print(paste("Lambda estimado para Errores:", lambda_est_errores))
hist(datos_poisson$Errores_Lambda_1.5,
     breaks = seq(min(datos_poisson$Errores_Lambda_1.5)-0.5,
                  max(datos_poisson$Errores_Lambda_1.5)+0.5, by = 1),
     main = "Histograma de Errores por Expediente",
     xlab = "Número de Errores",
     ylab = "Frecuencia",
     col = "lightgreen",
     border = "black")
fit_errores <- fitdist(datos_poisson$Errores_Lambda_1.5, "pois")
summary(fit_errores)


# --- Escenario 3: Casos_Raros_Lambda_0.8 ---
lambda_est_casos <- mean(datos_poisson$Casos_Raros_Lambda_0.8)
print(paste("Lambda estimado para Casos Raros:", lambda_est_casos))
hist(datos_poisson$Casos_Raros_Lambda_0.8,
     breaks = seq(min(datos_poisson$Casos_Raros_Lambda_0.8)-0.5,
                  max(datos_poisson$Casos_Raros_Lambda_0.8)+0.5, by = 1),
     main = "Histograma de Casos Raros por Mes",
     xlab = "Número de Casos",
     ylab = "Frecuencia",
     col = "lightcoral",
     border = "black")
fit_casos <- fitdist(datos_poisson$Casos_Raros_Lambda_0.8, "pois")
summary(fit_casos)

# --- 1. Calcular estadísticas descriptivas para cada columna ---
# Nombres de las columnas que queremos analizar
columnas_datos <- names(datos_poisson)

# Crear una lista para almacenar los resultados de cada columna
resultados_list <- list()

for (col_name in columnas_datos) {
  datos_columna <- datos_poisson[[col_name]] # Acceder a la columna por su nombre
  
  media_col <- mean(datos_columna)
  varianza_col <- var(datos_columna)
  
  # Almacenar los resultados en la lista
  resultados_list[[col_name]] <- data.frame(
    Variable = col_name,
    Media_Muestral = round(media_col, 3),
    Varianza_Muestral = round(varianza_col, 3)
  )
}

# Combinar todos los data.frames de la lista en uno solo
tabla_descriptivos <- bind_rows(resultados_list)

# --- 2. Ajustar el modelo Poisson para obtener la estimación de lambda (MLE) ---
library(fitdistrplus)

# Lista para almacenar las estimaciones MLE de lambda
estimaciones_mle <- list()

for (col_name in columnas_datos) {
  datos_columna <- datos_poisson[[col_name]]
  
  # Ajustar la distribución Poisson
  fit_modelo <- fitdist(datos_columna, "pois")
  
  # Extraer la estimación de lambda y su error estándar
  lambda_mle <- fit_modelo$estimate["lambda"]
  se_lambda_mle <- fit_modelo$sd["lambda"]
  
  estimaciones_mle[[col_name]] <- data.frame(
    Variable = col_name,
    Lambda_MLE = round(lambda_mle, 3),
    SE_Lambda_MLE = round(se_lambda_mle, 3)
  )
}

# Combinar las estimaciones MLE
tabla_mle <- bind_rows(estimaciones_mle)

# --- 3. Unir ambas tablas de resultados ---
# Asegúrate de que las columnas a unir sean idénticas (en este caso 'Variable')
tabla_final_resultados <- left_join(tabla_descriptivos, tabla_mle, by = "Variable")

# Añadir los lambdas teóricos con los que se generaron los datos para referencia
# Puedes agregar esta información manualmente o si la tuvieras en otro archivo
lambdas_teoricos <- data.frame(
  Variable = c("Llamadas_Lambda_5", "Errores_Lambda_1.5", "Casos_Raros_Lambda_0.8"),
  Lambda_Teorico_Generado = c(5.0, 1.5, 0.8)
)

tabla_final_resultados <- left_join(tabla_final_resultados, lambdas_teoricos, by = "Variable")


# --- 4. Mostrar la tabla formateada ---
# Usando kable para una tabla limpia en la consola o en R Markdown
kable(tabla_final_resultados,
      caption = "Resultados del Análisis de Distribución de Poisson (Datos Simulados)",
      align = c("l", "r", "r", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F)
