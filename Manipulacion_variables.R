head(iris)
str(iris)
colnames(iris) <- c("Longitud del sepalo", "Ancho del sepalo", "Longitud del petalo", "Ancho del petalo", "Especie")
colnames(iris)
iris$`Ancho del sepalo`
# Varias columnas
head(iris[c("Ancho del sepalo","Longitud del sepalo")])
# Añadir una columna al dataframe
iris$id <- c(1:150)
head(iris)
# Eliminar una columna al dataframe
iris$id <- NULL
head(iris)
#Añadir filas al dataframe
nuevafila <- c(3.5, 2.5, 2.2, 1.5, "setosa")
iris1 <- rbind(iris, nuevafila)
# La nueva fila se coloca al final
tail(iris1)
# Eliminar filas del dataframe
tail(iris1[-151,])
summary(iris)
# Cambiar el tipo de variable
iris$`Longitud del sepalo` <- as.numeric(iris$`Longitud del sepalo`)
iris$`Ancho del sepalo` <- as.numeric(iris$`Ancho del sepalo`)
iris$`Longitud del petalo` <- as.numeric(iris$`Longitud del petalo`)
iris$`Ancho del petalo` <- as.numeric(iris$`Ancho del petalo`)
# install.packages("readr")

# library(readr)
url <- "https://raw.githubusercontent.com/jmcastagnetto/covid-19-peru-data/main/datos/covid-19-peru-test-results.csv"

df <- read_csv(url)

# Verificar si hay datos faltantes en el dataframe
missing_values <- any(is.na(df))
print(paste("¿Hay datos faltantes en el dataframe? ", missing_values))

# Contar la cantidad de datos faltantes por columna
missing_count <- colSums(is.na(df))
print("Cantidad de datos faltantes por columna:")

print(missing_count)
head(df)

# Eliminar filas con datos faltantes
df_sin_missing <- na.omit(df)
print("Dataframe sin datos faltantes:")

print(df_sin_missing)

# Imputar valores faltantes (por ejemplo, usando la media)
df_imputado <- df
df_imputado$personas[is.na(df_imputado$personas)] <- mean(df_imputado$personas, na.rm = TRUE)
print("Dataframe con valores imputados:")

print(df_imputado)
