#MODELO PREDICTIVO  RENTA DE BICICELTAS

#PASO1: IMPORTA EL ARCHIVO BICIS

library(readr)
housing <- read_csv("housing.csv")
View(housing)

# Paso 2: Entender la base de datos
str(housing)
summary(housing)
plot(housing$median_income, housing$median_house_value, 
     main = "Influencia del ingreso medio sobre el valor de las viviendas", 
     xlab = "Ingreso Medio", ylab = "Valor Mediano de la Vivienda")




# Paso 3: Generar la regresión lineal
regresion <- lm(median_house_value ~ longitude + latitude + housing_median_age + total_rooms + total_bedrooms + population + households + median_income, 
                data = housing)
summary(regresion)






# Paso 4: Evaluar y ajustar la regresión lineal
regresion_ajustada <- lm(median_house_value ~ longitude + latitude + housing_median_age + total_rooms + total_bedrooms + population + households + median_income, 
                         data = housing)
summary(regresion_ajustada)


# Paso 5: Construir un modelo de predicción
datos_nuevos <- data.frame(longitude = -122, latitude = 38, housing_median_age = 40, total_rooms = 2000, 
                           total_bedrooms = 400, population = 1000, households = 350, median_income = 6.0)
View(datos_nuevos)
predict(regresion, datos_nuevos)

library(ggplot2)

# Crear un dataframe con los datos de ingreso medio y valor mediano de la vivienda
df <- data.frame(median_income = housing$median_income,
                 median_house_value = housing$median_house_value)

# Graficar la relación entre ingreso medio y valor mediano de la vivienda con una regresión lineal
ggplot(df, aes(x = median_income, y = median_house_value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión lineal entre Ingreso Medio y Valor Mediano de la Vivienda",
       x = "Ingreso Medio",
       y = "Valor Mediano de la Vivienda")


# Crear un dataframe con los datos de ingreso medio y valor mediano de la vivienda
df <- data.frame(median_income = housing$median_income,
                 median_house_value = housing$median_house_value)

# Graficar la relación entre ingreso medio y valor mediano de la vivienda
ggplot(df, aes(x = median_income, y = median_house_value)) +
  geom_point(color = "blue", alpha = 0.5) +  # Puntos azules con transparencia
  labs(title = "Relación entre Ingreso Medio y Valor Mediano de la Vivienda",
       x = "Ingreso Medio",
       y = "Valor Mediano de la Vivienda")

#Conclusiones

