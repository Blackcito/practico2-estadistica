library(readr)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(TTR)
library(scales)

data <- read_csv("C:\\Users\\alder\\Downloads\\base_de_datos\\emails.csv")
# Establece una semilla aleatoria para que la división sea reproducible
set.seed(123)

# Crea un vector de índices aleatorios para dividir los datos
indices <- sample(1:nrow(data), nrow(data))

# Calcula el número de filas para el conjunto de entrenamiento (80%)
n_entrenamiento <- round(0.8 * nrow(data))

# Divide los datos en conjuntos de entrenamiento y prueba
entrenamiento <- data[indices[1:n_entrenamiento], ]
prueba <- data[indices[(n_entrenamiento + 1):nrow(data)], ]

# Verifica la proporción de correos spam en ambos conjuntos
proporcion_spam_entrenamiento <- sum(entrenamiento$Prediction == 1) / nrow(entrenamiento)
proporcion_spam_prueba <- sum(prueba$Prediction == 1) / nrow(prueba)

# Imprime las proporciones
cat("Proporción de correos spam en el conjunto de entrenamiento:", proporcion_spam_entrenamiento, "\n")
cat("Proporción de correos spam en el conjunto de prueba:", proporcion_spam_prueba, "\n")

# Remueve la primera y última columna de los conjuntos de entrenamiento y prueba
entrenamiento <- entrenamiento[, -1]
prueba <- prueba[, -1]

# Calcula el número total de correos spam y ham en el conjunto de entrenamiento
total_spam <- sum(entrenamiento$Prediction == 1)
total_ham <- n_entrenamiento - total_spam

# Crea una matriz para almacenar las probabilidades
probabilidades <- matrix(0, nrow = ncol(entrenamiento), ncol = 2)

# Itera a través de cada palabra (columna) en el conjunto de entrenamiento
for (i in 2:ncol(entrenamiento)) {  # Comenzamos desde la columna 2 (la primera columna es la etiqueta)
  # Contar cuántas veces aparece la palabra en correos spam y ham
  word_count_spam <- sum(entrenamiento[entrenamiento$Prediction == 1, i])
  word_count_ham <- sum(entrenamiento[entrenamiento$Prediction == 0, i])
  
  # Calcular las probabilidades con suavizado de Laplace
  prob_spam <- (word_count_spam + 1) / (total_spam + 2)  # Agregamos 1 al numerador y 2 al denominador
  prob_ham <- (word_count_ham + 1) / (total_ham + 2)
  
  # Almacenar las probabilidades en la matriz
  probabilidades[i - 1, 1] <- prob_spam  # Restamos 1 para evitar la primera columna
  probabilidades[i - 1, 2] <- prob_ham
}

# Convierte la matriz de probabilidades en un data frame con nombres de columna
probabilidades_df <- data.frame(Word = colnames(entrenamiento), Prob_Spam = probabilidades[, 1], Prob_Ham = probabilidades[, 2])
# Imprime las primeras filas del data frame de probabilidades
head(probabilidades_df)
