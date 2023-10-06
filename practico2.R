library(readr)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(TTR)
library(scales)

#data <- read_csv("C:\\Users\\alder\\Downloads\\base_de_datos\\emails.csv")
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



# Valor constante 'm'
m <- 30000  # Puedes ajustar el valor de 'm' según tus necesidades

# Crear un dataframe vacío para almacenar las probabilidades
probabilidades_df <- data.frame(Palabra = character(0), Probabilidad_1 = numeric(0), Probabilidad_0 = numeric(0))

# Iterar a través de las columnas de Bag of Words, excluyendo la primera y la última columna
for (i in 1:(ncol(entrenamiento) - 1)) {  # Excluyendo la primera y la última columna
  
  # Extraer la palabra actual
  palabra_actual <- names(entrenamiento)[i]
  
  # Contar(Wi ∩ Clase) + 1 para 1
  Conteo_Wi_Clase_1 <- sum(entrenamiento[[i]][entrenamiento$Prediction == 1]) + 1
  
  # Contar(Wi ∩ Clase) + 1 para 0
  Conteo_Wi_Clase_0 <- sum(entrenamiento[[i]][entrenamiento$Prediction == 0]) + 1
  
  # Contar(Clase) + m para 1
  Conteo_Clase_1 <- sum(entrenamiento$Prediction == 1) + m
  
  # Contar(Clase) + m para 0
  Conteo_Clase_0 <- sum(entrenamiento$Prediction == 0) + m
  
  # Calcular la probabilidad para 1
  Probabilidad_Spam <- Conteo_Wi_Clase_1 / Conteo_Clase_1
  
  # Calcular la probabilidad para 0
  Probabilidad_Ham <- Conteo_Wi_Clase_0 / Conteo_Clase_0
  
  if (Probabilidad_Spam > Probabilidad_Ham){
    binario_vector = 1
  } 
  else{
    binario_vector = 0
  }
  
  # Agregar los resultados al dataframe
  nueva_fila <- data.frame(Palabra = palabra_actual, Probabilidad_1 = Probabilidad_Spam, Probabilidad_0 = Probabilidad_Ham, binario = binario_vector)
  probabilidades_df <- rbind(probabilidades_df , nueva_fila)
}
head(probabilidades_df)




# Crear un dataframe vacío para almacenar los resultados
resultados_df <- data.frame(
  Palabra = character(0),
  Wi_Spam = numeric(0),
  Wi_Ham = numeric(0)
)

# Iterar a través de las filas del dataframe probabilidades_df
for (j in 1:nrow(probabilidades_df)) {
  
  # Acceder a los valores necesarios de probabilidades_df
  binario <- probabilidades_df$binario[j]
  Probabilidad_1 <- probabilidades_df$Probabilidad_1[j]
  Probabilidad_0 <- probabilidades_df$Probabilidad_0[j]
  
  # Calcular los valores Wi_Spam y Wi_Ham
  wi_spam <- binario * log(Probabilidad_1) + (1 - binario) * log(1 - Probabilidad_1)
  wi_ham <- binario * log(Probabilidad_0) + (1 - binario) * log(1 - Probabilidad_0)
  
  # Agregar los resultados al dataframe resultados_df
  nueva_fila <- data.frame(Palabra = probabilidades_df$Palabra[j], Wi_Spam = wi_spam, Wi_Ham = wi_ham)
  resultados_df <- rbind(resultados_df, nueva_fila)
}

for(i in nrow(1:resultados_df)){
  
  resultado <- log1pexp(resultados_df$Wi_Spam[i] + log_P_Spam, resultados_df$wi_ham[i] + log_P_Ham)
  
}






