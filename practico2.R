library(readr)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(TTR)
library(scales)

data <- read_csv("C:\\Users\\alder\\Downloads\\base_de_datos\\emails.csv")
# Establece una semilla aleatoria para que la división sea reproducible
set.seed(666)

# Crea un vector de índices aleatorios para dividir los datos
indices <- sample(1:nrow(data), nrow(data))

# Calcula el número de filas para el conjunto de entrenamiento (80%)
n_entrenamiento <- round(0.1 * nrow(data))

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

log_P_Spam <- total_spam/nrow(entrenamiento)
log_P_Ham <- total_ham/nrow(entrenamiento)






##BASE ENTRENAMIENTO

# Valor constante 'm'
m <- 3000  # Puedes ajustar el valor de 'm' según tus necesidades

# Crear un dataframe vacío para almacenar las probabilidades
probabilidades_df <- data.frame(Palabra = character(0), Probabilidad_1 = numeric(0), Probabilidad_0 = numeric(0))

# Iterar a través de las columnas de Bag of Words, excluyendo la primera y la última columna
for (i in 1:(ncol(entrenamiento) - 1)) {  # Excluyendo la primera y la última columna
  
  # Extraer la palabra actual
  palabra_actual <- names(entrenamiento)[i]
  
  # Contar(Wi ∩ Clase) + 1 para 1
  Conteo_Wi_Clase_1 <- (sum(entrenamiento[[i]][entrenamiento$Prediction == 1]) + 1)
                  
  
  # Contar(Wi ∩ Clase) + 1 para 0
  Conteo_Wi_Clase_0 <- (sum(entrenamiento[[i]][entrenamiento$Prediction == 0]) + 1)
  
  # Contar(Clase) + m para 1
  Conteo_Clase_1 <- sum(subset(entrenamiento, Prediction == 1)) + m
  
  # Contar(Clase) + m para 0
  Conteo_Clase_0 <- sum(subset(entrenamiento, Prediction == 0)) + m
  
  # Calcular la probabilidad para 1
  Probabilidad_Spam <- Conteo_Wi_Clase_1 / Conteo_Clase_1
  
  # Calcular la probabilidad para 0
  Probabilidad_Ham <- Conteo_Wi_Clase_0 / Conteo_Clase_0
  
  #Logaritmo probabilidades para 1
  
  Log_spam <- log(Probabilidad_Ham)
  
  #Logaritmo probabilidades para 0
  
  Log_ham <- log(Probabilidad_Spam)
  

  # Agregar los resultados al dataframe
  nueva_fila <- data.frame(Palabra = palabra_actual, Probabilidad_1 = Probabilidad_Spam, Probabilidad_0 = Probabilidad_Ham, binario = binario_vector, Log_spam = Log_spam, Log_ham = Log_ham)
  probabilidades_df <- rbind(probabilidades_df , nueva_fila)
}
head(probabilidades_df)






##BASE DE PRUEBA

vectores_binarios <- matrix(0, nrow = nrow(entrenamiento), ncol = ncol(entrenamiento) - 1)  # Inicializar matriz de vectores binarios

# Iterar a través de las filas (correos electrónicos) de la base de datos
for (i in 1:nrow(entrenamiento)) {
  correo <- entrenamiento[i, -ncol(entrenamiento)]  # Excluir la última columna y obtener el correo actual
  vectores_binarios[i,] <- as.integer(correo > 0)  # Crear un vector binario: 1 si la frecuencia es mayor que 0, 0 en otro caso
}





#log(P(ωi| Spam)) Calculo

# Crear un dataframe vacío para almacenar los resultados
resultados_df <- data.frame(
  correo = character(0),
  Wi_Spam = numeric(0),
  Wi_Ham = numeric(0)
)

# Iterar a través de las filas del dataframe probabilidades_df
for (i in 1:nrow(vectores_binarios)) {
  
  print(i)
  for (j in 1:nrow(probabilidades_df)){
    print(j)
    
    # Acceder a los valores necesarios de probabilidades_df
    binario <- vectores_binarios[i][j]
    Probabilidad_1 <- probabilidades_df$Probabilidad_1[j]
    Probabilidad_0 <- probabilidades_df$Probabilidad_0[j]
    
    # Calcular los valores Wi_Spam y Wi_Ham
    ωi_spam <- sum(vectores_binarios[i][j] * log(Probabilidad_1) + (1 - vectores_binarios[i][j]) * log(1 - Probabilidad_1))
    ωi_ham <- sum(vectores_binarios[i][j] * log(Probabilidad_0) + (1 - vectores_binarios[i][j]) * log(1 - Probabilidad_0))
    
    # Agregar los resultados al dataframe resultados_df
    nueva_fila <- data.frame(correo = data$`Email No.`, Wi_Spam = wi_spam, Wi_Ham = wi_ham)
    resultados_df <- rbind(resultados_df, nueva_fila)
    
  }
  
  
}




# Crear un dataframe vacío para almacenar los resultados
resultados_df <- data.frame(
  correo = numeric(0),
  Wi_Spam = numeric(0),
  Wi_Ham = numeric(0)
)

# Iterar a través de las filas del dataframe entrenamiento
for (i in 1:nrow(entrenamiento)) {
  print(i)
  
  # Acceder a la fila actual de vectores_binarios
  binario <- vectores_binarios[i, ]
  
  # Calcular los valores Wi_Spam y Wi_Ham
  ωi_spam <- sum(binario * log(probabilidades_df$Probabilidad_1) + (1 - binario) * log(1 - probabilidades_df$Probabilidad_1))
  ωi_ham <- sum(binario * log(probabilidades_df$Probabilidad_0) + (1 - binario) * log(1 - probabilidades_df$Probabilidad_0))
  
  # Agregar los resultados a resultados_df
  nueva_fila <- data.frame(correo = i, Wi_Spam = ωi_spam, Wi_Ham = ωi_ham)
  resultados_df <- rbind(resultados_df, nueva_fila)
}


logaddexp <- function(a, b) {
  max_val <- max(a, b)
  max_val + log(1 + exp(-abs(a - b)))
}

# Create an empty data frame to store the results
predict_result <- data.frame()



for (i in 1:nrow(entrenamiento)) {
  P_ωi <- logaddexp(resultados_df$Wi_Spam[i] + log_P_Spam, resultados_df$Wi_Ham[i] + log_P_Ham)
  
  # Create a new row with the result and add it to predict_result
  new_row <- data.frame(Resultado = resultado)
  predict_result <- rbind(predict_result, new_row)
}



# Create an empty data frame to store the results
predict_result_2 <- data.frame()

for (i in 1:(nrow(entrenamiento))) {
  
  predict_spam <- resultados_df$Wi_Spam[i] + log_P_Spam - predict_result$Resultado[i]
  predict_ham <- resultados_df$Wi_Ham[i] + log_P_Ham - predict_result$Resultado[i]
  
  # Create a new row with pico and pico2 and add it to predict_result_2
  new_row <- data.frame(predict_spam = predict_spam, predict_ham = predict_ham)
  predict_result_2 <- rbind(predict_result_2, new_row)
}

# Crear un nuevo dataframe con la columna assSpam_ham
predict_result_2$assSpam_ham <- ifelse(predict_result_2$predict_spam > predict_result_2$predict_ham, 1, 0)

# Verificar el contenido del dataframe
head(predict_result_2)


