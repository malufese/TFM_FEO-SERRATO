# Cargar las bibliotecas necesarias
library(dplyr)
library(writexl)
library(stringr)
library(readxl)
data <- read_excel("TOTAL.xlsx")
colnames(data) <- c("Desbalanceo", "Tamaño_Muestra", "Coeficiente", "Estimado", "Verdadero", "Diferencia", "VIF", "Método", "Multicolinealidad", "Porcentaje_Unos")


calculate_indices <- function(df) {
  df %>%
    group_by(Coeficiente, Método, Multicolinealidad, Desbalanceo, Tamaño_Muestra) %>%
    summarize(
      BMC = mean(Diferencia, na.rm = TRUE),
      SE = sd(Diferencia, na.rm = TRUE)) %>%
    ungroup()
}

# Calcular los índices
indices_results <- calculate_indices(data)

# Mostrar los resultados
print(indices_results)
View(indices_results)

# Guardar los resultados en un archivo Excel
write.xlsx(indices_results, file = "_Final_Indices_Resultados.xlsx")

# Cargar las bibliotecas necesarias
library(dplyr)
library(writexl)
library(stringr)
library(readxl)
indices_results <- read_excel("_Final_Indices_Resultados.xlsx") #Se ha eliminado los coeficientes del intersecto en excel, este fichero está deepurado. 

# Calcular medias para Elastic Net
elastic_net <- indices_results %>%
  filter(str_detect(Coeficiente, "enet_coef")) %>%
  group_by(Método = "Elastic Net", Desbalanceo) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),    # Media de BMC
    media_SE = mean(SE, na.rm = TRUE),      # Media de SE
    BMC_B = (media_BMC)^2,                            # Cuadrado de la media de BMC
    SE_B = (media_SE)^2,                              # Cuadrado de la media de SE
    SUMA = BMC_B + SE_B,                              # Suma de los cuadrados
    RMSE = sqrt(SUMA),                                # Raíz cuadrada de la suma
    media_RMSE = round(RMSE, 3)                       # Media de RMSE redondeada
  )

View(elastic_net)



# Calcular medias para Regresión Logística


regresion_logistica <- indices_results %>%
  filter(str_detect(Coeficiente, "logit_coef") & Método != "Regresión Logística (SMOTE)") %>%
  group_by(Método = "Regresión Logística", Desbalanceo) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),    # Media de BMC
    media_SE = mean(SE, na.rm = TRUE),      # Media de SE
    BMC_B = (media_BMC)^2,                            # Cuadrado de la media de BMC
    SE_B = (media_SE)^2,                              # Cuadrado de la media de SE
    SUMA = BMC_B + SE_B,                              # Suma de los cuadrados
    RMSE = sqrt(SUMA),                                # Raíz cuadrada de la suma
    media_RMSE = round(RMSE, 3)                       # Media de RMSE redondeada
  )



View(regresion_logistica)

# Cálculo ridge

ridge <- indices_results %>%
  filter(str_detect(Coeficiente, "ridge_coef")) %>%
  group_by(Método = "Ridge", Desbalanceo) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),    # Media de BMC
    media_SE = mean(SE, na.rm = TRUE),      # Media de SE
    BMC_B = (media_BMC)^2,                            # Cuadrado de la media de BMC
    SE_B = (media_SE)^2,                              # Cuadrado de la media de SE
    SUMA = BMC_B + SE_B,                              # Suma de los cuadrados
    RMSE = sqrt(SUMA),                                # Raíz cuadrada de la suma
    media_RMSE = round(RMSE, 3)                       # Media de RMSE redondeada
  )


# Visualizar el resultado
View(ridge)




# Calcular medias para LASSO
lasso <- indices_results %>%
  filter(str_detect(Coeficiente, "lasso_coef")) %>%
  group_by(Método = "LASSO", Desbalanceo) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),    # Media de BMC
    media_SE = mean(SE, na.rm = TRUE),      # Media de SE
    BMC_B = (media_BMC)^2,                            # Cuadrado de la media de BMC
    SE_B = (media_SE)^2,                              # Cuadrado de la media de SE
    SUMA = BMC_B + SE_B,                              # Suma de los cuadrados
    RMSE = sqrt(SUMA),                                # Raíz cuadrada de la suma
    media_RMSE = round(RMSE, 3)                       # Media de RMSE redondeada
  )

View(lasso)

# Calcular medias para SMOTE (desbalanceo desde 0.6)
smote <- indices_results %>%
  filter(Método == "Regresión Logística (SMOTE)" & as.numeric(Desbalanceo) >= 0.6) %>%
  group_by(Método = "SMOTE", Desbalanceo) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),    # Media de BMC
    media_SE = mean(SE, na.rm = TRUE),      # Media de SE
    BMC_B = (media_BMC)^2,                            # Cuadrado de la media de BMC
    SE_B = (media_SE)^2,                              # Cuadrado de la media de SE
    SUMA = BMC_B + SE_B,                              # Suma de los cuadrados
    RMSE = sqrt(SUMA),                                # Raíz cuadrada de la suma
    media_RMSE = round(RMSE, 3)                       # Media de RMSE redondeada
  )


View(smote)


# Combinar todas las tablas en una
resultados_combinados <- bind_rows(elastic_net, regresion_logistica, ridge, lasso, smote)
View(resultados_combinados)

# Exportar los resultados combinados a un archivo Excel
write_xlsx(resultados_combinados, "DEFINIVO_IMBALANCE.xlsx")

# Visualizar los resultados
print(resultados_combinados)




library(dplyr)
library(stringr)
library(writexl)

indices_results <- read_excel("_Final_Indices_Resultados.xlsx") # Cargar datos depurados

# Calcular medias para Elastic Net
elastic_net <- indices_results %>%
  filter(str_detect(Coeficiente, "enet_coef")) %>%
  group_by(Método = "Elastic Net", Multicolinealidad) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),    # Media de BMC
    media_SE = mean(SE, na.rm = TRUE),      # Media de SE
    BMC_B = (media_BMC)^2,                  # Cuadrado de la media de BMC
    SE_B = (media_SE)^2,                    # Cuadrado de la media de SE
    SUMA = BMC_B + SE_B,                    # Suma de los cuadrados
    RMSE = sqrt(SUMA),                      # Raíz cuadrada de la suma
    media_RMSE = round(RMSE, 3)             # Media de RMSE redondeada
  )

View(elastic_net)

# Calcular medias para Regresión Logística
regresion_logistica <- indices_results %>%
  filter(str_detect(Coeficiente, "logit_coef") & Método != "Regresión Logística (SMOTE)") %>%
  group_by(Método = "Regresión Logística", Multicolinealidad) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),
    media_SE = mean(SE, na.rm = TRUE),
    BMC_B = (media_BMC)^2,
    SE_B = (media_SE)^2,
    SUMA = BMC_B + SE_B,
    RMSE = sqrt(SUMA),
    media_RMSE = round(RMSE, 3)
  )

View(regresion_logistica)

# Calcular medias para Ridge
ridge <- indices_results %>%
  filter(str_detect(Coeficiente, "ridge_coef")) %>%
  group_by(Método = "Ridge", Multicolinealidad) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),
    media_SE = mean(SE, na.rm = TRUE),
    BMC_B = (media_BMC)^2,
    SE_B = (media_SE)^2,
    SUMA = BMC_B + SE_B,
    RMSE = sqrt(SUMA),
    media_RMSE = round(RMSE, 3)
  )

View(ridge)

# Calcular medias para LASSO
lasso <- indices_results %>%
  filter(str_detect(Coeficiente, "lasso_coef")) %>%
  group_by(Método = "LASSO", Multicolinealidad) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),
    media_SE = mean(SE, na.rm = TRUE),
    BMC_B = (media_BMC)^2,
    SE_B = (media_SE)^2,
    SUMA = BMC_B + SE_B,
    RMSE = sqrt(SUMA),
    media_RMSE = round(RMSE, 3)
  )

View(lasso)

# Calcular medias para SMOTE (multicolinealidad)
smote <- indices_results %>%
  filter(Método == "Regresión Logística (SMOTE)") %>%
  group_by(Método = "SMOTE", Multicolinealidad) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),
    media_SE = mean(SE, na.rm = TRUE),
    BMC_B = (media_BMC)^2,
    SE_B = (media_SE)^2,
    SUMA = BMC_B + SE_B,
    RMSE = sqrt(SUMA),
    media_RMSE = round(RMSE, 3)
  )

View(smote)

# Combinar todas las tablas en una
resultados_combinados <- bind_rows(elastic_net, regresion_logistica, ridge, lasso, smote)

# Exportar los resultados combinados a un archivo Excel
write_xlsx(resultados_combinados, "DEFINITIVO_MULTICOLINEALIDAD.xlsx")

# Visualizar los resultados
print(resultados_combinados)



# Calcular medias para Elastic Net por tamaño de muestra
elastic_net <- indices_results %>%
  filter(str_detect(Coeficiente, "enet_coef")) %>%
  group_by(Método = "Elastic Net", Tamaño_Muestra) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),    # Media de BMC
    media_SE = mean(SE, na.rm = TRUE),      # Media de SE
    BMC_B = (media_BMC)^2,                 # Cuadrado de la media de BMC
    SE_B = (media_SE)^2,                   # Cuadrado de la media de SE
    SUMA = BMC_B + SE_B,                   # Suma de los cuadrados
    RMSE = sqrt(SUMA),                     # Raíz cuadrada de la suma
    media_RMSE = round(RMSE, 3)            # Media de RMSE redondeada
  )

# Calcular medias para Regresión Logística por tamaño de muestra
regresion_logistica <- indices_results %>%
  filter(str_detect(Coeficiente, "logit_coef") & Método != "Regresión Logística (SMOTE)") %>%
  group_by(Método = "Regresión Logística", Tamaño_Muestra) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),
    media_SE = mean(SE, na.rm = TRUE),
    BMC_B = (media_BMC)^2,
    SE_B = (media_SE)^2,
    SUMA = BMC_B + SE_B,
    RMSE = sqrt(SUMA),
    media_RMSE = round(RMSE, 3)
  )

# Calcular medias para Ridge por tamaño de muestra
ridge <- indices_results %>%
  filter(str_detect(Coeficiente, "ridge_coef")) %>%
  group_by(Método = "Ridge", Tamaño_Muestra) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),
    media_SE = mean(SE, na.rm = TRUE),
    BMC_B = (media_BMC)^2,
    SE_B = (media_SE)^2,
    SUMA = BMC_B + SE_B,
    RMSE = sqrt(SUMA),
    media_RMSE = round(RMSE, 3)
  )

# Calcular medias para LASSO por tamaño de muestra
lasso <- indices_results %>%
  filter(str_detect(Coeficiente, "lasso_coef")) %>%
  group_by(Método = "LASSO", Tamaño_Muestra) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),
    media_SE = mean(SE, na.rm = TRUE),
    BMC_B = (media_BMC)^2,
    SE_B = (media_SE)^2,
    SUMA = BMC_B + SE_B,
    RMSE = sqrt(SUMA),
    media_RMSE = round(RMSE, 3)
  )

# Calcular medias para SMOTE por tamaño de muestra
smote <- indices_results %>%
  filter(Método == "Regresión Logística (SMOTE)") %>%
  group_by(Método = "SMOTE", Tamaño_Muestra) %>%
  summarise(
    media_BMC = mean(BMC, na.rm = TRUE),
    media_SE = mean(SE, na.rm = TRUE),
    BMC_B = (media_BMC)^2,
    SE_B = (media_SE)^2,
    SUMA = BMC_B + SE_B,
    RMSE = sqrt(SUMA),
    media_RMSE = round(RMSE, 3)
  )

# Combinar todas las tablas en una
resultados_por_tamano_muestra <- bind_rows(elastic_net, regresion_logistica, ridge, lasso, smote)

# Exportar los resultados combinados a un archivo Excel
write_xlsx(resultados_por_tamano_muestra, "Resultados_Tamano_Muestra.xlsx")

# Visualizar los resultados
View(resultados_por_tamano_muestra)
