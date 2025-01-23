# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(openxlsx)

# Leer los datos desde el archivo (modifica el nombre del archivo según corresponda)
data <- read_excel("TOTAL.xlsx")

# Asegúrate de que los nombres de las columnas sean correctos
colnames(data) <- c("Desbalanceo", "Tamaño_Muestra", "Coeficiente", "Estimado", "Verdadero", "Diferencia", "VIF", "Método", "Multicolinealidad", "Porcentaje_Unos")

# Función para calcular VMC y ECMMC
calculate_indices <- function(df) {
  df %>%
    group_by(Coeficiente, Método, Multicolinealidad, Desbalanceo, Tamaño_Muestra) %>%
    summarize(
      BMC = mean(Diferencia),
      VMC = sum((Estimado - mean(Estimado))^2) / (n() - 1),
      ECMMC = VMC + BMC^2,
      SE = sqrt(VMC),
      RMSE = sqrt(ECMMC)
    )
}

# Calcular los índices
indices_results <- calculate_indices(data)

# Mostrar los resultados
print(indices_results)

# Guardar los resultados en un archivo Excel
write.xlsx(indices_results, file = "Indices_Resultados.xlsx")
