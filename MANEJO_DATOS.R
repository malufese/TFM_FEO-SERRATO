# Instalar y cargar los paquetes necesarios
install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)
library(openxlsx) # Para guardar el archivo Excel si es necesario

# Cargar el archivo Excel
file_path <- "5.CINCUENTA_TOTAL.xlsx"


# Convertir las columnas de VIF a numeric
data <- data %>%
  mutate(across(starts_with("VIF"), as.numeric))

# Identificar las columnas fijas
fixed_columns <- c("Desbalanceo", "Porcentaje_Unos", "Tamaño_Muestra", "Multicolinealidad")

# Identificar las columnas que deben ser transformadas
variable_columns <- setdiff(names(data), c(fixed_columns, "VIF_X1", "VIF_X2", "VIF_X3"))

# Transformar el DataFrame de wide a long
long_df <- data %>%
  pivot_longer(cols = all_of(variable_columns), 
               names_to = "Coeficiente", 
               values_to = "Estimado") %>%
  mutate(Error_Estandar = NA,
         Desviación_Típica = NA,
         Verdadero = NA,
         Diferencia = NA,
         Método = case_when(
           grepl("^logit", Coeficiente) ~ "Logit",
           grepl("^lasso", Coeficiente) ~ "Lasso",
           grepl("^ridge", Coeficiente) ~ "Ridge",
           grepl("^enet", Coeficiente) ~ "Elastic Net",
           TRUE ~ "Otros"))

# Crear un vector vacío para almacenar los valores de VIF asignados
long_df$VIF <- NA

# Asignar los valores de VIF a los coeficientes logit en orden
vif_index <- 1

for (i in 1:nrow(long_df)) {
  if (long_df$Coeficiente[i] == "logit_coef_X1") {
    long_df$VIF[i] <- data$VIF_X1[vif_index]
  } else if (long_df$Coeficiente[i] == "logit_coef_X2") {
    long_df$VIF[i] <- data$VIF_X2[vif_index]
  } else if (long_df$Coeficiente[i] == "logit_coef_X3") {
    long_df$VIF[i] <- data$VIF_X3[vif_index]
    vif_index <- vif_index + 1
  }
}

# Asignar los valores de Multicolinealidad en bloques de 100 filas
multicolinealidad_values <- rep(c("none", "moderate", "high"), each = 1600, length.out = nrow(long_df))
long_df$Multicolinealidad <- multicolinealidad_values

# Reorganizar columnas según el formato deseado
long_df <- long_df %>%
  select(Desbalanceo, Tamaño_Muestra, Coeficiente, Estimado, Error_Estandar, Desviación_Típica, 
         Verdadero, Diferencia, VIF, Método, Multicolinealidad, Porcentaje_Unos)



# Guardar el DataFrame transformado en un nuevo archivo Excel
write.xlsx(long_df, "long_cincuenta.xlsx")











# Cargar los archivos Excel
files <- c("long_cincuenta.xlsx",
           "6.SESENTA_TOTAL.xlsx", 
           "7.SETENTA_TOTAL.xlsx", 
           "8.OCHENTA_TOTAL.xlsx", 
           "9. NOVENTA_TOTAL.xlsx", 
           "9_5.NOVENTAYCONCO_TOTAL.xlsx")

# Leer los datos en dataframes
dfs <- lapply(files, read_excel)

# Encontrar las columnas comunes en todas las bases de datos
common_columns <- Reduce(intersect, lapply(dfs, colnames))

# Mantener solo las columnas comunes en cada dataframe
dfs_common <- lapply(dfs, function(df) df[, common_columns])

# Convertir todas las columnas a caracteres para evitar conflictos
dfs_common <- lapply(dfs_common, function(df) {
  df[] <- lapply(df, as.character)
  return(df)
})

# Juntar todas las bases de datos en una sola
combined_df <- bind_rows(dfs_common)



# Crear el patrón 0.1, 0.5, 1, -0.5
pattern <- rep(c(0.1, 0.5, 1, -0.5), times = 6000)

# Asignar el patrón a las primeras 24,000 filas de la columna 'Verdadero'
combined_df$Verdadero[1:24000] <- pattern

# Convertir 'Verdadero' y 'Coeficiente' a numérico
combined_df$Verdadero <- as.numeric(combined_df$Verdadero)
combined_df$Estimado <- as.numeric(combined_df$Estimado)

combined_df$Diferencia <- NA
combined_df$Diferencia <- combined_df$Verdadero - combined_df$Estimado
combined_df <- combined_df[, - c(5, 6)]

# Sustituir .Intercept. por (Intercept) en la columna 'Coeficiente'
combined_df$Coeficiente <- gsub("\\.Intercept\\.", "(Intercept)", combined_df$Coeficiente)


# Sustituir "Logit" por "Regresión Logística" en la columna 'Método'
combined_df$Método <- gsub("Logit", "Regresión Logística", combined_df$Método)

# Guardar el dataframe combinado en un nuevo archivo
View(combined_df)
write.csv(combined_df, "final_combined_dataframe.csv", row.names = T)
write.xlsx(combined_df, "TOTAL.xlsx")




# Crear el dataframe resumen
summary_df <- combined_df %>%
  group_by(Método, Coeficiente,Desbalanceo, Tamaño_Muestra, Multicolinealidad) %>%
  summarise(
    media_estimacion = mean(Estimado, na.rm = TRUE),
    desviacion_estimada = sd(Estimado, na.rm = TRUE),
    intervalo_confianza_inf = mean(Estimado, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(Estimado, na.rm = TRUE) / sqrt(n()),
    intervalo_confianza_sup = mean(Estimado, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(Estimado, na.rm = TRUE) / sqrt(n()),
    media_diferencia = mean(Diferencia, na.rm = TRUE),
    desviacion_diferencia = sd(Diferencia, na.rm = TRUE),
    Dintervalo_confianza_inf = mean(Diferencia, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(Diferencia, na.rm = TRUE) / sqrt(n()),
    Dintervalo_confianza_sup = mean(Diferencia, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(Diferencia, na.rm = TRUE) / sqrt(n())
  )
View(summary_df)


write.xlsx(summary_df, "summary_TOTAL.xlsx")




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # G R Á F I C O S# # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Filtrar solo los datos de 'Regresión Logística' y tamaño de muestra de 250
logit_df <- combined_df %>% 
  filter(Método == "Regresión Logística" & Tamaño_Muestra == 10000 & Multicolinealidad == 'high')

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Filtrar solo los datos de 'Regresión Logística' y tamaño de muestra de 250
logit_df <- combined_df %>% 
  filter(Método == "Regresión Logística" & Tamaño_Muestra == 250 & Multicolinealidad == 'none')

# Convertir la columna 'Coeficiente' a factor
logit_df$Coeficiente <- factor(logit_df$Coeficiente, levels = unique(logit_df$Coeficiente))

# Crear un gráfico por coeficiente con límites específicos
p_intercept <- ggplot(logit_df %>% filter(Coeficiente == "logit_coef_(Intercept)"), 
                      aes(x = Estimado, y = factor(Desbalanceo), fill = Coeficiente)) +
  geom_violin(alpha = 0.9, color = "#7D82B8") +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "red") +
  scale_fill_manual(values = "#ABB1FA") +
  labs(title = expression(hat(beta)[0]), x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(-5, 5))

p_x1 <- ggplot(logit_df %>% filter(Coeficiente == "logit_coef_X1"), 
               aes(x = Estimado, y = factor(Desbalanceo), fill = Coeficiente)) +
  geom_violin(alpha = 0.9, color = "#DBC774") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  scale_fill_manual(values = "#FFE787") +
  labs(title = expression(hat(beta)[1]), x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(-10, 10))

p_x2 <- ggplot(logit_df %>% filter(Coeficiente == "logit_coef_X2"), 
               aes(x = Estimado, y = factor(Desbalanceo), fill = Coeficiente)) +
  geom_violin(alpha = 0.9, color = "#D19B88") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_fill_manual(values = "#FFBEA6") +
  labs(title = expression(hat(beta)[2]), x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(-5, 5))

p_x3 <- ggplot(logit_df %>% filter(Coeficiente == "logit_coef_X3"), 
               aes(x = Estimado, y = factor(Desbalanceo), fill = Coeficiente)) +
  geom_violin(alpha = 0.9, color = "#5DB37E") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red") +
  scale_fill_manual(values = "#75E09E") +
  labs(title = expression(hat(beta)[3]), x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(-2, 2))

# Crear un título para el gráfico combinado
main_title <- textGrob("Estimación de parámetros según desbalanceo de clase para Regresión Logística Binaria con n= 10000 y multicolinealidad alta", 
                       gp = gpar(fontsize = 14, fontface = "bold"))

# Crear un título para el eje x
x_title <- textGrob("Valores estimados", gp = gpar(fontsize = 12))

# Crear un subtítulo para el eje y
y_title <- textGrob("Desbalanceo (%)", gp = gpar(fontsize = 12), rot = 90)

# Combinar los gráficos en uno solo con el título principal
combined_plot <- grid.arrange(arrangeGrob(p_intercept, p_x1, p_x2, p_x3, ncol = 2),
                              bottom = x_title,
                              left = y_title,
                              top = main_title)








#################################################################################################################################################################################################################################################


library(dplyr)
library(ggplot2)

# Filtrar los datos para los coeficientes de interés y tamaño de muestra de 250
logit_df <- summary_df %>% 
  filter(Método == "Regresión Logística" & 
           Tamaño_Muestra == 250 & 
           Coeficiente %in% c("logit_coef_(Intercept)", "logit_coef_X1", "logit_coef_X2", "logit_coef_X3"))

# Asegúrate de que la columna Multicolinealidad tiene los niveles correctos
logit_df$Multicolinealidad <- factor(logit_df$Multicolinealidad, levels = c("none", "moderate", "high"))

# Crear etiquetas para los coeficientes
logit_df$Coeficiente <- factor(logit_df$Coeficiente, 
                               levels = c("logit_coef_(Intercept)", "logit_coef_X1", "logit_coef_X2", "logit_coef_X3"),
                               labels = c(expression(hat(beta)[0]), expression(hat(beta)[1]), expression(hat(beta)[2]), expression(hat(beta)[3])))

# Crear el gráfico de líneas con intervalos de confianza
ggplot(logit_df, aes(x = factor(Desbalanceo), y = media_diferencia, group = Multicolinealidad, color = Multicolinealidad)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = Dintervalo_confianza_inf, ymax = Dintervalo_confianza_sup, fill = Multicolinealidad), alpha = 0.15, color = NA) +
  scale_color_manual(values = c("none" = "#2CD18B", "moderate" = "#615ADA", "high" = "#EEBF4A")) +
  scale_fill_manual(values = c("none" = "#2CD18B", "moderate" = "#615ADA", "high" = "#EEBF4A")) +
  labs(title = "Variación media de los Coeficientes de la Regresión Logística Binaria",
       subtitle = "Regresión Logística Binaria con n = 250",
       x = "Desbalanceo (%)",
       y = "Diferencia media",
       color = "Multicolinealidad",
       fill = "Multicolinealidad") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_cartesian(ylim = c(-3.5, 6.5)) +
  facet_wrap(~ Coeficiente, labeller = label_parsed)



# Filtrar los datos para los coeficientes de interés y tamaño de muestra de 500


logit_df <- summary_df %>% 
  filter(Método == "Regresión Logística" & 
           Tamaño_Muestra == 500 & 
           Coeficiente %in% c("logit_coef_(Intercept)", "logit_coef_X1", "logit_coef_X2", "logit_coef_X3"))

# Asegúrate de que la columna Multicolinealidad tiene los niveles correctos
logit_df$Multicolinealidad <- factor(logit_df$Multicolinealidad, levels = c("none", "moderate", "high"))

# Crear etiquetas para los coeficientes
logit_df$Coeficiente <- factor(logit_df$Coeficiente, 
                               levels = c("logit_coef_(Intercept)", "logit_coef_X1", "logit_coef_X2", "logit_coef_X3"),
                               labels = c(expression(hat(beta)[0]), expression(hat(beta)[1]), expression(hat(beta)[2]), expression(hat(beta)[3])))

# Crear el gráfico de líneas con intervalos de confianza
ggplot(logit_df, aes(x = factor(Desbalanceo), y = media_diferencia, group = Multicolinealidad, color = Multicolinealidad)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = Dintervalo_confianza_inf, ymax = Dintervalo_confianza_sup, fill = Multicolinealidad), alpha = 0.15, color = NA) +
  scale_color_manual(values = c("none" = "#2CD18B", "moderate" = "#615ADA", "high" = "#EEBF4A")) +
  scale_fill_manual(values = c("none" = "#2CD18B", "moderate" = "#615ADA", "high" = "#EEBF4A")) +
  labs(title = "Variación media de los Coeficientes de la Regresión Logística Binaria",
       subtitle = "Regresión Logística Binaria con n = 500",
       x = "Desbalanceo (%)",
       y = "Diferencia media",
       color = "Multicolinealidad",
       fill = "Multicolinealidad") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_cartesian(ylim = c(-2.5, 2.5)) +
  facet_wrap(~ Coeficiente, labeller = label_parsed)





# Filtrar los datos para los coeficientes de interés y tamaño de muestra de 1000
logit_df <- summary_df %>% 
  filter(Método == "Regresión Logística" & 
           Tamaño_Muestra == 1000 & 
           Coeficiente %in% c("logit_coef_(Intercept)", "logit_coef_X1", "logit_coef_X2", "logit_coef_X3"))

# Asegúrate de que la columna Multicolinealidad tiene los niveles correctos
logit_df$Multicolinealidad <- factor(logit_df$Multicolinealidad, levels = c("none", "moderate", "high"))

# Crear etiquetas para los coeficientes
logit_df$Coeficiente <- factor(logit_df$Coeficiente, 
                               levels = c("logit_coef_(Intercept)", "logit_coef_X1", "logit_coef_X2", "logit_coef_X3"),
                               labels = c(expression(hat(beta)[0]), expression(hat(beta)[1]), expression(hat(beta)[2]), expression(hat(beta)[3])))

# Crear el gráfico de líneas con intervalos de confianza
ggplot(logit_df, aes(x = factor(Desbalanceo), y = media_diferencia, group = Multicolinealidad, color = Multicolinealidad)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = Dintervalo_confianza_inf, ymax = Dintervalo_confianza_sup, fill = Multicolinealidad), alpha = 0.15, color = NA) +
  scale_color_manual(values = c("none" = "#2CD18B", "moderate" = "#615ADA", "high" = "#EEBF4A")) +
  scale_fill_manual(values = c("none" = "#2CD18B", "moderate" = "#615ADA", "high" = "#EEBF4A")) +
  labs(title = "Variación media de los Coeficientes de la Regresión Logística Binaria",
       subtitle = "Regresión Logística Binaria con n = 1000",
       x = "Desbalanceo (%)",
       y = "Diferencia media",
       color = "Multicolinealidad",
       fill = "Multicolinealidad") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_cartesian(ylim = c(-2.5, 2.5)) +
  facet_wrap(~ Coeficiente, labeller = label_parsed)

# Filtrar los datos para los coeficientes de interés y tamaño de muestra de 5000
logit_df <- summary_df %>% 
  filter(Método == "Regresión Logística" & 
           Tamaño_Muestra == 5000 & 
           Coeficiente %in% c("logit_coef_(Intercept)", "logit_coef_X1", "logit_coef_X2", "logit_coef_X3"))

# Asegúrate de que la columna Multicolinealidad tiene los niveles correctos
logit_df$Multicolinealidad <- factor(logit_df$Multicolinealidad, levels = c("none", "moderate", "high"))

# Crear etiquetas para los coeficientes
logit_df$Coeficiente <- factor(logit_df$Coeficiente, 
                               levels = c("logit_coef_(Intercept)", "logit_coef_X1", "logit_coef_X2", "logit_coef_X3"),
                               labels = c(expression(hat(beta)[0]), expression(hat(beta)[1]), expression(hat(beta)[2]), expression(hat(beta)[3])))

# Crear el gráfico de líneas con intervalos de confianza
ggplot(logit_df, aes(x = factor(Desbalanceo), y = media_diferencia, group = Multicolinealidad, color = Multicolinealidad)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = Dintervalo_confianza_inf, ymax = Dintervalo_confianza_sup, fill = Multicolinealidad), alpha = 0.15, color = NA) +
  scale_color_manual(values = c("none" = "#2CD18B", "moderate" = "#615ADA", "high" = "#EEBF4A")) +
  scale_fill_manual(values = c("none" = "#2CD18B", "moderate" = "#615ADA", "high" = "#EEBF4A")) +
  labs(title = "Variación media de los Coeficientes de la Regresión Logística Binaria",
       subtitle = "Regresión Logística Binaria con n = 5000",
       x = "Desbalanceo (%)",
       y = "Diferencia media",
       color = "Multicolinealidad",
       fill = "Multicolinealidad") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_cartesian(ylim = c(- 2,  2)) +
  facet_wrap(~ Coeficiente, labeller = label_parsed)


# Filtrar los datos para los coeficientes de interés y tamaño de muestra de 10000
logit_df <- summary_df %>% 
  filter(Método == "Regresión Logística" & 
           Tamaño_Muestra == 10000 & 
           Coeficiente %in% c("logit_coef_(Intercept)", "logit_coef_X1", "logit_coef_X2", "logit_coef_X3"))

# Asegúrate de que la columna Multicolinealidad tiene los niveles correctos
logit_df$Multicolinealidad <- factor(logit_df$Multicolinealidad, levels = c("none", "moderate", "high"))

# Crear etiquetas para los coeficientes
logit_df$Coeficiente <- factor(logit_df$Coeficiente, 
                               levels = c("logit_coef_(Intercept)", "logit_coef_X1", "logit_coef_X2", "logit_coef_X3"),
                               labels = c(expression(hat(beta)[0]), expression(hat(beta)[1]), expression(hat(beta)[2]), expression(hat(beta)[3])))

# Crear el gráfico de líneas con intervalos de confianza
ggplot(logit_df, aes(x = factor(Desbalanceo), y = media_diferencia, group = Multicolinealidad, color = Multicolinealidad)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = Dintervalo_confianza_inf, ymax = Dintervalo_confianza_sup, fill = Multicolinealidad), alpha = 0.15, color = NA) +
  scale_color_manual(values = c("none" = "#2CD18B", "moderate" = "#615ADA", "high" = "#EEBF4A")) +
  scale_fill_manual(values = c("none" = "#2CD18B", "moderate" = "#615ADA", "high" = "#EEBF4A")) +
  labs(title = "Variación media de los Coeficientes de la Regresión Logística Binaria",
       subtitle = "Regresión Logística Binaria con n = 10000",
       x = "Desbalanceo (%)",
       y = "Diferencia media",
       color = "Multicolinealidad",
       fill = "Multicolinealidad") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_cartesian(ylim = c(- 2,  2)) +
  facet_wrap(~ Coeficiente, labeller = label_parsed)

