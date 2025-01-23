install.packages("ARTool")
install.packages("readxl")
install.packages("dplyr")
install.packages("openxlsx")
library(ARTool)
library(readxl)
library(dplyr)
library(openxlsx)


# Leer los datos desde el archivo (modifica el nombre del archivo según corresponda)
data <- read_excel("Indices_Resultados.xlsx")
head(data)

# Convertir las variables a factores
data$Desbalanceo <- as.factor(data$Desbalanceo)
data$Multicolinealidad <- as.factor(data$Multicolinealidad)
data$Tamaño_Muestra <- as.factor(data$Tamaño_Muestra)


# Definir los métodos
methods <- c("Elastic Net", "Lasso", "Ridge", "Regresión Logística", "Regresión Logística (SMOTE)")

# Inicializar listas para almacenar los resultados
all_art_results <- list()
all_tukey_results <- list()

# Crear un bucle para realizar el ANOVA no paramétrico y pruebas post-hoc para cada método
for (method in methods) {
  # Filtrar los datos para el método actual
  if (method == "Regresión Logística (SMOTE)") {
    subset_data <- data %>% filter(Método == method & Desbalanceo != 0.5)
  } else {
    subset_data <- data %>% filter(Método == method)
  }
  
  if (nrow(subset_data) > 0) {
    # Realizar el ANOVA no paramétrico usando ARTool
    art_model <- art(RMSE ~ Desbalanceo * Multicolinealidad * Tamaño_Muestra, data = subset_data)
    
    # Obtener el resumen del modelo ART
    art_anova <- anova(art_model)
    print(paste("Resultados del ANOVA no paramétrico para el método:", method))
    print(art_anova)
    
    # Calcular omega cuadrado parcial usando la librería effectsize
    omega_sq <- effectsize::omega_squared(art_model)
    interpret_omega <- effectsize::interpret_omega_squared(omega_sq)
    
    # Guardar los resultados del ANOVA en un DataFrame
    art_anova_df <- as.data.frame(art_anova)
    art_anova_df$omega_sq <- omega_sq$Omega2_partial
    art_anova_df$interpretation <- interpret_omega$Interpretation
    art_anova_df$Método <- method
    all_art_results[[method]] <- art_anova_df
    
    # Realizar la prueba post-hoc de Tukey para todas las variables independientes utilizando art.con
    tukey_results <- list()
    for (var in c("Desbalanceo", "Multicolinealidad", "Tamaño_Muestra")) {
      tukey_result <- art.con(art_model, var, adjust="tukey")
      tukey_df <- as.data.frame(tukey_result)
      tukey_df$Variable <- var
      tukey_results[[var]] <- tukey_df
    }
    
    # Realizar la prueba post-hoc de Tukey para las interacciones de segundo orden
    interactions_2nd_order <- c("Desbalanceo:Multicolinealidad", "Desbalanceo:Tamaño_Muestra", "Multicolinealidad:Tamaño_Muestra")
    for (interaction in interactions_2nd_order) {
      interaction_result <- art.con(art_model, interaction, adjust="tukey")
      interaction_df <- as.data.frame(interaction_result)
      interaction_df$Variable <- interaction
      tukey_results[[interaction]] <- interaction_df
    }
    
    # Realizar la prueba post-hoc de Tukey para la interacción de tercer orden
    interaction_3rd_order <- "Desbalanceo:Multicolinealidad:Tamaño_Muestra"
    interaction_result <- art.con(art_model, interaction_3rd_order, adjust="tukey")
    interaction_df <- as.data.frame(interaction_result)
    interaction_df$Variable <- interaction_3rd_order
    tukey_results[[interaction_3rd_order]] <- interaction_df
    
    # Combinar los resultados de Tukey en un solo DataFrame para este método
    tukey_combined <- do.call(rbind, tukey_results)
    tukey_combined$Método <- method
    all_tukey_results[[method]] <- tukey_combined
  } else {
    print(paste("No hay datos suficientes para el método:", method))
  }
}

# Combinar todos los resultados de ANOVA no paramétrico en un único DataFrame
final_art_results <- do.call(rbind, all_art_results)

# Combinar todos los resultados de Tukey en un único DataFrame
final_tukey_results <- do.call(rbind, all_tukey_results)

# Guardar los resultados en un archivo Excel
write.xlsx(list("ART_ANOVA_Results_RMSE" = final_art_results, "Tukey_Post_Hoc" = final_tukey_results), file = "ARTool_ANOVA_VMC_resultados.xlsx")





library(ggplot2)

# Supongo que tienes tus datos en data_logistica
# data_logistica <- your_data_frame_here

# Generar el gráfico mejorado
p1 <- ggplot(data = data, aes(x = Desbalanceo, y = RMSE, colour = Multicolinealidad, group = Multicolinealidad)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 21, fill = "white") +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  labs(y = 'mean (RMSE)', x = 'Desbalanceo', colour = 'Multicolinealidad') +
  scale_colour_manual(values = c("none" = "#1b9e77", "moderate" = "#d95f02", "high" = "#7570b3")) +
  ggtitle("Interacción entre Desbalanceo y Multicolinealidad en RMSE") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "top", # Puedes ajustar la posición de la leyenda según tu preferencia
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) # Ajustar la posición del título de la leyenda

print(p1)



library(ggplot2)
library(forcats)

# Asegurarse de que Tamaño_Muestra esté ordenado de menos a más
data$Tamaño_Muestra <- factor(data$Tamaño_Muestra, levels = c("250", "500", "1000", "5000", "10000"))

# Generar el gráfico con facetas por Tamaño_Muestra y ajustar los límites del eje y
p2 <- ggplot(data = data, aes(x = Desbalanceo, y = BMC, colour = Multicolinealidad, group = Multicolinealidad)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 21, fill = "white") +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  labs(y = 'BMC', x = 'Desequilibrio de clase', colour = 'Multicolinealidad') +
  scale_colour_manual(values = c("none" = "#1b9e77", "moderate" = "#d95f02", "high" = "#7570b3")) +
  ggtitle("Interacción entre Desbalanceo, Multicolinealidad y Tamaño de Muestra BMC") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~ Tamaño_Muestra, scales = "free_y") + # Ajustar límites del eje y
  ylim(-0.1, 0.1) # Ajustar los límites del eje y según sea necesario

print(p2)


# Asegurarse de que Tamaño_Muestra esté ordenado de menos a más
data$Tamaño_Muestra <- factor(data$Tamaño_Muestra, levels = c("250", "500", "1000", "5000", "10000"))

# Generar el gráfico con facetas por Tamaño_Muestra y ajustar los límites del eje y
p2 <- ggplot(data = data, aes(x = Desbalanceo, y = SE, colour = Multicolinealidad, group = Multicolinealidad)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 21, fill = "white") +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  labs(y = 'SE', x = 'Desequilibrio de clases', colour = 'Multicolinealidad') +
  scale_colour_manual(values = c("none" = "#1b9e77", "moderate" = "#d95f02", "high" = "#7570b3")) +
  ggtitle("Interacción entre Desbalanceo, Multicolinealidad y Tamaño de Muestra en SE") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~ Tamaño_Muestra, scales = "free_y") + # Ajustar límites del eje y
  ylim(0, 0.4) # Ajustar los límites del eje y según sea necesario

print(p2)



p2 <- ggplot(data = data, aes(x = Desbalanceo, y = RMSE, colour = Multicolinealidad, group = Multicolinealidad)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 21, fill = "white") +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  labs(y = 'RMSE', x = 'Desequilibrio de clases', colour = 'Multicolinealidad') +
  scale_colour_manual(values = c("none" = "#1b9e77", "moderate" = "#d95f02", "high" = "#7570b3")) +
  ggtitle("Interacción entre Desbalanceo, Multicolinealidad y Tamaño de Muestra en RMSE") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~ Tamaño_Muestra, scales = "free_y") + # Ajustar límites del eje y
  ylim(0, 0.4) # Ajustar los límites del eje y según sea necesario

print(p2)
