install.packages("ARTool")
install.packages("readxl")
install.packages("dplyr")
install.packages("openxlsx")
library(ARTool)
library(readxl)
library(dplyr)
library(openxlsx)
library(dplyr)
library(tidyr)


# Leer los datos desde el archivo (modifica el nombre del archivo según corresponda)
# Cargar los datos
library(art)
library(tidyverse)
library(effectsize)
library(writexl)

data <- read_excel("_Final_Indices_Resultados.xlsx")
data$Desbalanceo <- as.factor(data$Desbalanceo)
data$Multicolinealidad <- as.factor(data$Multicolinealidad)
data$Tamaño_Muestra <- as.factor(data$Tamaño_Muestra)



colnames(data)



# Calcular RMSE a nivel de fila
data <- data %>%
  mutate(
    BMC_B = BMC^2,           # Cuadrado de BMC
    SE_B = SE^2,             # Cuadrado de SE
    SUMA = BMC_B + SE_B,     # Suma de los cuadrados
    RMSE = sqrt(SUMA)        # Raíz cuadrada de la suma
  )


# Remover filas con valores faltantes en RMSE
data <- data %>% drop_na(RMSE)

# Verificar que RMSE está correctamente calculado
head(data)



# Definir métodos
methods <- c("Elastic Net", "Lasso", "Ridge", "Regresión Logística", "Regresión Logística (SMOTE)")

# Inicializar listas para almacenar resultados
all_art_results <- list()
all_tukey_results <- list()

# ANOVA no paramétrico por método con Tamaño_Muestra
for (method in methods) {
  # Filtrar datos por método
  if (method == "Regresión Logística (SMOTE)") {
    subset_data <- data %>% filter(Método == method & Desbalanceo != 0.5)
  } else {
    subset_data <- data %>% filter(Método == method)
  }
  
  if (nrow(subset_data) > 0) {
    # ANOVA no paramétrico con ARTool
    art_model <- art(RMSE ~ Tamaño_Muestra * Multicolinealidad * Desbalanceo, data = subset_data)
    
    # Resultados del ANOVA
    art_anova <- anova(art_model)
    print(paste("Resultados del ANOVA no paramétrico para el método:", method))
    print(art_anova)
    
    # Cálculo de omega cuadrado parcial
    omega_sq <- effectsize::omega_squared(art_model)
    interpret_omega <- effectsize::interpret_omega_squared(omega_sq)
    
    # Guardar resultados del ANOVA
    art_anova_df <- as.data.frame(art_anova)
    art_anova_df$omega_sq <- omega_sq$Omega2_partial
    art_anova_df$interpretation <- interpret_omega$Interpretation
    art_anova_df$Método <- method
    all_art_results[[method]] <- art_anova_df
    
    # Pruebas post-hoc de Tukey
    tukey_results <- list()
    for (var in c("Tamaño_Muestra", "Multicolinealidad", "Desbalanceo")) {
      tukey_result <- art.con(art_model, var, adjust = "tukey")
      tukey_df <- as.data.frame(tukey_result)
      tukey_df$Variable <- var
      tukey_results[[var]] <- tukey_df
    }
    
    # Interacciones de segundo orden
    interactions_2nd_order <- c("Tamaño_Muestra:Multicolinealidad", "Tamaño_Muestra:Desbalanceo", "Multicolinealidad:Desbalanceo")
    for (interaction in interactions_2nd_order) {
      interaction_result <- art.con(art_model, interaction, adjust = "tukey")
      interaction_df <- as.data.frame(interaction_result)
      interaction_df$Variable <- interaction
      tukey_results[[interaction]] <- interaction_df
    }
    
    # Interacción de tercer orden
    interaction_3rd_order <- "Tamaño_Muestra:Multicolinealidad:Desbalanceo"
    interaction_result <- art.con(art_model, interaction_3rd_order, adjust = "tukey")
    interaction_df <- as.data.frame(interaction_result)
    interaction_df$Variable <- interaction_3rd_order
    tukey_results[[interaction_3rd_order]] <- interaction_df
    
    # Combinar resultados de Tukey
    tukey_combined <- do.call(rbind, tukey_results)
    tukey_combined$Método <- method
    all_tukey_results[[method]] <- tukey_combined
  } else {
    print(paste("No hay datos suficientes para el método:", method))
  }
}

# Combinar resultados de ANOVA y Tukey
final_art_results <- do.call(rbind, all_art_results)
final_tukey_results <- do.call(rbind, all_tukey_results)

# Guardar resultados en un archivo Excel
write_xlsx(list("F_ART_ANOVA_Results_RMSE" = final_art_results, "F_Tukey_Post_Hoc" = final_tukey_results), "F_ARTool_ANOVA_Tamano_Muestra.xlsx")

# Mostrar resultados
View(final_art_results)
View(final_tukey_results)


########ANOVA_SE

# Definir métodos
methods <- c("Elastic Net", "Lasso", "Ridge", "Regresión Logística", "Regresión Logística (SMOTE)")

# Inicializar listas para almacenar resultados
all_art_results <- list()
all_tukey_results <- list()

# ANOVA no paramétrico por método con Tamaño_Muestra
for (method in methods) {
  # Filtrar datos por método
  if (method == "Regresión Logística (SMOTE)") {
    subset_data <- data %>% filter(Método == method & Desbalanceo != 0.5)
  } else {
    subset_data <- data %>% filter(Método == method)
  }
  
  if (nrow(subset_data) > 0) {
    # ANOVA no paramétrico con ARTool usando SE
    art_model <- art(SE ~ Tamaño_Muestra * Multicolinealidad * Desbalanceo, data = subset_data)
    
    # Resultados del ANOVA
    art_anova <- anova(art_model)
    print(paste("Resultados del ANOVA no paramétrico para el método:", method))
    print(art_anova)
    
    # Cálculo de omega cuadrado parcial
    omega_sq <- effectsize::omega_squared(art_model)
    interpret_omega <- effectsize::interpret_omega_squared(omega_sq)
    
    # Guardar resultados del ANOVA
    art_anova_df <- as.data.frame(art_anova)
    art_anova_df$omega_sq <- omega_sq$Omega2_partial
    art_anova_df$interpretation <- interpret_omega$Interpretation
    art_anova_df$Método <- method
    all_art_results[[method]] <- art_anova_df
    
    # Pruebas post-hoc de Tukey
    tukey_results <- list()
    for (var in c("Tamaño_Muestra", "Multicolinealidad", "Desbalanceo")) {
      tukey_result <- art.con(art_model, var, adjust = "tukey")
      tukey_df <- as.data.frame(tukey_result)
      tukey_df$Variable <- var
      tukey_results[[var]] <- tukey_df
    }
    
    # Interacciones de segundo orden
    interactions_2nd_order <- c("Tamaño_Muestra:Multicolinealidad", "Tamaño_Muestra:Desbalanceo", "Multicolinealidad:Desbalanceo")
    for (interaction in interactions_2nd_order) {
      interaction_result <- art.con(art_model, interaction, adjust = "tukey")
      interaction_df <- as.data.frame(interaction_result)
      interaction_df$Variable <- interaction
      tukey_results[[interaction]] <- interaction_df
    }
    
    # Interacción de tercer orden
    interaction_3rd_order <- "Tamaño_Muestra:Multicolinealidad:Desbalanceo"
    interaction_result <- art.con(art_model, interaction_3rd_order, adjust = "tukey")
    interaction_df <- as.data.frame(interaction_result)
    interaction_df$Variable <- interaction_3rd_order
    tukey_results[[interaction_3rd_order]] <- interaction_df
    
    # Combinar resultados de Tukey
    tukey_combined <- do.call(rbind, tukey_results)
    tukey_combined$Método <- method
    all_tukey_results[[method]] <- tukey_combined
  } else {
    print(paste("No hay datos suficientes para el método:", method))
  }
}

# Combinar resultados de ANOVA y Tukey
final_art_results <- do.call(rbind, all_art_results)
final_tukey_results <- do.call(rbind, all_tukey_results)

# Guardar resultados en un archivo Excel
write_xlsx(list("SE_ART_ANOVA_Results" = final_art_results, "SE_Tukey_Post_Hoc" = final_tukey_results), "SE_ARTool_ANOVA_Tamano_Muestra.xlsx")

# Mostrar resultados
View(final_art_results)
View(final_tukey_results)



# Definir métodos
methods <- c("Elastic Net", "Lasso", "Ridge", "Regresión Logística", "Regresión Logística (SMOTE)")

# Inicializar listas para almacenar resultados
all_art_results <- list()
all_tukey_results <- list()

# ANOVA no paramétrico por método con Tamaño_Muestra
for (method in methods) {
  # Filtrar datos por método
  if (method == "Regresión Logística (SMOTE)") {
    subset_data <- data %>% filter(Método == method & Desbalanceo != 0.5)
  } else {
    subset_data <- data %>% filter(Método == method)
  }
  
  if (nrow(subset_data) > 0) {
    # ANOVA no paramétrico con ARTool usando BMC
    art_model <- art(BMC ~ Tamaño_Muestra * Multicolinealidad * Desbalanceo, data = subset_data)
    
    # Resultados del ANOVA
    art_anova <- anova(art_model)
    print(paste("Resultados del ANOVA no paramétrico para el método:", method))
    print(art_anova)
    
    # Cálculo de omega cuadrado parcial
    omega_sq <- effectsize::omega_squared(art_model)
    interpret_omega <- effectsize::interpret_omega_squared(omega_sq)
    
    # Guardar resultados del ANOVA
    art_anova_df <- as.data.frame(art_anova)
    art_anova_df$omega_sq <- omega_sq$Omega2_partial
    art_anova_df$interpretation <- interpret_omega$Interpretation
    art_anova_df$Método <- method
    all_art_results[[method]] <- art_anova_df
    
    # Pruebas post-hoc de Tukey
    tukey_results <- list()
    for (var in c("Tamaño_Muestra", "Multicolinealidad", "Desbalanceo")) {
      tukey_result <- art.con(art_model, var, adjust = "tukey")
      tukey_df <- as.data.frame(tukey_result)
      tukey_df$Variable <- var
      tukey_results[[var]] <- tukey_df
    }
    
    # Interacciones de segundo orden
    interactions_2nd_order <- c("Tamaño_Muestra:Multicolinealidad", "Tamaño_Muestra:Desbalanceo", "Multicolinealidad:Desbalanceo")
    for (interaction in interactions_2nd_order) {
      interaction_result <- art.con(art_model, interaction, adjust = "tukey")
      interaction_df <- as.data.frame(interaction_result)
      interaction_df$Variable <- interaction
      tukey_results[[interaction]] <- interaction_df
    }
    
    # Interacción de tercer orden
    interaction_3rd_order <- "Tamaño_Muestra:Multicolinealidad:Desbalanceo"
    interaction_result <- art.con(art_model, interaction_3rd_order, adjust = "tukey")
    interaction_df <- as.data.frame(interaction_result)
    interaction_df$Variable <- interaction_3rd_order
    tukey_results[[interaction_3rd_order]] <- interaction_df
    
    # Combinar resultados de Tukey
    tukey_combined <- do.call(rbind, tukey_results)
    tukey_combined$Método <- method
    all_tukey_results[[method]] <- tukey_combined
  } else {
    print(paste("No hay datos suficientes para el método:", method))
  }
}

# Combinar resultados de ANOVA y Tukey
final_art_results <- do.call(rbind, all_art_results)
final_tukey_results <- do.call(rbind, all_tukey_results)

# Guardar resultados en un archivo Excel
write_xlsx(list("_BMC_ART_ANOVA_Results" = final_art_results, "BMC_Tukey_Post_Hoc" = final_tukey_results), "BMC_ARTool_ANOVA_Tamano_Muestra.xlsx")

# Mostrar resultados
View(final_art_results)
View(final_tukey_results)







library(ggplot2)

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



p3 <- ggplot(data = data, aes(x = Desbalanceo, y = SE, colour = Multicolinealidad, group = Multicolinealidad)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 21, fill = "white") +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  labs(y = 'RMSE', x = 'Desequilibrio de clases', colour = 'Multicolinealidad') +
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

print(p3)
