##############################################################################################################################################################################################CINCUENTA####################################################################################################################################################################################################


# Instalar las librerías necesarias y sus dependencias
install.packages(c("MASS", "car", "glmnet", "openxlsx", "dplyr", 'ggplot2', 'tidyr'))

# Cargar librerías necesarias
library(MASS)
library(car)
library(glmnet)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)

# Función para generar datos con diferentes niveles de multicolinealidad
generate_data <- function(sample_size, true_beta0, true_beta1, true_beta2, true_beta3, multicollinearity="none") {
  X1 <- integer(0)
  X2 <- integer(0)
  X3 <- integer(0)
  Y <- integer(0)
  
  count_1 <- 0
  count_0 <- 0
  target_count <- sample_size / 2
  
  while (count_1 < target_count || count_0 < target_count) {
    x1 <- rbinom(1, 1, 0.5)
    
    if (multicollinearity == "none") {
      x2 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
      x3 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
    } else if (multicollinearity == "moderate") {
      x2 <- 3 * x1 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X1
      x3 <- 3.5 * x2 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X2
    } else if (multicollinearity == "high") {
      x2 <- 4 * x1 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X1
      x3 <- 4.5 * x2 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X2
    }
    
    x2 <- cut(x2, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    x3 <- cut(x3, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    
    x2 <- as.numeric(as.character(x2))
    x3 <- as.numeric(as.character(x3))
    
    logit <- true_beta0 + true_beta1 * x1 + true_beta2 * x2 + true_beta3 * x3
    p <- exp(logit) / (1 + exp(logit))
    
    y <- rbinom(1, 1, p)
    
    if (y == 1 && count_1 < target_count) {
      count_1 <- count_1 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    } else if (y == 0 && count_0 < target_count) {
      count_0 <- count_0 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    }
  }
  
  data <- data.frame(Y, X1, X2, X3)
  
  return(data)
}

# Función para ajustar el modelo logístico y obtener los coeficientes y VIFs
get_logistic_coefficients_and_vifs <- function(data) {
  model <- glm(Y ~ X1 + X2 + X3, data = data, family = binomial)
  vifs <- vif(model)
  coefficients <- coef(model)
  names(coefficients) <- paste("logit_coef", names(coefficients), sep = "_")
  names(vifs) <- paste("logit_vif", names(vifs), sep = "_")
  return(list(coefficients = coefficients, vifs = vifs))
}

# Función para ajustar modelos LASSO, Ridge y Elastic Net
get_regularized_models <- function(data) {
  X <- model.matrix(Y ~ X1 + X2 + X3, data)[, -1]
  y <- data$Y
  
  # Ajustar modelo LASSO
  lasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)
  lasso_coef <- as.vector(coef(lasso_model, s = "lambda.min"))
  names(lasso_coef) <- paste("lasso_coef", rownames(coef(lasso_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Ridge
  ridge_model <- cv.glmnet(X, y, family = "binomial", alpha = 0)
  ridge_coef <- as.vector(coef(ridge_model, s = "lambda.min"))
  names(ridge_coef) <- paste("ridge_coef", rownames(coef(ridge_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Elastic Net
  elastic_net_model <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
  elastic_net_coef <- as.vector(coef(elastic_net_model, s = "lambda.min"))
  names(elastic_net_coef) <- paste("enet_coef", rownames(coef(elastic_net_model, s = "lambda.min")), sep = "_")
  
  return(c(lasso_coef, ridge_coef, elastic_net_coef))
}

# Función para calcular el porcentaje de 1 en Y
calculate_percentage_ones <- function(data) {
  mean(data$Y) * 100
}

# Función para calcular el RMSE
calculate_rmse <- function(data, model) {
  predicted <- predict(model, data, type = "response")
  sqrt(mean((data$Y - predicted)^2))
}

# Tamaños de muestra
sample_sizes <- c(250, 500, 1000, 5000, 10000)

# Iteraciones
set.seed(777) # Para reproducibilidad
iterations <- 100
results <- data.frame()

for (sample_size in sample_sizes) {
  for (condition in c("none", "moderate", "high")) {
    condition_results <- list()
    for (i in 1:iterations) {
      data <- generate_data(sample_size, 0.1, 0.5, 1, -0.5, multicollinearity = condition)
      logistic_results <- get_logistic_coefficients_and_vifs(data)
      regularized_results <- get_regularized_models(data)
      percentage_ones <- calculate_percentage_ones(data)
      
      iteration_result <- c(logistic_results$coefficients, logistic_results$vifs, regularized_results, percentage_ones = percentage_ones)
      condition_results[[i]] <- iteration_result
    }
    
    # Convertir listas a data frames
    condition_results_df <- do.call(rbind, condition_results)
    condition_results_df <- as.data.frame(condition_results_df)
    
    # Agregar columnas de tamaño de muestra y condición
    condition_results_df$sample_size <- sample_size
    condition_results_df$condition <- condition
    
    # Agregar resultados al data frame general
    results <- rbind(results, condition_results_df)
  }
}

# Asegurar nombres únicos para las columnas
names(results) <- make.names(names(results), unique = TRUE)

# Calcular medias de las iteraciones por tamaño de muestra y condición
summary_results <- results %>%
  group_by(sample_size, condition) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

# Transformar los datos para la visualización
summary_results_long <- summary_results %>%
  pivot_longer(cols = -c(sample_size, condition), names_to = "metric", values_to = "mean_value")

# Mostrar el resumen transformado
print(summary_results_long)

# Visualizar el resumen
p<- ggplot(summary_results_long, aes(x = sample_size, y = mean_value, color = condition)) +
  geom_line(aes(group = condition)) +
  geom_point() +
  labs(title = "Resumen de medias de iteraciones por tamaño de muestra y condición",
       x = "Tamaño de muestra",
       y = "Media de la métrica",
       color = "Condición") +
  theme_minimal() +
  facet_wrap(~ metric, scales = "free_y", ncol = 2)


# Guardar la imagen en formato SVG
ggsave("summary_results.svg", plot = p, device = "svg")

# Exportar resultados a archivo Excel
write.xlsx(results, file = "cincuenta_resultado.xlsx")

write.xlsx(summary_results, file = "summary_cincuenta_resultado.xlsx")



##############################################################################################################################################################################################SESENTA######################################################################################################################################################################################################
# Instalar y cargar las librerías necesarias
install.packages(c("MASS", "car", "glmnet", "openxlsx", "dplyr", "ggplot2", "tidyr", "smotefamily"))

# Cargar las librerías
library(MASS)
library(car)
library(glmnet)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(smotefamily)


# Función para generar datos con diferentes niveles de multicolinealidad
generate_data <- function(sample_size, true_beta0, true_beta1, true_beta2, true_beta3, multicollinearity="none") {
  X1 <- integer(0)
  X2 <- integer(0)
  X3 <- integer(0)
  Y <- integer(0)
  
  count_1 <- 0
  count_0 <- 0
  target_count_1 <- 0.6 * sample_size
  target_count_0 <- 0.4 * sample_size
  
  while (count_1 < target_count_1 || count_0 < target_count_0) {
    x1 <- rbinom(1, 1, 0.5)
    
    if (multicollinearity == "none") {
      x2 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
      x3 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
    } else if (multicollinearity == "moderate") {
      x2 <- 3 * x1 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X1
      x3 <- 3.5 * x2 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X2
    } else if (multicollinearity == "high") {
      x2 <- 4 * x1 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X1
      x3 <- 4.5 * x2 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X2
    }
    
    x2 <- cut(x2, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    x3 <- cut(x3, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    
    x2 <- as.numeric(as.character(x2))
    x3 <- as.numeric(as.character(x3))
    
    logit <- true_beta0 + true_beta1 * x1 + true_beta2 * x2 + true_beta3 * x3
    p <- exp(logit) / (1 + exp(logit))
    
    y <- rbinom(1, 1, p)
    
    if (y == 1 && count_1 < target_count_1) {
      count_1 <- count_1 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    } else if (y == 0 && count_0 < target_count_0) {
      count_0 <- count_0 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    }
  }
  
  data <- data.frame(Y, X1, X2, X3)
  data[] <- lapply(data, as.numeric)  # Asegurar que todos los datos sean numéricos
  
  return(data)
}

# Función para aplicar SMOTE y limitar la clase mayoritaria
apply_smote <- function(data, K = 5) {
  # Asegurar que los datos sean numéricos
  data_numeric <- as.data.frame(lapply(data, as.numeric))
  
  # Aplicar SMOTE
  smote_result <- SMOTE(X = data_numeric[,-1], target = data_numeric[,1], K = K, dup_size = 1)
  data_smote <- data.frame(smote_result$data)
  colnames(data_smote)[ncol(data_smote)] <- "Y"
  data_smote <- data_smote %>% relocate(Y, .before = everything())
  
  # Limitar la clase mayoritaria y minoritaria a un tamaño máximo igual
  class_counts <- table(data_smote$Y)
  minority_class <- data_smote %>% filter(Y == 1)
  majority_class <- data_smote %>% filter(Y == 0)
  
  max_n <- min(nrow(minority_class), nrow(majority_class))
  
  minority_class <- minority_class[sample(nrow(minority_class), max_n), ]
  majority_class <- majority_class[sample(nrow(majority_class), max_n), ]
  
  data_smote <- rbind(minority_class, majority_class)
  
  data_smote[] <- lapply(data_smote, as.numeric)  # Asegurar que todos los datos sean numéricos
  return(data_smote)
}

# Función para ajustar el modelo logístico y obtener los coeficientes, errores estándar y VIFs
get_logistic_coefficients_and_vifs <- function(data) {
  model <- glm(Y ~ X1 + X2 + X3, data = data, family = binomial)
  vifs <- vif(model)
  coefficients <- coef(model)
  standard_errors <- sqrt(diag(vcov(model)))
  names(coefficients) <- paste("logit_coef", names(coefficients), sep = "_")
  names(vifs) <- paste("logit_vif", names(vifs), sep = "_")
  names(standard_errors) <- paste("logit_se", names(standard_errors), sep = "_")
  return(list(coefficients = coefficients, vifs = vifs, standard_errors = standard_errors))
}

# Función para ajustar modelos LASSO, Ridge y Elastic Net
get_regularized_models <- function(data) {
  X <- model.matrix(Y ~ X1 + X2 + X3, data)[, -1]
  y <- data$Y
  
  # Ajustar modelo LASSO
  lasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)
  lasso_coef <- as.vector(coef(lasso_model, s = "lambda.min"))
  names(lasso_coef) <- paste("lasso_coef", rownames(coef(lasso_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Ridge
  ridge_model <- cv.glmnet(X, y, family = "binomial", alpha = 0)
  ridge_coef <- as.vector(coef(ridge_model, s = "lambda.min"))
  names(ridge_coef) <- paste("ridge_coef", rownames(coef(ridge_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Elastic Net
  elastic_net_model <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
  elastic_net_coef <- as.vector(coef(elastic_net_model, s = "lambda.min"))
  names(elastic_net_coef) <- paste("enet_coef", rownames(coef(elastic_net_model, s = "lambda.min")), sep = "_")
  
  return(c(lasso_coef, ridge_coef, elastic_net_coef))
}

# Función para calcular el porcentaje de 1 en Y
calculate_percentage_ones <- function(data) {
  mean(data$Y) * 100
}

# Función para calcular el RMSE
calculate_rmse <- function(data, model) {
  predicted <- predict(model, data, type = "response")
  sqrt(mean((data$Y - predicted)^2))
}

# Función para calcular error estándar
calculate_standard_error <- function(model) {
  sqrt(diag(vcov(model)))
}

# Tamaños de muestra
sample_sizes <- c(250, 500, 1000, 5000, 10000)

# Iteraciones
set.seed(777) # Para reproducibilidad
iterations <- 100
results <- data.frame()
all_results <- list()

for (sample_size in sample_sizes) {
  for (condition in c("none", "moderate", "high")) {
    iteration_results <- data.frame()
    for (i in 1:iterations) {
      data <- generate_data(sample_size, 0.1, 0.5, 1, -0.5, multicollinearity = condition)
      
      # Resultados sin SMOTE
      logistic_results <- get_logistic_coefficients_and_vifs(data)
      regularized_results <- get_regularized_models(data)
      percentage_ones <- calculate_percentage_ones(data)
      
      # Registrar resultados para Regresión Logística
      for (name in names(logistic_results$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.6,
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results$coefficients[name],
          Error_Estandar = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))]
        ))
      }
      
      # Registrar resultados para LASSO, Ridge y Elastic Net
      method_names <- c("Lasso", "Ridge", "Elastic Net")
      method_coefficients <- list(regularized_results[grep("lasso_coef", names(regularized_results))],
                                  regularized_results[grep("ridge_coef", names(regularized_results))],
                                  regularized_results[grep("enet_coef", names(regularized_results))])
      
      for (m in 1:length(method_names)) {
        for (name in names(method_coefficients[[m]])) {
          iteration_results <- rbind(iteration_results, data.frame(
            Desbalanceo = 0.6,
            Tamaño_Muestra = sample_size,
            Coeficiente = gsub(paste0(method_names[m], "_coef_"), "", name),
            Estimado = method_coefficients[[m]][name],
            Error_Estandar = NA,  # Esto se puede calcular si es necesario
            Desviación_Típica = NA,
            Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            Diferencia = method_coefficients[[m]][name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            VIF = NA,  # VIF no se aplica para LASSO, Ridge y Elastic Net
            Método = method_names[m],
            Multicolinealidad = condition,
            Porcentaje_Unos = percentage_ones,
            BMC = NA,  # Esto se puede calcular si es necesario
            SE = NA  # Esto se puede calcular si es necesario
          ))
        }
      }
      
      # Resultados con SMOTE solo para regresión logística
      data_smote <- apply_smote(data, K = 5)
      logistic_results_smote <- get_logistic_coefficients_and_vifs(data_smote)
      percentage_ones_smote <- calculate_percentage_ones(data_smote)
      
      for (name in names(logistic_results_smote$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.6,
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results_smote$coefficients[name],
          Error_Estandar = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results_smote$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results_smote$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística (SMOTE)",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones_smote,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))]
        ))
      }
    }
    results <- rbind(results, iteration_results)
    all_results[[paste(sample_size, condition, sep = "_")]] <- iteration_results
  }
}

# Calcular la desviación típica para cada combinación de Tamaño_Muestra, Coeficiente y Método
results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método) %>%
  mutate(Desviación_Típica = sd(Estimado))

# Calcular la media de todas las iteraciones
mean_results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método, Multicolinealidad) %>%
  summarise(Media_Estimado = mean(Estimado), Media_Error_Estandar = mean(Error_Estandar), Media_Desviación_Típica = mean(Desviación_Típica), .groups = 'drop')

# Guardar los resultados a un archivo Excel
write.xlsx(results, file = "sesenta_resultado_prueba.xlsx")
write.xlsx(mean_results, file = "media_resultado_prueba.xlsx")

# Mostrar los resultados finales
results
mean_results




##############################################################################################################################################################################################SETENTA######################################################################################################################################################################################################
# Instalar y cargar las librerías necesarias
install.packages(c("MASS", "car", "glmnet", "openxlsx", "dplyr", "ggplot2", "tidyr", "smotefamily"))

# Cargar las librerías
library(MASS)
library(car)
library(glmnet)
library(openxlsx)
library(dplyr)
library(tidyr)
library(smotefamily)

# Función para generar datos con diferentes niveles de multicolinealidad
generate_data <- function(sample_size, true_beta0, true_beta1, true_beta2, true_beta3, multicollinearity="none") {
  X1 <- integer(0)
  X2 <- integer(0)
  X3 <- integer(0)
  Y <- integer(0)
  
  count_1 <- 0
  count_0 <- 0
  target_count_1 <- 0.7 * sample_size  # Ajuste a 70%
  target_count_0 <- 0.3 * sample_size  # Ajuste a 30%
  
  while (count_1 < target_count_1 || count_0 < target_count_0) {
    x1 <- rbinom(1, 1, 0.5)
    
    if (multicollinearity == "none") {
      x2 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
      x3 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
    } else if (multicollinearity == "moderate") {
      x2 <- 3 * x1 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X1
      x3 <- 3.5 * x2 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X2
    } else if (multicollinearity == "high") {
      x2 <- 4 * x1 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X1
      x3 <- 4.5 * x2 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X2
    }
    
    x2 <- cut(x2, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    x3 <- cut(x3, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    
    x2 <- as.numeric(as.character(x2))
    x3 <- as.numeric(as.character(x3))
    
    logit <- true_beta0 + true_beta1 * x1 + true_beta2 * x2 + true_beta3 * x3
    p <- exp(logit) / (1 + exp(logit))
    
    y <- rbinom(1, 1, p)
    
    if (y == 1 && count_1 < target_count_1) {
      count_1 <- count_1 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    } else if (y == 0 && count_0 < target_count_0) {
      count_0 <- count_0 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    }
  }
  
  data <- data.frame(Y, X1, X2, X3)
  data[] <- lapply(data, as.numeric)  # Asegurar que todos los datos sean numéricos
  
  return(data)
}

# Función para aplicar SMOTE y limitar la clase mayoritaria
apply_smote <- function(data, K = 5) {
  # Asegurar que los datos sean numéricos
  data_numeric <- as.data.frame(lapply(data, as.numeric))
  
  # Aplicar SMOTE
  smote_result <- SMOTE(X = data_numeric[,-1], target = data_numeric[,1], K = K, dup_size = 1)
  data_smote <- data.frame(smote_result$data)
  colnames(data_smote)[ncol(data_smote)] <- "Y"
  data_smote <- data_smote %>% relocate(Y, .before = everything())
  
  # Limitar la clase mayoritaria y minoritaria a un tamaño máximo igual
  class_counts <- table(data_smote$Y)
  minority_class <- data_smote %>% filter(Y == 1)
  majority_class <- data_smote %>% filter(Y == 0)
  
  max_n <- min(nrow(minority_class), nrow(majority_class))
  
  minority_class <- minority_class[sample(nrow(minority_class), max_n), ]
  majority_class <- majority_class[sample(nrow(majority_class), max_n), ]
  
  data_smote <- rbind(minority_class, majority_class)
  
  data_smote[] <- lapply(data_smote, as.numeric)  # Asegurar que todos los datos sean numéricos
  return(data_smote)
}

# Función para ajustar el modelo logístico y obtener los coeficientes, errores estándar y VIFs
get_logistic_coefficients_and_vifs <- function(data) {
  model <- glm(Y ~ X1 + X2 + X3, data = data, family = binomial)
  vifs <- vif(model)
  coefficients <- coef(model)
  standard_errors <- sqrt(diag(vcov(model)))
  rmse <- calculate_rmse(data, model)
  names(coefficients) <- paste("logit_coef", names(coefficients), sep = "_")
  names(vifs) <- paste("logit_vif", names(vifs), sep = "_")
  names(standard_errors) <- paste("logit_se", names(standard_errors), sep = "_")
  return(list(coefficients = coefficients, vifs = vifs, standard_errors = standard_errors, rmse = rmse))
}

# Función para ajustar modelos LASSO, Ridge y Elastic Net
get_regularized_models <- function(data) {
  X <- model.matrix(Y ~ X1 + X2 + X3, data)[, -1]
  y <- data$Y
  
  # Ajustar modelo LASSO
  lasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)
  lasso_coef <- as.vector(coef(lasso_model, s = "lambda.min"))
  names(lasso_coef) <- paste("lasso_coef", rownames(coef(lasso_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Ridge
  ridge_model <- cv.glmnet(X, y, family = "binomial", alpha = 0)
  ridge_coef <- as.vector(coef(ridge_model, s = "lambda.min"))
  names(ridge_coef) <- paste("ridge_coef", rownames(coef(ridge_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Elastic Net
  elastic_net_model <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
  elastic_net_coef <- as.vector(coef(elastic_net_model, s = "lambda.min"))
  names(elastic_net_coef) <- paste("enet_coef", rownames(coef(elastic_net_model, s = "lambda.min")), sep = "_")
  
  return(c(lasso_coef, ridge_coef, elastic_net_coef))
}

# Función para calcular el porcentaje de 1 en Y
calculate_percentage_ones <- function(data) {
  mean(data$Y) * 100
}

# Función para calcular el RMSE
calculate_rmse <- function(data, model) {
  predicted <- predict(model, data, type = "response")
  sqrt(mean((data$Y - predicted)^2))
}

# Función para calcular error estándar
calculate_standard_error <- function(model) {
  sqrt(diag(vcov(model)))
}

# Tamaños de muestra
sample_sizes <- c(250, 500, 1000, 5000, 10000)

# Iteraciones
set.seed(777) # Para reproducibilidad
iterations <- 100
results <- data.frame()
all_results <- list()

for (sample_size in sample_sizes) {
  for (condition in c("none", "moderate", "high")) {
    iteration_results <- data.frame()
    for (i in 1:iterations) {
      data <- generate_data(sample_size, 0.1, 0.5, 1, -0.5, multicollinearity = condition)
      
      # Resultados sin SMOTE
      logistic_results <- get_logistic_coefficients_and_vifs(data)
      regularized_results <- get_regularized_models(data)
      percentage_ones <- calculate_percentage_ones(data)
      
      # Registrar resultados para Regresión Logística
      for (name in names(logistic_results$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.7,  # Ajuste a 70%
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results$coefficients[name],
          Error_Estandar = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          RMSE = logistic_results$rmse
        ))
      }
      
      # Registrar resultados para LASSO, Ridge y Elastic Net
      method_names <- c("Lasso", "Ridge", "Elastic Net")
      method_coefficients <- list(regularized_results[grep("lasso_coef", names(regularized_results))],
                                  regularized_results[grep("ridge_coef", names(regularized_results))],
                                  regularized_results[grep("enet_coef", names(regularized_results))])
      
      for (m in 1:length(method_names)) {
        for (name in names(method_coefficients[[m]])) {
          iteration_results <- rbind(iteration_results, data.frame(
            Desbalanceo = 0.7,  # Ajuste a 70%
            Tamaño_Muestra = sample_size,
            Coeficiente = gsub(paste0(method_names[m], "_coef_"), "", name),
            Estimado = method_coefficients[[m]][name],
            Error_Estandar = NA,  # Esto se puede calcular si es necesario
            Desviación_Típica = NA,
            Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            Diferencia = method_coefficients[[m]][name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            VIF = NA,  # VIF no se aplica para LASSO, Ridge y Elastic Net
            Método = method_names[m],
            Multicolinealidad = condition,
            Porcentaje_Unos = percentage_ones,
            BMC = NA,  # Esto se puede calcular si es necesario
            SE = NA,  # Esto se puede calcular si es necesario
            RMSE = NA  # RMSE no se aplica para LASSO, Ridge y Elastic Net en este contexto
          ))
        }
      }
      
      # Resultados con SMOTE solo para regresión logística
      data_smote <- apply_smote(data, K = 5)
      logistic_results_smote <- get_logistic_coefficients_and_vifs(data_smote)
      percentage_ones_smote <- calculate_percentage_ones(data_smote)
      
      for (name in names(logistic_results_smote$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.7,  # Ajuste a 70%
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results_smote$coefficients[name],
          Error_Estandar = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results_smote$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results_smote$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística (SMOTE)",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones_smote,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          RMSE = logistic_results_smote$rmse
        ))
      }
    }
    results <- rbind(results, iteration_results)
    all_results[[paste(sample_size, condition, sep = "_")]] <- iteration_results
  }
}

# Calcular la desviación típica para cada combinación de Tamaño_Muestra, Coeficiente y Método
results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método) %>%
  mutate(Desviación_Típica = sd(Estimado))

# Calcular la media de todas las iteraciones
mean_results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método, Multicolinealidad) %>%
  summarise(Media_Estimado = mean(Estimado), Media_Error_Estandar = mean(Error_Estandar), Media_Desviación_Típica = mean(Desviación_Típica), Media_RMSE = mean(RMSE, na.rm = TRUE), .groups = 'drop')

# Guardar los resultados a un archivo Excel
write.xlsx(results, file = "setenta_resultado_prueba.xlsx")
write.xlsx(mean_results, file = "media_rsenteesultado_prueba.xlsx")

# Mostrar los resultados finales
results
mean_results

#############################################################################################################################################################################################OCHENTENTA####################################################################################################################################################################################################
# Instalar y cargar las librerías necesarias
install.packages(c("MASS", "car", "glmnet", "openxlsx", "dplyr", "ggplot2", "tidyr", "smotefamily"))

# Cargar las librerías
library(MASS)
library(car)
library(glmnet)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(smotefamily)

# Función para generar datos con diferentes niveles de multicolinealidad
generate_data <- function(sample_size, true_beta0, true_beta1, true_beta2, true_beta3, multicollinearity="none") {
  X1 <- integer(0)
  X2 <- integer(0)
  X3 <- integer(0)
  Y <- integer(0)
  
  count_1 <- 0
  count_0 <- 0
  target_count_1 <- 0.8 * sample_size  # Ajuste a 80%
  target_count_0 <- 0.2 * sample_size  # Ajuste a 20%
  
  while (count_1 < target_count_1 || count_0 < target_count_0) {
    x1 <- rbinom(1, 1, 0.5)
    
    if (multicollinearity == "none") {
      x2 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
      x3 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
    } else if (multicollinearity == "moderate") {
      x2 <- 3 * x1 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X1
      x3 <- 3.5 * x2 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X2
    } else if (multicollinearity == "high") {
      x2 <- 4 * x1 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X1
      x3 <- 4.5 * x2 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X2
    }
    
    x2 <- cut(x2, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    x3 <- cut(x3, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    
    x2 <- as.numeric(as.character(x2))
    x3 <- as.numeric(as.character(x3))
    
    logit <- true_beta0 + true_beta1 * x1 + true_beta2 * x2 + true_beta3 * x3
    p <- exp(logit) / (1 + exp(logit))
    
    y <- rbinom(1, 1, p)
    
    if (y == 1 && count_1 < target_count_1) {
      count_1 <- count_1 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    } else if (y == 0 && count_0 < target_count_0) {
      count_0 <- count_0 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    }
  }
  
  data <- data.frame(Y, X1, X2, X3)
  data[] <- lapply(data, as.numeric)  # Asegurar que todos los datos sean numéricos
  
  return(data)
}

# Función para aplicar SMOTE y limitar la clase mayoritaria
apply_smote <- function(data, K = 5) {
  # Asegurar que los datos sean numéricos
  data_numeric <- as.data.frame(lapply(data, as.numeric))
  
  # Aplicar SMOTE
  smote_result <- SMOTE(X = data_numeric[,-1], target = data_numeric[,1], K = K, dup_size = 1)
  data_smote <- data.frame(smote_result$data)
  colnames(data_smote)[ncol(data_smote)] <- "Y"
  data_smote <- data_smote %>% relocate(Y, .before = everything())
  
  # Limitar la clase mayoritaria y minoritaria a un tamaño máximo igual
  class_counts <- table(data_smote$Y)
  minority_class <- data_smote %>% filter(Y == 1)
  majority_class <- data_smote %>% filter(Y == 0)
  
  max_n <- min(nrow(minority_class), nrow(majority_class))
  
  minority_class <- minority_class[sample(nrow(minority_class), max_n), ]
  majority_class <- majority_class[sample(nrow(majority_class), max_n), ]
  
  data_smote <- rbind(minority_class, majority_class)
  
  data_smote[] <- lapply(data_smote, as.numeric)  # Asegurar que todos los datos sean numéricos
  return(data_smote)
}

# Función para ajustar el modelo logístico y obtener los coeficientes, errores estándar y VIFs
get_logistic_coefficients_and_vifs <- function(data) {
  model <- glm(Y ~ X1 + X2 + X3, data = data, family = binomial)
  vifs <- vif(model)
  coefficients <- coef(model)
  standard_errors <- sqrt(diag(vcov(model)))
  rmse <- calculate_rmse(data, model)
  names(coefficients) <- paste("logit_coef", names(coefficients), sep = "_")
  names(vifs) <- paste("logit_vif", names(vifs), sep = "_")
  names(standard_errors) <- paste("logit_se", names(standard_errors), sep = "_")
  return(list(coefficients = coefficients, vifs = vifs, standard_errors = standard_errors, rmse = rmse))
}

# Función para ajustar modelos LASSO, Ridge y Elastic Net
get_regularized_models <- function(data) {
  X <- model.matrix(Y ~ X1 + X2 + X3, data)[, -1]
  y <- data$Y
  
  # Ajustar modelo LASSO
  lasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)
  lasso_coef <- as.vector(coef(lasso_model, s = "lambda.min"))
  names(lasso_coef) <- paste("lasso_coef", rownames(coef(lasso_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Ridge
  ridge_model <- cv.glmnet(X, y, family = "binomial", alpha = 0)
  ridge_coef <- as.vector(coef(ridge_model, s = "lambda.min"))
  names(ridge_coef) <- paste("ridge_coef", rownames(coef(ridge_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Elastic Net
  elastic_net_model <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
  elastic_net_coef <- as.vector(coef(elastic_net_model, s = "lambda.min"))
  names(elastic_net_coef) <- paste("enet_coef", rownames(coef(elastic_net_model, s = "lambda.min")), sep = "_")
  
  return(c(lasso_coef, ridge_coef, elastic_net_coef))
}

# Función para calcular el porcentaje de 1 en Y
calculate_percentage_ones <- function(data) {
  mean(data$Y) * 100
}

# Función para calcular el RMSE
calculate_rmse <- function(data, model) {
  predicted <- predict(model, data, type = "response")
  sqrt(mean((data$Y - predicted)^2))
}

# Función para calcular error estándar
calculate_standard_error <- function(model) {
  sqrt(diag(vcov(model)))
}

# Tamaños de muestra
sample_sizes <- c(250, 500, 1000, 5000, 10000)

# Iteraciones
set.seed(777) # Para reproducibilidad
iterations <- 100
results <- data.frame()
all_results <- list()

for (sample_size in sample_sizes) {
  for (condition in c("none", "moderate", "high")) {
    iteration_results <- data.frame()
    for (i in 1:iterations) {
      data <- generate_data(sample_size, 0.1, 0.5, 1, -0.5, multicollinearity = condition)
      
      # Resultados sin SMOTE
      logistic_results <- get_logistic_coefficients_and_vifs(data)
      regularized_results <- get_regularized_models(data)
      percentage_ones <- calculate_percentage_ones(data)
      
      # Registrar resultados para Regresión Logística
      for (name in names(logistic_results$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.8,  # Ajuste a 80%
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results$coefficients[name],
          Error_Estandar = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          RMSE = logistic_results$rmse
        ))
      }
      
      # Registrar resultados para LASSO, Ridge y Elastic Net
      method_names <- c("Lasso", "Ridge", "Elastic Net")
      method_coefficients <- list(regularized_results[grep("lasso_coef", names(regularized_results))],
                                  regularized_results[grep("ridge_coef", names(regularized_results))],
                                  regularized_results[grep("enet_coef", names(regularized_results))])
      
      for (m in 1:length(method_names)) {
        for (name in names(method_coefficients[[m]])) {
          iteration_results <- rbind(iteration_results, data.frame(
            Desbalanceo = 0.8,  # Ajuste a 80%
            Tamaño_Muestra = sample_size,
            Coeficiente = gsub(paste0(method_names[m], "_coef_"), "", name),
            Estimado = method_coefficients[[m]][name],
            Error_Estandar = NA,  # Esto se puede calcular si es necesario
            Desviación_Típica = NA,
            Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            Diferencia = method_coefficients[[m]][name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            VIF = NA,  # VIF no se aplica para LASSO, Ridge y Elastic Net
            Método = method_names[m],
            Multicolinealidad = condition,
            Porcentaje_Unos = percentage_ones,
            BMC = NA,  # Esto se puede calcular si es necesario
            SE = NA,  # Esto se puede calcular si es necesario
            RMSE = NA  # RMSE no se aplica para LASSO, Ridge y Elastic Net en este contexto
          ))
        }
      }
      
      # Resultados con SMOTE solo para regresión logística
      data_smote <- apply_smote(data, K = 5)
      logistic_results_smote <- get_logistic_coefficients_and_vifs(data_smote)
      percentage_ones_smote <- calculate_percentage_ones(data_smote)
      
      for (name in names(logistic_results_smote$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.8,  # Ajuste a 80%
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results_smote$coefficients[name],
          Error_Estandar = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results_smote$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results_smote$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística (SMOTE)",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones_smote,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          RMSE = logistic_results_smote$rmse
        ))
      }
    }
    results <- rbind(results, iteration_results)
    all_results[[paste(sample_size, condition, sep = "_")]] <- iteration_results
  }
}

# Calcular la desviación típica para cada combinación de Tamaño_Muestra, Coeficiente y Método
results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método) %>%
  mutate(Desviación_Típica = sd(Estimado))

# Calcular la media de todas las iteraciones
mean_results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método, Multicolinealidad) %>%
  summarise(Media_Estimado = mean(Estimado), Media_Error_Estandar = mean(Error_Estandar), Media_Desviación_Típica = mean(Desviación_Típica), Media_RMSE = mean(RMSE, na.rm = TRUE), .groups = 'drop')

# Guardar los resultados a un archivo Excel
write.xlsx(results, file = "ochenta_resultado.xlsx")
write.xlsx(mean_results, file = "media_ochenta.xlsx")

# Mostrar los resultados finales
results
mean_results

# Instalar y cargar las librerías necesarias
install.packages(c("MASS", "car", "glmnet", "openxlsx", "dplyr", "ggplot2", "tidyr", "smotefamily"))

# Cargar las librerías
library(MASS)
library(car)
library(glmnet)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(smotefamily)

# Función para generar datos con diferentes niveles de multicolinealidad
generate_data <- function(sample_size, true_beta0, true_beta1, true_beta2, true_beta3, multicollinearity="none") {
  X1 <- integer(0)
  X2 <- integer(0)
  X3 <- integer(0)
  Y <- integer(0)
  
  count_1 <- 0
  count_0 <- 0
  target_count_1 <- 0.8 * sample_size  # Ajuste a 80%
  target_count_0 <- 0.2 * sample_size  # Ajuste a 20%
  
  while (count_1 < target_count_1 || count_0 < target_count_0) {
    x1 <- rbinom(1, 1, 0.5)
    
    if (multicollinearity == "none") {
      x2 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
      x3 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
    } else if (multicollinearity == "moderate") {
      x2 <- 3 * x1 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X1
      x3 <- 3.5 * x2 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X2
    } else if (multicollinearity == "high") {
      x2 <- 4 * x1 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X1
      x3 <- 4.5 * x2 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X2
    }
    
    x2 <- cut(x2, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    x3 <- cut(x3, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    
    x2 <- as.numeric(as.character(x2))
    x3 <- as.numeric(as.character(x3))
    
    logit <- true_beta0 + true_beta1 * x1 + true_beta2 * x2 + true_beta3 * x3
    p <- exp(logit) / (1 + exp(logit))
    
    y <- rbinom(1, 1, p)
    
    if (y == 1 && count_1 < target_count_1) {
      count_1 <- count_1 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    } else if (y == 0 && count_0 < target_count_0) {
      count_0 <- count_0 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    }
  }
  
  data <- data.frame(Y, X1, X2, X3)
  data[] <- lapply(data, as.numeric)  # Asegurar que todos los datos sean numéricos
  
  return(data)
}

# Función para aplicar SMOTE y limitar la clase mayoritaria
apply_smote <- function(data, K = 5) {
  # Asegurar que los datos sean numéricos
  data_numeric <- as.data.frame(lapply(data, as.numeric))
  
  # Aplicar SMOTE
  smote_result <- SMOTE(X = data_numeric[,-1], target = data_numeric[,1], K = K, dup_size = 1)
  data_smote <- data.frame(smote_result$data)
  colnames(data_smote)[ncol(data_smote)] <- "Y"
  data_smote <- data_smote %>% relocate(Y, .before = everything())
  
  # Limitar la clase mayoritaria y minoritaria a un tamaño máximo igual
  class_counts <- table(data_smote$Y)
  minority_class <- data_smote %>% filter(Y == 1)
  majority_class <- data_smote %>% filter(Y == 0)
  
  max_n <- min(nrow(minority_class), nrow(majority_class))
  
  minority_class <- minority_class[sample(nrow(minority_class), max_n), ]
  majority_class <- majority_class[sample(nrow(majority_class), max_n), ]
  
  data_smote <- rbind(minority_class, majority_class)
  
  data_smote[] <- lapply(data_smote, as.numeric)  # Asegurar que todos los datos sean numéricos
  return(data_smote)
}

# Función para ajustar el modelo logístico y obtener los coeficientes, errores estándar y VIFs
get_logistic_coefficients_and_vifs <- function(data) {
  model <- glm(Y ~ X1 + X2 + X3, data = data, family = binomial)
  vifs <- vif(model)
  coefficients <- coef(model)
  standard_errors <- sqrt(diag(vcov(model)))
  rmse <- calculate_rmse(data, model)
  names(coefficients) <- paste("logit_coef", names(coefficients), sep = "_")
  names(vifs) <- paste("logit_vif", names(vifs), sep = "_")
  names(standard_errors) <- paste("logit_se", names(standard_errors), sep = "_")
  return(list(coefficients = coefficients, vifs = vifs, standard_errors = standard_errors, rmse = rmse))
}

# Función para ajustar modelos LASSO, Ridge y Elastic Net
get_regularized_models <- function(data) {
  X <- model.matrix(Y ~ X1 + X2 + X3, data)[, -1]
  y <- data$Y
  
  # Ajustar modelo LASSO
  lasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)
  lasso_coef <- as.vector(coef(lasso_model, s = "lambda.min"))
  names(lasso_coef) <- paste("lasso_coef", rownames(coef(lasso_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Ridge
  ridge_model <- cv.glmnet(X, y, family = "binomial", alpha = 0)
  ridge_coef <- as.vector(coef(ridge_model, s = "lambda.min"))
  names(ridge_coef) <- paste("ridge_coef", rownames(coef(ridge_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Elastic Net
  elastic_net_model <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
  elastic_net_coef <- as.vector(coef(elastic_net_model, s = "lambda.min"))
  names(elastic_net_coef) <- paste("enet_coef", rownames(coef(elastic_net_model, s = "lambda.min")), sep = "_")
  
  return(c(lasso_coef, ridge_coef, elastic_net_coef))
}

# Función para calcular el porcentaje de 1 en Y
calculate_percentage_ones <- function(data) {
  mean(data$Y) * 100
}

# Función para calcular el RMSE
calculate_rmse <- function(data, model) {
  predicted <- predict(model, data, type = "response")
  sqrt(mean((data$Y - predicted)^2))
}

# Función para calcular error estándar
calculate_standard_error <- function(model) {
  sqrt(diag(vcov(model)))
}

# Tamaños de muestra
sample_sizes <- c(250, 500, 1000, 5000, 10000)

# Iteraciones
set.seed(777) # Para reproducibilidad
iterations <- 100
results <- data.frame()
all_results <- list()

for (sample_size in sample_sizes) {
  for (condition in c("none", "moderate", "high")) {
    iteration_results <- data.frame()
    for (i in 1:iterations) {
      data <- generate_data(sample_size, 0.1, 0.5, 1, -0.5, multicollinearity = condition)
      
      # Resultados sin SMOTE
      logistic_results <- get_logistic_coefficients_and_vifs(data)
      regularized_results <- get_regularized_models(data)
      percentage_ones <- calculate_percentage_ones(data)
      
      # Registrar resultados para Regresión Logística
      for (name in names(logistic_results$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.8,  # Ajuste a 80%
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results$coefficients[name],
          Error_Estandar = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          RMSE = logistic_results$rmse
        ))
      }
      
      # Registrar resultados para LASSO, Ridge y Elastic Net
      method_names <- c("Lasso", "Ridge", "Elastic Net")
      method_coefficients <- list(regularized_results[grep("lasso_coef", names(regularized_results))],
                                  regularized_results[grep("ridge_coef", names(regularized_results))],
                                  regularized_results[grep("enet_coef", names(regularized_results))])
      
      for (m in 1:length(method_names)) {
        for (name in names(method_coefficients[[m]])) {
          iteration_results <- rbind(iteration_results, data.frame(
            Desbalanceo = 0.8,  # Ajuste a 80%
            Tamaño_Muestra = sample_size,
            Coeficiente = gsub(paste0(method_names[m], "_coef_"), "", name),
            Estimado = method_coefficients[[m]][name],
            Error_Estandar = NA,  # Esto se puede calcular si es necesario
            Desviación_Típica = NA,
            Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            Diferencia = method_coefficients[[m]][name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            VIF = NA,  # VIF no se aplica para LASSO, Ridge y Elastic Net
            Método = method_names[m],
            Multicolinealidad = condition,
            Porcentaje_Unos = percentage_ones,
            BMC = NA,  # Esto se puede calcular si es necesario
            SE = NA,  # Esto se puede calcular si es necesario
            RMSE = NA  # RMSE no se aplica para LASSO, Ridge y Elastic Net en este contexto
          ))
        }
      }
      
      # Resultados con SMOTE solo para regresión logística
      data_smote <- apply_smote(data, K = 5)
      logistic_results_smote <- get_logistic_coefficients_and_vifs(data_smote)
      percentage_ones_smote <- calculate_percentage_ones(data_smote)
      
      for (name in names(logistic_results_smote$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.8,  # Ajuste a 80%
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results_smote$coefficients[name],
          Error_Estandar = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results_smote$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results_smote$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística (SMOTE)",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones_smote,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          RMSE = logistic_results_smote$rmse
        ))
      }
    }
    results <- rbind(results, iteration_results)
    all_results[[paste(sample_size, condition, sep = "_")]] <- iteration_results
  }
}

# Calcular la desviación típica para cada combinación de Tamaño_Muestra, Coeficiente y Método
results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método) %>%
  mutate(Desviación_Típica = sd(Estimado))

# Calcular la media de todas las iteraciones
mean_results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método, Multicolinealidad) %>%
  summarise(Media_Estimado = mean(Estimado), Media_Error_Estandar = mean(Error_Estandar), Media_Desviación_Típica = mean(Desviación_Típica), Media_RMSE = mean(RMSE, na.rm = TRUE), .groups = 'drop')

# Guardar los resultados a un archivo Excel
write.xlsx(results, file = "ochenta_resultado.xlsx")
write.xlsx(mean_results, file = "media_ochenta.xlsx")

# Mostrar los resultados finales
results
mean_results

##############################################################################################################################################################################################NOVENTA######################################################################################################################################################################################################

# Instalar y cargar las librerías necesarias
install.packages(c("MASS", "car", "glmnet", "openxlsx", "dplyr", "ggplot2", "tidyr", "smotefamily"))

# Cargar las librerías
library(MASS)
library(car)
library(glmnet)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(smotefamily)

# Función para generar datos con diferentes niveles de multicolinealidad
generate_data <- function(sample_size, true_beta0, true_beta1, true_beta2, true_beta3, multicollinearity="none") {
  X1 <- integer(0)
  X2 <- integer(0)
  X3 <- integer(0)
  Y <- integer(0)
  
  count_1 <- 0
  count_0 <- 0
  target_count_1 <- 0.9 * sample_size  # Ajuste a 90%
  target_count_0 <- 0.1 * sample_size  # Ajuste a 10%
  
  while (count_1 < target_count_1 || count_0 < target_count_0) {
    x1 <- rbinom(1, 1, 0.5)
    
    if (multicollinearity == "none") {
      x2 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
      x3 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
    } else if (multicollinearity == "moderate") {
      x2 <- 3 * x1 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X1
      x3 <- 3.5 * x2 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X2
    } else if (multicollinearity == "high") {
      x2 <- 4 * x1 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X1
      x3 <- 4.5 * x2 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X2
    }
    
    x2 <- cut(x2, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    x3 <- cut(x3, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    
    x2 <- as.numeric(as.character(x2))
    x3 <- as.numeric(as.character(x3))
    
    logit <- true_beta0 + true_beta1 * x1 + true_beta2 * x2 + true_beta3 * x3
    p <- exp(logit) / (1 + exp(logit))
    
    y <- rbinom(1, 1, p)
    
    if (y == 1 && count_1 < target_count_1) {
      count_1 <- count_1 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    } else if (y == 0 && count_0 < target_count_0) {
      count_0 <- count_0 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    }
  }
  
  data <- data.frame(Y, X1, X2, X3)
  data[] <- lapply(data, as.numeric)  # Asegurar que todos los datos sean numéricos
  
  return(data)
}

# Función para aplicar SMOTE y limitar la clase mayoritaria
apply_smote <- function(data, K = 5) {
  # Asegurar que los datos sean numéricos
  data_numeric <- as.data.frame(lapply(data, as.numeric))
  
  # Aplicar SMOTE
  smote_result <- SMOTE(X = data_numeric[,-1], target = data_numeric[,1], K = K, dup_size = 1)
  data_smote <- data.frame(smote_result$data)
  colnames(data_smote)[ncol(data_smote)] <- "Y"
  data_smote <- data_smote %>% relocate(Y, .before = everything())
  
  # Limitar la clase mayoritaria y minoritaria a un tamaño máximo igual
  class_counts <- table(data_smote$Y)
  minority_class <- data_smote %>% filter(Y == 1)
  majority_class <- data_smote %>% filter(Y == 0)
  
  max_n <- min(nrow(minority_class), nrow(majority_class))
  
  minority_class <- minority_class[sample(nrow(minority_class), max_n), ]
  majority_class <- majority_class[sample(nrow(majority_class), max_n), ]
  
  data_smote <- rbind(minority_class, majority_class)
  
  data_smote[] <- lapply(data_smote, as.numeric)  # Asegurar que todos los datos sean numéricos
  return(data_smote)
}

# Función para ajustar el modelo logístico y obtener los coeficientes, errores estándar y VIFs
get_logistic_coefficients_and_vifs <- function(data) {
  model <- glm(Y ~ X1 + X2 + X3, data = data, family = binomial)
  vifs <- vif(model)
  coefficients <- coef(model)
  standard_errors <- sqrt(diag(vcov(model)))
  rmse <- calculate_rmse(data, model)
  names(coefficients) <- paste("logit_coef", names(coefficients), sep = "_")
  names(vifs) <- paste("logit_vif", names(vifs), sep = "_")
  names(standard_errors) <- paste("logit_se", names(standard_errors), sep = "_")
  return(list(coefficients = coefficients, vifs = vifs, standard_errors = standard_errors, rmse = rmse))
}

# Función para ajustar modelos LASSO, Ridge y Elastic Net
get_regularized_models <- function(data) {
  X <- model.matrix(Y ~ X1 + X2 + X3, data)[, -1]
  y <- data$Y
  
  # Ajustar modelo LASSO
  lasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)
  lasso_coef <- as.vector(coef(lasso_model, s = "lambda.min"))
  names(lasso_coef) <- paste("lasso_coef", rownames(coef(lasso_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Ridge
  ridge_model <- cv.glmnet(X, y, family = "binomial", alpha = 0)
  ridge_coef <- as.vector(coef(ridge_model, s = "lambda.min"))
  names(ridge_coef) <- paste("ridge_coef", rownames(coef(ridge_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Elastic Net
  elastic_net_model <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
  elastic_net_coef <- as.vector(coef(elastic_net_model, s = "lambda.min"))
  names(elastic_net_coef) <- paste("enet_coef", rownames(coef(elastic_net_model, s = "lambda.min")), sep = "_")
  
  return(c(lasso_coef, ridge_coef, elastic_net_coef))
}

# Función para calcular el porcentaje de 1 en Y
calculate_percentage_ones <- function(data) {
  mean(data$Y) * 100
}

# Función para calcular el RMSE
calculate_rmse <- function(data, model) {
  predicted <- predict(model, data, type = "response")
  sqrt(mean((data$Y - predicted)^2))
}

# Función para calcular error estándar
calculate_standard_error <- function(model) {
  sqrt(diag(vcov(model)))
}

# Tamaños de muestra
sample_sizes <- c(250, 500, 1000, 5000, 10000)

# Iteraciones
set.seed(777) # Para reproducibilidad
iterations <- 100
results <- data.frame()
all_results <- list()

for (sample_size in sample_sizes) {
  for (condition in c("none", "moderate", "high")) {
    iteration_results <- data.frame()
    for (i in 1:iterations) {
      data <- generate_data(sample_size, 0.1, 0.5, 1, -0.5, multicollinearity = condition)
      
      # Resultados sin SMOTE
      logistic_results <- get_logistic_coefficients_and_vifs(data)
      regularized_results <- get_regularized_models(data)
      percentage_ones <- calculate_percentage_ones(data)
      
      # Registrar resultados para Regresión Logística
      for (name in names(logistic_results$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.9,  # Ajuste a 90%
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results$coefficients[name],
          Error_Estandar = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          RMSE = logistic_results$rmse
        ))
      }
      
      # Registrar resultados para LASSO, Ridge y Elastic Net
      method_names <- c("Lasso", "Ridge", "Elastic Net")
      method_coefficients <- list(regularized_results[grep("lasso_coef", names(regularized_results))],
                                  regularized_results[grep("ridge_coef", names(regularized_results))],
                                  regularized_results[grep("enet_coef", names(regularized_results))])
      
      for (m in 1:length(method_names)) {
        for (name in names(method_coefficients[[m]])) {
          iteration_results <- rbind(iteration_results, data.frame(
            Desbalanceo = 0.9,  # Ajuste a 90%
            Tamaño_Muestra = sample_size,
            Coeficiente = gsub(paste0(method_names[m], "_coef_"), "", name),
            Estimado = method_coefficients[[m]][name],
            Error_Estandar = NA,  # Esto se puede calcular si es necesario
            Desviación_Típica = NA,
            Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            Diferencia = method_coefficients[[m]][name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            VIF = NA,  # VIF no se aplica para LASSO, Ridge y Elastic Net
            Método = method_names[m],
            Multicolinealidad = condition,
            Porcentaje_Unos = percentage_ones,
            BMC = NA,  # Esto se puede calcular si es necesario
            SE = NA,  # Esto se puede calcular si es necesario
            RMSE = NA  # RMSE no se aplica para LASSO, Ridge y Elastic Net en este contexto
          ))
        }
      }
      
      # Resultados con SMOTE solo para regresión logística
      data_smote <- apply_smote(data, K = 5)
      logistic_results_smote <- get_logistic_coefficients_and_vifs(data_smote)
      percentage_ones_smote <- calculate_percentage_ones(data_smote)
      
      for (name in names(logistic_results_smote$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.9,  # Ajuste a 90%
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results_smote$coefficients[name],
          Error_Estandar = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results_smote$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results_smote$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística (SMOTE)",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones_smote,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          RMSE = logistic_results_smote$rmse
        ))
      }
    }
    results <- rbind(results, iteration_results)
    all_results[[paste(sample_size, condition, sep = "_")]] <- iteration_results
  }
}

# Calcular la desviación típica para cada combinación de Tamaño_Muestra, Coeficiente y Método
results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método) %>%
  mutate(Desviación_Típica = sd(Estimado))

# Calcular la media de todas las iteraciones
mean_results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método, Multicolinealidad) %>%
  summarise(Media_Estimado = mean(Estimado), Media_Error_Estandar = mean(Error_Estandar), Media_Desviación_Típica = mean(Desviación_Típica), Media_RMSE = mean(RMSE, na.rm = TRUE), .groups = 'drop')

# Guardar los resultados a un archivo Excel
write.xlsx(results, file = "noventa_resultado_prueba.xlsx")
write.xlsx(mean_results, file = "media_resultado_prueba.xlsx")

# Mostrar los resultados finales
results
mean_results


################################################################################################################################################################################NOVENTA Y CINCO########################################################################################################################################################################################################################################

# Instalar y cargar las librerías necesarias
install.packages(c("MASS", "car", "glmnet", "openxlsx", "dplyr", "ggplot2", "tidyr", "smotefamily"))

# Cargar las librerías
library(MASS)
library(car)
library(glmnet)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(smotefamily)

# Función para generar datos con diferentes niveles de multicolinealidad
generate_data <- function(sample_size, true_beta0, true_beta1, true_beta2, true_beta3, multicollinearity="none") {
  X1 <- integer(0)
  X2 <- integer(0)
  X3 <- integer(0)
  Y <- integer(0)
  
  count_1 <- 0
  count_0 <- 0
  target_count_1 <- 0.95 * sample_size  # Ajuste a 95%
  target_count_0 <- 0.05 * sample_size  # Ajuste a 5%
  
  while (count_1 < target_count_1 || count_0 < target_count_0) {
    x1 <- rbinom(1, 1, 0.5)
    
    if (multicollinearity == "none") {
      x2 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
      x3 <- sample(1:5, 1, replace = TRUE) # Variable ordinal independiente
    } else if (multicollinearity == "moderate") {
      x2 <- 3 * x1 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X1
      x3 <- 3.5 * x2 + rnorm(1, mean = 0, sd = 0.7) # Mayor correlación con X2
    } else if (multicollinearity == "high") {
      x2 <- 4 * x1 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X1
      x3 <- 4.5 * x2 + rnorm(1, mean = 0, sd = 0.5) # Mucha mayor correlación con X2
    }
    
    x2 <- cut(x2, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    x3 <- cut(x3, breaks = c(-Inf, 1, 2, 3, 4, Inf), labels = 1:5, include.lowest = TRUE)
    
    x2 <- as.numeric(as.character(x2))
    x3 <- as.numeric(as.character(x3))
    
    logit <- true_beta0 + true_beta1 * x1 + true_beta2 * x2 + true_beta3 * x3
    p <- exp(logit) / (1 + exp(logit))
    
    y <- rbinom(1, 1, p)
    
    if (y == 1 && count_1 < target_count_1) {
      count_1 <- count_1 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    } else if (y == 0 && count_0 < target_count_0) {
      count_0 <- count_0 + 1
      X1 <- c(X1, x1)
      X2 <- c(X2, x2)
      X3 <- c(X3, x3)
      Y <- c(Y, y)
    }
  }
  
  data <- data.frame(Y, X1, X2, X3)
  data[] <- lapply(data, as.numeric)  # Asegurar que todos los datos sean numéricos
  
  return(data)
}

# Función para aplicar SMOTE y limitar la clase mayoritaria
apply_smote <- function(data, K = 5) {
  # Asegurar que los datos sean numéricos
  data_numeric <- as.data.frame(lapply(data, as.numeric))
  
  # Aplicar SMOTE
  smote_result <- SMOTE(X = data_numeric[,-1], target = data_numeric[,1], K = K, dup_size = 1)
  data_smote <- data.frame(smote_result$data)
  colnames(data_smote)[ncol(data_smote)] <- "Y"
  data_smote <- data_smote %>% relocate(Y, .before = everything())
  
  # Limitar la clase mayoritaria y minoritaria a un tamaño máximo igual
  class_counts <- table(data_smote$Y)
  minority_class <- data_smote %>% filter(Y == 1)
  majority_class <- data_smote %>% filter(Y == 0)
  
  max_n <- min(nrow(minority_class), nrow(majority_class))
  
  minority_class <- minority_class[sample(nrow(minority_class), max_n), ]
  majority_class <- majority_class[sample(nrow(majority_class), max_n), ]
  
  data_smote <- rbind(minority_class, majority_class)
  
  data_smote[] <- lapply(data_smote, as.numeric)  # Asegurar que todos los datos sean numéricos
  return(data_smote)
}

# Función para ajustar el modelo logístico y obtener los coeficientes, errores estándar y VIFs
get_logistic_coefficients_and_vifs <- function(data) {
  model <- glm(Y ~ X1 + X2 + X3, data = data, family = binomial)
  vifs <- vif(model)
  coefficients <- coef(model)
  standard_errors <- sqrt(diag(vcov(model)))
  rmse <- calculate_rmse(data, model)
  names(coefficients) <- paste("logit_coef", names(coefficients), sep = "_")
  names(vifs) <- paste("logit_vif", names(vifs), sep = "_")
  names(standard_errors) <- paste("logit_se", names(standard_errors), sep = "_")
  return(list(coefficients = coefficients, vifs = vifs, standard_errors = standard_errors, rmse = rmse))
}

# Función para ajustar modelos LASSO, Ridge y Elastic Net
get_regularized_models <- function(data) {
  X <- model.matrix(Y ~ X1 + X2 + X3, data)[, -1]
  y <- data$Y
  
  # Ajustar modelo LASSO
  lasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)
  lasso_coef <- as.vector(coef(lasso_model, s = "lambda.min"))
  names(lasso_coef) <- paste("lasso_coef", rownames(coef(lasso_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Ridge
  ridge_model <- cv.glmnet(X, y, family = "binomial", alpha = 0)
  ridge_coef <- as.vector(coef(ridge_model, s = "lambda.min"))
  names(ridge_coef) <- paste("ridge_coef", rownames(coef(ridge_model, s = "lambda.min")), sep = "_")
  
  # Ajustar modelo Elastic Net
  elastic_net_model <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
  elastic_net_coef <- as.vector(coef(elastic_net_model, s = "lambda.min"))
  names(elastic_net_coef) <- paste("enet_coef", rownames(coef(elastic_net_model, s = "lambda.min")), sep = "_")
  
  return(c(lasso_coef, ridge_coef, elastic_net_coef))
}

# Función para calcular el porcentaje de 1 en Y
calculate_percentage_ones <- function(data) {
  mean(data$Y) * 100
}

# Función para calcular el RMSE
calculate_rmse <- function(data, model) {
  predicted <- predict(model, data, type = "response")
  sqrt(mean((data$Y - predicted)^2))
}

# Función para calcular error estándar
calculate_standard_error <- function(model) {
  sqrt(diag(vcov(model)))
}

# Tamaños de muestra
sample_sizes <- c(250, 500, 1000, 5000, 10000)

# Iteraciones
set.seed(777) # Para reproducibilidad
iterations <- 100
results <- data.frame()
all_results <- list()

for (sample_size in sample_sizes) {
  for (condition in c("none", "moderate", "high")) {
    iteration_results <- data.frame()
    for (i in 1:iterations) {
      data <- generate_data(sample_size, 0.1, 0.5, 1, -0.5, multicollinearity = condition)
      
      # Resultados sin SMOTE
      logistic_results <- get_logistic_coefficients_and_vifs(data)
      regularized_results <- get_regularized_models(data)
      percentage_ones <- calculate_percentage_ones(data)
      
      # Registrar resultados para Regresión Logística
      for (name in names(logistic_results$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.95,  # Ajuste a 95%
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results$coefficients[name],
          Error_Estandar = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          RMSE = logistic_results$rmse
        ))
      }
      
      # Registrar resultados para LASSO, Ridge y Elastic Net
      method_names <- c("Lasso", "Ridge", "Elastic Net")
      method_coefficients <- list(regularized_results[grep("lasso_coef", names(regularized_results))],
                                  regularized_results[grep("ridge_coef", names(regularized_results))],
                                  regularized_results[grep("enet_coef", names(regularized_results))])
      
      for (m in 1:length(method_names)) {
        for (name in names(method_coefficients[[m]])) {
          iteration_results <- rbind(iteration_results, data.frame(
            Desbalanceo = 0.95,  # Ajuste a 95%
            Tamaño_Muestra = sample_size,
            Coeficiente = gsub(paste0(method_names[m], "_coef_"), "", name),
            Estimado = method_coefficients[[m]][name],
            Error_Estandar = NA,  # Esto se puede calcular si es necesario
            Desviación_Típica = NA,
            Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            Diferencia = method_coefficients[[m]][name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
            VIF = NA,  # VIF no se aplica para LASSO, Ridge y Elastic Net
            Método = method_names[m],
            Multicolinealidad = condition,
            Porcentaje_Unos = percentage_ones,
            BMC = NA,  # Esto se puede calcular si es necesario
            SE = NA,  # Esto se puede calcular si es necesario
            RMSE = NA  # RMSE no se aplica para LASSO, Ridge y Elastic Net en este contexto
          ))
        }
      }
      
      # Resultados con SMOTE solo para regresión logística
      data_smote <- apply_smote(data, K = 5)
      logistic_results_smote <- get_logistic_coefficients_and_vifs(data_smote)
      percentage_ones_smote <- calculate_percentage_ones(data_smote)
      
      for (name in names(logistic_results_smote$coefficients)) {
        iteration_results <- rbind(iteration_results, data.frame(
          Desbalanceo = 0.95,  # Ajuste a 95%
          Tamaño_Muestra = sample_size,
          Coeficiente = name,
          Estimado = logistic_results_smote$coefficients[name],
          Error_Estandar = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          Desviación_Típica = NA,
          Verdadero = ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          Diferencia = logistic_results_smote$coefficients[name] - ifelse(grepl("Intercept", name), 0.1, ifelse(grepl("X1", name), 0.5, ifelse(grepl("X2", name), 1, -0.5))),
          VIF = ifelse(name == "logit_coef_(Intercept)", NA, logistic_results_smote$vifs[paste("logit_vif", gsub("logit_coef_", "", name), sep = "_")]),
          Método = "Regresión Logística (SMOTE)",
          Multicolinealidad = condition,
          Porcentaje_Unos = percentage_ones_smote,
          BMC = NA,  # Esto se puede calcular si es necesario
          SE = logistic_results_smote$standard_errors[paste0("logit_se", gsub("logit_coef", "", name))],
          RMSE = logistic_results_smote$rmse
        ))
      }
    }
    results <- rbind(results, iteration_results)
    all_results[[paste(sample_size, condition, sep = "_")]] <- iteration_results
  }
}

# Calcular la desviación típica para cada combinación de Tamaño_Muestra, Coeficiente y Método
results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método) %>%
  mutate(Desviación_Típica = sd(Estimado))

# Calcular la media de todas las iteraciones
mean_results <- results %>%
  group_by(Tamaño_Muestra, Coeficiente, Método, Multicolinealidad) %>%
  summarise(Media_Estimado = mean(Estimado), Media_Error_Estandar = mean(Error_Estandar), Media_Desviación_Típica = mean(Desviación_Típica), Media_RMSE = mean(RMSE, na.rm = TRUE), .groups = 'drop')

# Guardar los resultados a un archivo Excel
write.xlsx(results, file = "n95_resultado_prueba.xlsx")
write.xlsx(mean_results, file = "95_resultado_prueba.xlsx")

# Mostrar los resultados finales
results
mean_results

View(results)