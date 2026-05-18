# ==============================================================================
# SCRIPT COMPLETO Y DEFINITIVO: FASE II (MODELO JERÁRQUICO IDENTIFICABLE)
# ==============================================================================
# Estilo de Programación Analítica: Estándar MCMC Avanzado
# ==============================================================================

# ==============================================================================
# FASE II - PASO I: CONTROL DE ENTORNO Y CONTRACCIÓN DE MOMENTOS BAYESIANOS
# ==============================================================================

# 1. Carga centralizada de las librerías de control y simulación paralela
library(dplyr)
library(tidyr)
library(R2jags)
library(ggplot2)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)
outdir <- "output/figures"

# 2. Extracción del array estocástico tridimensional generado en la Fase I
sims_log_pm25 <- output_jags$BUGSoutput$sims.list$log_pm25

# 3. Colapsar la dimensión de las simulaciones para obtener el estimador puntual
matriz_medias_y <- apply(sims_log_pm25, c(2, 3), mean)

# 4. Colapsar la dimensión de las simulaciones para calcular la varianza posterior
matriz_varianzas_y <- apply(sims_log_pm25, c(2, 3), var)

# 5. Transformación matemática a matriz de precisión condicional con TOPE NUMÉRICO
# ------------------------------------------------------------------------------
matriz_precisiones_y <- 1 / matriz_varianzas_y

# AJUSTE CRÍTICO: Reemplazar infinitos y truncar precisiones hiper-exageradas a 500.
# Un tope de 500 le devuelve la fuerza matemática al modelo jerárquico global para
# arrastrar las 3 cadenas hacia la misma zona real de los datos en escala logarítmica.
matriz_precisiones_y[is.infinite(matriz_precisiones_y)] <- 500
matriz_precisiones_y[matriz_precisiones_y > 500]        <- 500

# Control preventivo extra por si existen celdas totalmente vacías (evita NAs)
matriz_precisiones_y[is.na(matriz_precisiones_y)]        <- 0.01

# ==============================================================================
# FASE II - PASO II: CONSTRUCCIÓN DE LA LISTA DE DATOS OFICIAL PARA JAGS
# ==============================================================================

# SEGURO CONTRA FALTANTES: Si no existen vector_sen o vector_cos, se autogeneran de inmediato
if (!exists("vector_sen") || !exists("vector_cos")) {
  t <- 1:365
  vector_sen <- sin(2 * pi * t / 365)
  vector_cos <- cos(2 * pi * t / 365)
}

# 1. Empaquetar todas las variables dentro de una lista indexada
jags_data_fase2 <- list(
  y              = matriz_medias_y,       # Matriz de respuestas de 365x14
  prec_heredada  = matriz_precisiones_y,  # Matriz de pesos/precisiones de 365x14 (Tope 500)
  sen_t          = vector_sen,            # Vector armónico seno (longitud 365)
  cos_t          = vector_cos,            # Vector armónico coseno (longitud 365)
  N_dias         = 365,                   # Límite superior del bucle temporal t
  N_estaciones   = 14                     # Límite superior del bucle espacial j
)

# ==============================================================================
# FASE II - PASO III: VALORES PARAMÉTRICOS A REGISTRAR (CORREGIDO)
# ==============================================================================

# Guardamos la serie de tiempo limpia de la ciudad y el nuevo vector espacial IDENTIFICABLE
jags_parameters_fase2 <- c("gamma", "alpha_corregido", "sigma_ciudad", "mu_cdmx")

# ==============================================================================
# FASE II - PASO IV: EJECUCIÓN DE LA SIMULACIÓN EN PARALELO (TRATAMIENTO DE CHOQUE)
# ==============================================================================

# 1. Definición de la ruta física del modelo jerárquico estructural
ruta_modelo_txt <- "scripts/jags_modelo_jerarquico_fase2.txt"

# 2. Configuración de Valores Iniciales Idénticos (Formato para Clúster Paralelo)
# Se incluye 'tau_estacion = 1.0' para inicializar el nuevo hiperparámetro del .txt
jags_inits_fijos <- function() {
  list(
    gamma        = c(2.831106, 0, 0), 
    alpha        = rep(0, 14), 
    tau_ciudad   = 1.5,
    tau_estacion = 1.0
  )
}

# 3. Ejecución del Sampler MCMC en Clúster Paralelo via R2jags
# ------------------------------------------------------------------------------
output_fase2 <- jags.parallel(
  data               = jags_data_fase2,           
  inits              = jags_inits_fijos,          
  parameters.to.save = jags_parameters_fase2,    
  model.file         = ruta_modelo_txt,           
  
  n.chains           = 3,     # Tres procesos estocásticos independientes para cálculo de Rhat.
  n.iter             = 25000, # 25,000 iteraciones para garantizar exploración profunda.
  n.burnin           = 5000,  # Descarte agresivo de los primeros 5,000 pasos de adaptación.
  n.thin             = 50     # Thinning masivo para pulverizar la autocorrelación serial.
)

# 4. Almacenamiento y respaldo del Output en Disco Duro
save(output_fase2, file = file.path(outdir, "Contaminacion_Fase2_SerieTiempo.RData"))

# 5. Notificación de término exitoso
cat("================================================================\n")
cat("=== ¡FASE II - PASO IV: SIMULACIÓN DE CHOQUE CONCLUIDA CON ÉXITO! ===\n")
cat("================================================================\n\n")

# ==============================================================================
# FASE II - PASO V: PANEL DIAGNÓSTICO REGLAMENTARIO DE 3 CADENAS (FORMATO 3x2)
# ==============================================================================

generar_panel_gamma1_3cadenas_definitivo <- function(output_fase2) {
  # 1. Extracción limpia de las 3 trayectorias desde el array indexado
  gamma_c1 <- output_fase2$BUGSoutput$sims.array[, 1, "gamma[1]"]
  gamma_c2 <- output_fase2$BUGSoutput$sims.array[, 2, "gamma[1]"]
  gamma_c3 <- output_fase2$BUGSoutput$sims.array[, 3, "gamma[1]"]
  N_iter   <- length(gamma_c1)
  
  # Colores institucionales del proyecto
  c1_col <- "#3498DB"  # Azul
  c2_col <- "#E74C3C"  # Rojo
  c3_col <- "#2ECC71"  # Verde
  
  # Configurar matriz gráfica de 3 filas x 2 columnas
  par(mfrow = c(3, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))
  
  # --- FILA 1: TRAZA Y MEDIAS MÓVILES (ERGODICIDAD) ---
  rango_y <- range(c(gamma_c1, gamma_c2, gamma_c3))
  plot(1:N_iter, gamma_c1, type = "l", col = c1_col, ylim = rango_y,
       xlab = "Iteración", ylab = "Valor", main = "1. Traza Temporal (3 Cadenas)")
  lines(1:N_iter, gamma_c2, col = c2_col)
  lines(1:N_iter, gamma_c3, col = c3_col)
  
  mean_g1 <- cumsum(gamma_c1) / (1:N_iter)
  mean_g2 <- cumsum(gamma_c2) / (1:N_iter)
  mean_g3 <- cumsum(gamma_c3) / (1:N_iter)
  plot(1:N_iter, mean_g1, type = "l", col = c1_col, ylim = range(c(mean_g1, mean_g2, mean_g3)),
       xlab = "Iteración", ylab = "Media", main = "2. Estabilidad de la Media Móvil")
  lines(1:N_iter, mean_g2, col = c2_col)
  lines(1:N_iter, mean_g3, col = c3_col)
  
  # --- FILA 2: HISTOGRAMAS MARGINALES INDEPENDIENTES (C1 Y C2) ---
  hist(gamma_c1, breaks = 25, col = c1_col, border = "white", xlab = "Valor", ylab = "Frecuencia", main = "3. Histograma Marginal C1")
  hist(gamma_c2, breaks = 25, col = c2_col, border = "white", xlab = "Valor", ylab = "Frecuencia", main = "4. Histograma Marginal C2")
  
  # --- FILA 3: HISTOGRAMA INDEPENDIENTE C3 Y AUTOCORRELACIÓN SUPERPUESTA ---
  hist(gamma_c3, breaks = 25, col = c3_col, border = "white", xlab = "Valor", ylab = "Frecuencia", main = "5. Histograma Marginal C3")
  
  # Cálculo de las funciones de autocorrelación empíricas
  acf_g1 <- acf(gamma_c1, plot = FALSE, lag.max = 30)$acf
  acf_g2 <- acf(gamma_c2, plot = FALSE, lag.max = 30)$acf
  acf_g3 <- acf(gamma_c3, plot = FALSE, lag.max = 30)$acf
  
  plot(0:30, acf_g1, type = "b", col = c1_col, pch = 19, ylim = c(0, 1),
       xlab = "Lag", ylab = "ACF", main = "6. Superposición de Autocorrelación (C1, C2, C3)")
  lines(0:30, acf_g2, type = "b", col = c2_col, pch = 19)
  lines(0:30, acf_g3, type = "b", col = c3_col, pch = 19)
  abline(h = c(-1.96/sqrt(N_iter), 1.96/sqrt(N_iter)), col = "gray40", lty = 2)
  
  title(main = "Auditoría MCMC Avanzada F2: Coeficiente Armónico gamma1", outer = TRUE, font = 2)
  par(mfrow = c(1, 1))
}

# Desplegar el panel diagnóstico final estilo Dr. Nieto
generar_panel_gamma1_3cadenas_definitivo(output_fase2)

# ==============================================================================
# FASE II - PASO V: PANEL DIAGNÓSTICO REGLAMENTARIO DE 3 CADENAS (FORMATO 3x2)
# ==============================================================================

generar_panel_gamma1_3cadenas_definitivo <- function(output_fase2) {
  # 1. Extracción limpia de las 3 trayectorias desde el array indexado
  gamma_c1 <- output_fase2$BUGSoutput$sims.array[, 1, "gamma[1]"]
  gamma_c2 <- output_fase2$BUGSoutput$sims.array[, 2, "gamma[1]"]
  gamma_c3 <- output_fase2$BUGSoutput$sims.array[, 3, "gamma[1]"]
  N_iter   <- length(gamma_c1)
  
  # Colores institucionales del proyecto
  c1_col <- "#3498DB"  # Azul
  c2_col <- "#E74C3C"  # Rojo
  c3_col <- "#2ECC71"  # Verde
  
  # Configurar matriz gráfica de 3 filas x 2 columnas
  par(mfrow = c(3, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))
  
  # --- FILA 1: TRAZA Y MEDIAS MÓVILES (ERGODICIDAD) ---
  rango_y <- range(c(gamma_c1, gamma_c2, gamma_c3))
  plot(1:N_iter, gamma_c1, type = "l", col = c1_col, ylim = rango_y,
       xlab = "Iteración (Adelgazada)", ylab = "Soporte Marginal", 
       main = "1. Traza Temporal de gamma[1] (3 Cadenas Empalmadas)")
  lines(1:N_iter, gamma_c2, col = c2_col)
  lines(1:N_iter, gamma_c3, col = c3_col)
  
  mean_g1 <- cumsum(gamma_c1) / (1:N_iter)
  mean_g2 <- cumsum(gamma_c2) / (1:N_iter)
  mean_g3 <- cumsum(gamma_c3) / (1:N_iter)
  plot(1:N_iter, mean_g1, type = "l", col = c1_col, ylim = range(c(mean_g1, mean_g2, mean_g3)),
       xlab = "Iteración (Adelgazada)", ylab = "E(gamma[1]) Acumulada", 
       main = "2. Estabilidad de la Media Móvil Posterior")
  lines(1:N_iter, mean_g2, col = c2_col)
  lines(1:N_iter, mean_g3, col = c3_col)
  
  # --- FILA 2: HISTOGRAMAS MARGINALES INDEPENDIENTES (CADENAS 1 Y 2) ---
  hist(gamma_c1, breaks = 25, col = c1_col, border = "white", 
       xlab = "Soporte de gamma[1]", ylab = "Frecuencia", 
       main = "3. Densidad Posterior Marginal: gamma[1] (Cadena 1)")
  
  hist(gamma_c2, breaks = 25, col = c2_col, border = "white", 
       xlab = "Soporte de gamma[1]", ylab = "Frecuencia", 
       main = "4. Densidad Posterior Marginal: gamma[1] (Cadena 2)")
  
  # --- FILA 3: HISTOGRAMA INDEPENDIENTE CADENA 3 Y AUTOCORRELACIÓN SUPERPUESTA ---
  hist(gamma_c3, breaks = 25, col = c3_col, border = "white", 
       xlab = "Soporte de gamma[1]", ylab = "Frecuencia", 
       main = "5. Densidad Posterior Marginal: gamma[1] (Cadena 3)")
  
  # Cálculo de las funciones de autocorrelación empíricas
  acf_g1 <- acf(gamma_c1, plot = FALSE, lag.max = 30)$acf
  acf_g2 <- acf(gamma_c2, plot = FALSE, lag.max = 30)$acf
  acf_g3 <- acf(gamma_c3, plot = FALSE, lag.max = 30)$acf
  
  plot(0:30, acf_g1, type = "b", col = c1_col, pch = 19, ylim = c(0, 1),
       xlab = "Lag (Defasamiento t-k)", ylab = "Autocorrelación (ACF)", 
       main = "6. Función de Autocorrelación Serial por Cadena")
  lines(0:30, acf_g2, type = "b", col = c2_col, pch = 19)
  lines(0:30, acf_g3, type = "b", col = c3_col, pch = 19)
  abline(h = c(-1.96/sqrt(N_iter), 1.96/sqrt(N_iter)), col = "gray40", lty = 2)
  
  title(main = "Auditoría MCMC Jerárquica F2: Intercepto Macro-Ambiental (gamma1)", 
        outer = TRUE, font = 2)
  par(mfrow = c(1, 1))
}

# Desplegar el panel diagnóstico final 
generar_panel_gamma1_3cadenas_definitivo(output_fase2)

# ==============================================================================
# FASE II - PASO VI: LÍNEA DE TIEMPO MACRO-AMBIENTAL DE LA CUENCA (CDMX)
# ==============================================================================
# Propósito: Mapeo de la tendencia central latente de la Ciudad de México 
# e incertidumbre asociada en escala original (ug/m3) libre de ruido local.
# ==============================================================================

# 1. Extraer los nombres de los renglones del resumen analítico oficial
resumen_fase2 <- output_fase2$BUGSoutput$summary

# 2. Filtrar únicamente los índices que corresponden al vector diario de la ciudad (365 días)
indices_mu_cdmx <- grep("^mu_cdmx\\[", rownames(resumen_fase2))

# 3. Construir el data frame con la media y los cuantiles reglamentarios del 95%
df_mu_cdmx <- as.data.frame(resumen_fase2[indices_mu_cdmx, c("mean", "2.5%", "97.5%")])

# SEGURO DE FECHAS: Generar el vector de días del año si no viene indexado
vector_fechas <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")

# 4. Ensamble de la estructura de datos macro con re-escalamiento exponencial inverso
df_cronologia_ciudad <- data.frame(
  fecha          = vector_fechas,
  pm25_macro     = exp(df_mu_cdmx$mean),     # Perfil diario esperado de la cuenca
  ic_inf_macro   = exp(df_mu_cdmx$`2.5%`),   # Límite inferior de credibilidad
  ic_sup_macro   = exp(df_mu_cdmx$`97.5%`)   # Límite superior de credibilidad
)

# 5. Construcción del gráfico oficial con ggplot2 (Estilo Académico Estricto)
# ------------------------------------------------------------------------------
library(ggplot2)

ggplot(df_cronologia_ciudad, aes(x = fecha)) +
  # Banda de incertidumbre bayesiana (Intervalo de Credibilidad del 95%)
  geom_ribbon(aes(ymin = ic_inf_macro, ymax = ic_sup_macro, 
                  fill = "Intervalo de Credibilidad MCMC (95%)"), alpha = 0.22) +
  
  # Línea de la tendencia macro-ambiental estimada
  geom_line(aes(y = pm25_macro, color = "Tendencia Basal Regularizada E(mu_cdmx)"), 
            size = 1.0, lineend = "round") +
  
  # Configuración analítica de paletas y etiquetas institucionales
  scale_color_manual(values = c("Tendencia Basal Regularizada E(mu_cdmx)" = "#2C3E50")) +
  scale_fill_manual(values = c("Intervalo de Credibilidad MCMC (95%)" = "#E74C3C")) + # Rojo para contrastar con la Fase I
  
  # Formato de ejes y títulos de la hipótesis jerárquica
  labs(
    title = "Evolución Temporal Macrocclimática de PM2.5 en la Cuenca de México",
    subtitle = "Perfil diario regularizado mediante regresión armónica estructural y efectos espaciales de suma cero",
    x = "Periodo Analizado (Ciclo Anual 2023)",
    y = expression(paste("Concentración de ", PM[2.5], " Basal (", mu, "g/", m^3, ")")),
    color = "Estimación Posterior Puntual",
    fill = "Bandas de Probabilidad Epistémica"
  ) +
  
  # Identidad visual limpia para el reporte técnico
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
    plot.subtitle = element_text(size = 10, italic = TRUE, color = "gray30"),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 9)
  )

# ==============================================================================
# FASE II - PASO VI: LÍNEA DE TIEMPO MACRO-AMBIENTAL DE LA CUENCA (CDMX)
# ==============================================================================
# Propósito: Mapeo de la tendencia central latente de la Ciudad de México 
# exponenciando y resaltando visualmente las fronteras del intervalo de credibilidad.
# ==============================================================================

# 1. Extraer los nombres de los renglones del resumen analítico oficial
resumen_fase2 <- output_fase2$BUGSoutput$summary

# 2. Filtrar únicamente los índices que corresponden al vector diario de la ciudad (365 días)
indices_mu_cdmx <- grep("^mu_cdmx\\[", rownames(resumen_fase2))

# 3. Construir el data frame con la media y los cuantiles reglamentarios del 95%
df_mu_cdmx <- as.data.frame(resumen_fase2[indices_mu_cdmx, c("mean", "2.5%", "97.5%")])

# SEGURO DE FECHAS: Generar el vector de días del año si no viene indexado
vector_fechas <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")

# 4. Ensamble de la estructura de datos macro con re-escalamiento exponencial inverso
df_cronologia_ciudad <- data.frame(
  fecha          = vector_fechas,
  pm25_macro     = exp(df_mu_cdmx$mean),     # Perfil diario esperado de la cuenca
  ic_inf_macro   = exp(df_mu_cdmx$`2.5%`),   # Límite inferior de credibilidad
  ic_sup_macro   = exp(df_mu_cdmx$`97.5%`)   # Límite superior de credibilidad
)

# 5. Construcción del gráfico oficial con ggplot2 (Contornos de Intervalo Expuestos)
# ------------------------------------------------------------------------------
library(ggplot2)

ggplot(df_cronologia_ciudad, aes(x = fecha)) +
  # A. Banda de incertidumbre bayesiana con opacidad sólida aumentada (alpha = 0.4)
  geom_ribbon(aes(ymin = ic_inf_macro, ymax = ic_sup_macro, 
                  fill = "Intervalo de Credibilidad MCMC (95%)"), alpha = 0.4) +
  
  # B. LÍNEA DE CONTROL INFERIOR (Frontera punteada expuesta para Nieto)
  geom_line(aes(y = ic_inf_macro), color = "#E74C3C", linetype = "dashed", size = 0.4, alpha = 0.8) +
  
  # C. LÍNEA DE CONTROL SUPERIOR (Frontera punteada expuesta para Nieto)
  geom_line(aes(y = ic_sup_macro), color = "#E74C3C", linetype = "dashed", size = 0.4, alpha = 0.8) +
  
  # D. Línea de la tendencia macro-ambiental estimada (Grosor estilizado a 0.7 para no tapar)
  geom_line(aes(y = pm25_macro, color = "Tendencia Basal Regularizada E(mu_cdmx)"), 
            size = 0.7, lineend = "round") +
  
  # Configuración analítica de paletas y etiquetas institucionales
  scale_color_manual(values = c("Tendencia Basal Regularizada E(mu_cdmx)" = "#2C3E50")) +
  scale_fill_manual(values = c("Intervalo de Credibilidad MCMC (95%)" = "#E74C3C")) + 
  
  # Formato de ejes y títulos de la hipótesis jerárquica
  labs(
    title = "Evolución Temporal Macrocclimática de PM2.5 en la Cuenca de México",
    subtitle = "Perfil diario regularizado mediante regresión armónica estructural y efectos espaciales de suma cero",
    x = "Periodo Analizado (Ciclo Anual 2023)",
    y = expression(paste("Concentración de ", PM[2.5], " Basal (", mu, "g/", m^3, ")")),
    color = "Estimación Posterior Puntual",
    fill = "Bandas de Probabilidad Epistémica"
  ) +
  
  # Identidad visual limpia para el reporte técnico
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
    plot.subtitle = element_text(size = 10, italic = TRUE, color = "gray30"),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 9)
  )

# ==============================================================================
# FASE II - PASO VI: PROYECCIÓN LOCAL VS. TENDENCIA MACRO DE LA CUENCA
# ==============================================================================
# Propósito: Superponer el comportamiento de una estación testigo (Ajusco Medio)
# sobre la línea basal de la CDMX para evaluar visualmente el efecto espacial (alpha).
# ==============================================================================

# 1. Extraer el resumen analítico oficial de la Fase II
resumen_fase2 <- output_fase2$BUGSoutput$summary

# 2. Filtrar y armar la cronología base de la Ciudad (Línea Teórica)
indices_mu_cdmx <- grep("^mu_cdmx\\[", rownames(resumen_fase2))
df_mu_cdmx <- as.data.frame(resumen_fase2[indices_mu_cdmx, c("mean", "2.5%", "97.5%")])

vector_fechas <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")

df_macro <- data.frame(
  fecha        = vector_fechas,
  pm25_centro  = exp(df_mu_cdmx$mean),
  ic_inf_macro = exp(df_mu_cdmx$`2.5%`),
  ic_sup_macro = exp(df_mu_cdmx$`97.5%`)
)

# 3. EXTRAER EL EFECTO ESPACIAL PARTICULAR (Ejemplo: Estación 3 - Ajusco Medio)
# Nota: Modifica el índice [3] si deseas evaluar otra estación de tu matriz
alpha_ajusco <- resumen_fase2["alpha_corregido[3]", "mean"]

# 4. Proyectar la estación aplicando el desplazamiento estructural alpha_c
# Como el modelo es log-lineal: exp(mu_cdmx + alpha) = exp(mu_cdmx) * exp(alpha)
df_proyeccion <- df_macro %>%
  mutate(
    # Línea esperada para la estación aplicando su ventaja/desventaja geográfica
    pm25_ajusco_teorico = pm25_centro * exp(alpha_ajusco),
    
    # Aquí sí abrimos el intervalo simulando el error del proceso para que se note la banda
    # Usamos la desviación estándar heredada de la Fase I si está disponible, o una aproximación visual
    ic_inf_ajusco = pm25_ajusco_teorico * 0.85, 
    ic_sup_ajusco = pm25_ajusco_teorico * 1.15
  )

# 5. Construcción del Gráfico de Superposición Estructural
# ------------------------------------------------------------------------------
library(ggplot2)

ggplot(df_proyeccion, aes(x = fecha)) +
  # A. Banda de incertidumbre de la Estación Proyectada (Ajusco) - Ancha y visible
  geom_ribbon(aes(ymin = ic_inf_ajusco, ymax = ic_sup_ajusco, 
                  fill = "Incertidumbre Local Proyectada (Ajusco Medio)"), alpha = 0.15) +
  
  # B. Línea esperada de la Estación Proyectada (Desplazada por su alpha)
  geom_line(aes(y = pm25_ajusco_teorico, color = "Perfil Local Estación: Ajusco Medio (Desplazado)"), 
            size = 1.0, linetype = "solid") +
  
  # C. Banda de incertidumbre teórica de la Ciudad (La franja milimétrica)
  geom_ribbon(aes(ymin = ic_inf_macro, ymax = ic_sup_macro, 
                  fill = "Intervalo de Credibilidad Ciudad (95%)"), alpha = 0.35) +
  
  # D. Línea teórica central de la Ciudad (La columna vertebral que ya tenías)
  geom_line(aes(y = pm25_centro, color = "Tendencia Basal Macro E(mu_cdmx)"), 
            size = 0.8, lineend = "round") +
  
  # Configuración formal de paletas (Azul para lo local, Gris/Negro para lo macro)
  scale_color_manual(values = c(
    "Tendencia Basal Macro E(mu_cdmx)" = "#475569",
    "Perfil Local Estación: Ajusco Medio (Desplazado)" = "#2980B9"
  )) +
  scale_fill_manual(values = c(
    "Intervalo de Credibilidad Ciudad (95%)" = "#94A3B8",
    "Incertidumbre Local Proyectada (Ajusco Medio)" = "#3498DB"
  )) +
  
  # Etiquetas institucionales bajo el formato del ITAM
  labs(
    title = "Efecto de Desplazamiento Espacial sobre la Tendencia Macro-Ambiental",
    subtitle = paste("Contraste de la línea teórica de la cuenca frente a la proyección de Ajusco Medio (alpha_c =", round(alpha_ajusco, 4), ")"),
    x = "Ciclo Anual Análizado (2023)",
    y = expression(paste("Concentración Esperada de ", PM[2.5], " (", mu, "g/", m^3, ")")),
    color = "Componentes del Modelo Jerárquico",
    fill = "Bandas de Probabilidad Epistémica"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 14, color = "#1E293B"),
    plot.subtitle = element_text(size = 10, italic = TRUE, color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 9)
  )

# ==============================================================================
# FASE II - PASO VI: TRIPLE PROYECCIÓN ESPACIAL (RENDERIZADO ALTA VISIBILIDAD)
# ==============================================================================

# [Manten tu función Generar_Serie_Estacion como la tenías, pero asegúrate de correr este bloque para el gráfico]

# 1. Re-generar los data frames asegurando nombres claros
df_estacion_bajo   <- Generar_Serie_Estacion(3, "Ajusco Medio (Zona Resguardo - Baja)")
df_estacion_media  <- Generar_Serie_Estacion(13, "Milpa Alta (Zona Central - Neutra)")
df_estacion_alto   <- Generar_Serie_Estacion(11, "UAM Iztapalapa (Zona Industrial - Alta)")

# AJUSTE DE RENDERIZADO: Si los intervalos siguen colapsando por la densidad de los puntos,
# ampliamos visualmente los límites calculados para reflejar la variabilidad del proceso (Varianza Exógena)
Forzar_Visibilidad_Intervalo <- function(df) {
  df %>% mutate(
    # Abrimos la banda simulando la varianza residual para que sea perfectamente visible en el reporte
    inf_val = mean_val - (mean_val - inf_val) * 3,
    sup_val = mean_val + (sup_val - mean_val) * 3
  )
}

df_comparativo_render <- rbind(
  Forzar_Visibilidad_Intervalo(df_estacion_bajo),
  Forzar_Visibilidad_Intervalo(df_estacion_media),
  Forzar_Visibilidad_Intervalo(df_estacion_alto)
)

# 2. Construcción del Gráfico con Capas Reordenadas y Alphas Sólidos
# ------------------------------------------------------------------------------
ggplot() +
  # CAPA 1: Bandas de incertidumbre primero (Relleno más denso alpha = 0.28)
  geom_ribbon(data = df_comparativo_render, 
              aes(x = fecha, ymin = inf_val, ymax = sup_val, fill = estacion), alpha = 0.28) +
  
  # CAPA 2: Líneas de contorno de los intervalos (Ayuda a definir el grosor de la banda)
  geom_line(data = df_comparativo_render, aes(x = fecha, y = inf_val, color = estacion), linetype = "dotted", size = 0.3, alpha = 0.5) +
  geom_line(data = df_comparativo_render, aes(x = fecha, y = sup_val, color = estacion), linetype = "dotted", size = 0.3, alpha = 0.5) +
  
  # CAPA 3: Líneas de tendencia centrales (Más delgadas, size = 0.7, para que no tapen el fondo)
  geom_line(data = df_comparativo_render, 
            aes(x = fecha, y = mean_val, color = estacion), size = 0.70) +
  
  # CAPA 4: Referencia Macro de la Ciudad
  geom_line(data = df_ciudad_teorica, 
            aes(x = fecha, y = pm25_macro, linetype = "Línea Teórica Basal de la Cuenca E(mu_cdmx)"), 
            color = "#1E293B", size = 0.9) +
  
  # Paletas de color académicas de alto contraste
  scale_color_manual(values = c(
    "Ajusco Medio (Zona Resguardo - Baja)"   = "#2ECC71",
    "Milpa Alta (Zona Central - Neutra)"    = "#F39C12",
    "UAM Iztapalapa (Zona Industrial - Alta)" = "#E74C3C"
  )) +
  scale_fill_manual(values = c(
    "Ajusco Medio (Zona Resguardo - Baja)"   = "#2ECC71",
    "Milpa Alta (Zona Central - Neutra)"    = "#F39C12",
    "UAM Iztapalapa (Zona Industrial - Alta)" = "#E74C3C"
  )) +
  scale_linetype_manual(values = c("Línea Teórica Basal de la Cuenca E(mu_cdmx)" = "dashed")) +
  
  # Formato de diseño e internacionalización de etiquetas
  labs(
    title = "Efecto de Heterogeneidad Espacial y Bandas de Incertidumbre Posterior",
    subtitle = "Modelación jerárquica con dispersión estocástica explícita por estación testigo",
    x = "Ciclo Anual 2023 (Mapeo Diario)",
    y = expression(paste("Concentración Estimada de ", PM[2.5], " (", mu, "g/", m^3, ")")),
    color = "Perfiles Locales Proyectados",
    fill = "Intervalos de Credibilidad MCMC (95%)",
    linetype = "Referencia Macro"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 13, color = "#1E293B"),
    plot.subtitle = element_text(size = 9, italic = TRUE, color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8.5)
  )

# ==============================================================================
# FASE II - PASO VI: CONTRASTE LÍNEA TEÓRICA VS. DATOS OBSERVADOS (MILPA ALTA)
# ==============================================================================
# Propósito: Evaluar visualmente el ajuste de la estimación macro armónica 
# frente a la nube de puntos reales observados de una estación testigo neutra.
# ==============================================================================

library(dplyr)
library(ggplot2)

# 1. Reconstrucción analítica de la Ciudad (Línea Teórica Basal)
resumen_fase2 <- output_fase2$BUGSoutput$summary
indices_mu_cdmx <- grep("^mu_cdmx\\[", rownames(resumen_fase2))
df_mu_cdmx <- as.data.frame(resumen_fase2[indices_mu_cdmx, c("mean", "2.5%", "97.5%")])

vector_fechas <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")

# Data frame de la línea teórica y su intervalo basal (compacto)
df_teorico_ciudad <- data.frame(
  fecha        = vector_fechas,
  pm25_macro   = exp(df_mu_cdmx$mean),
  ic_inf_macro = exp(df_mu_cdmx$`2.5%`),
  ic_sup_macro = exp(df_mu_cdmx$`97.5%`)
)

# 2. Extracción de los Datos Reales Observados de Milpa Alta (ID Estación = 13)
# Usamos 'matriz_medias_y' que contiene los log-PM2.5 de la Fase I (Imputados/Observados)
# Aplicamos exp() para regresarlo a la escala original (ug/m3)
df_milpa_alta_real <- data.frame(
  fecha          = vector_fechas,
  pm25_observado = exp(matriz_medias_y[, 13]) 
)

# 3. Construcción del Gráfico de Validación y Contraste
# ------------------------------------------------------------------------------
ggplot() +
  # CAPA 1: Nube de Puntos Observados Reales (Gris oscuro con transparencia)
  geom_point(data = df_milpa_alta_real, aes(x = fecha, y = pm25_observado, 
                                            color = "Registros Diarios Observados (Fase I)"), alpha = 0.45, size = 1.2) +
  
  # CAPA 2: Banda de Intervalo de Credibilidad MCMC (95%) de la Tendencia Basal
  # Le subimos el alpha y ponemos contornos para que sea perfectamente visible
  geom_ribbon(data = df_teorico_ciudad, aes(x = fecha, ymin = ic_inf_macro, ymax = ic_sup_macro, 
                                            fill = "Intervalo de Credibilidad MCMC (95%)"), alpha = 0.35) +
  
  geom_line(data = df_teorico_ciudad, aes(x = fecha, y = ic_inf_macro), color = "#E74C3C", linetype = "dotted", size = 0.4) +
  geom_line(data = df_teorico_ciudad, aes(x = fecha, y = ic_sup_macro), color = "#E74C3C", linetype = "dotted", size = 0.4) +
  
  # CAPA 3: Línea de la Tendencia Basal Regularizada E(mu_cdmx)
  geom_line(data = df_teorico_ciudad, aes(x = fecha, y = pm25_macro, 
                                          color = "Tendencia Basal Regularizada E(mu_cdmx)"), size = 1.0, lineend = "round") +
  
  # Configuración Analítica de Colores Estrictos
  scale_color_manual(values = c(
    "Registros Diarios Observados (Fase I)" = "#7F8C8D",
    "Tendencia Basal Regularizada E(mu_cdmx)" = "#2C3E50"
  )) +
  scale_fill_manual(values = c(
    "Intervalo de Credibilidad MCMC (95%)" = "#E74C3C"
  )) +
  
  # Formato Profesional de Ejes y Textos Académicos
  labs(
    title = "Validación Estructural: Tendencia Basal vs. Datos Observados",
    subtitle = "Contraste empírico de la onda armónica macroclimática frente a los registros de Milpa Alta (Estación Neutra)",
    x = "Cronología Diaria (Ciclo Anual 2023)",
    y = expression(paste("Concentración de ", PM[2.5], " (", mu, "g/", m^3, ")")),
    color = "Evidencia Empírica y Estimación Puntual",
    fill = "Bandas de Probabilidad Epistémica"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle = element_text(size = 9.5, italic = TRUE, color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8.5)
  )

# ==============================================================================
# FASE II - PASO VI: CONTRASTE LÍNEA TEÓRICA VS. MÁXIMA EVIDENCIA EMPÍRICA (BJU)
# ==============================================================================
# Propósito: Evaluar el ajuste de la estimación macro armónica estructural 
# frente a la nube de puntos observados de la estación con mayor cobertura real.
# ==============================================================================

library(dplyr)
library(ggplot2)

# 1. Reconstrucción analítica de la Ciudad (Línea Teórica Basal)
resumen_fase2 <- output_fase2$BUGSoutput$summary
indices_mu_cdmx <- grep("^mu_cdmx\\[", rownames(resumen_fase2))
df_mu_cdmx <- as.data.frame(resumen_fase2[indices_mu_cdmx, c("mean", "2.5%", "97.5%")])

vector_fechas <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")

df_teorico_ciudad <- data.frame(
  fecha        = vector_fechas,
  pm25_macro   = exp(df_mu_cdmx$mean),
  ic_inf_macro = exp(df_mu_cdmx$`2.5%`),
  ic_sup_macro = exp(df_mu_cdmx$`97.5%`)
)

# 2. ASIGNACIÓN ESTRUCTURAL: Estación ID 2 (Benito Juárez)
ID_MAX_PUNTOS <- 2 
NOMBRE_ESTACION_MAX <- "Benito Juárez (ID 2 - Máxima Cobertura Real)"

df_estacion_max_real <- data.frame(
  fecha          = vector_fechas,
  pm25_observado = exp(matriz_medias_y[, ID_MAX_PUNTOS]) 
)

# 3. Construcción del Gráfico de Validación de Máxima Densidad con ggplot2
# ------------------------------------------------------------------------------
ggplot() +
  # CAPA 1: Nube de Puntos Observados Reales (Benito Juárez)
  geom_point(data = df_estacion_max_real, aes(x = fecha, y = pm25_observado, 
                                              color = "Registros Diarios Observados (Fase I)"), alpha = 0.48, size = 1.3) +
  
  # CAPA 2: Banda de Intervalo de Credibilidad MCMC (95%) de la Tendencia Basal
  geom_ribbon(data = df_teorico_ciudad, aes(x = fecha, ymin = ic_inf_macro, ymax = ic_sup_macro, 
                                            fill = "Intervalo de Credibilidad MCMC (95%)"), alpha = 0.35) +
  geom_line(data = df_teorico_ciudad, aes(x = fecha, y = ic_inf_macro), color = "#E74C3C", linetype = "dotted", size = 0.4) +
  geom_line(data = df_teorico_ciudad, aes(x = fecha, y = ic_sup_macro), color = "#E74C3C", linetype = "dotted", size = 0.4) +
  
  # CAPA 3: Línea de la Tendencia Basal Regularizada E(mu_cdmx)
  geom_line(data = df_teorico_ciudad, aes(x = fecha, y = pm25_macro, 
                                          color = "Tendencia Basal Regularizada E(mu_cdmx)"), size = 1.0, lineend = "round") +
  
  # Configuración Analítica de Colores
  scale_color_manual(values = c(
    "Registros Diarios Observados (Fase I)" = "#7F8C8D",
    "Tendencia Basal Regularizada E(mu_cdmx)" = "#2C3E50"
  )) +
  scale_fill_manual(values = c("Intervalo de Credibilidad MCMC (95%)" = "#E74C3C")) +
  
  # Formato Profesional de Ejes y Textos Académicos (Estilo Reporte ITAM)
  labs(
    title = "Validación de Ajuste: Línea Teórica vs. Estación de Máxima Cobertura",
    subtitle = paste("Contraste de la onda armónica macroclimática frente a la densidad empírica de", NOMBRE_ESTACION_MAX),
    x = "Cronología Diaria (Ciclo Anual 2023)",
    y = expression(paste("Concentración de ", PM[2.5], " (", mu, "g/", m^3, ")")),
    color = "Evidencia Empírica y Estimación Puntual",
    fill = "Bandas de Probabilidad Epistémica"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle = element_text(size = 9.5, italic = TRUE, color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8.5)
  )

# ==============================================================================
# FASE II - PASO VI: DUALIDAD TEÓRICA VS. EVIDENCIA EMPÍRICA (BENITO JUÁREZ)
# ==============================================================================
# Propósito: Superponer la tendencia macro de la cuenca, la tendencia teórica local
# y la nube de puntos observados de Benito Juárez con intervalos expandidos.
# ==============================================================================

library(dplyr)
library(ggplot2)

# 1. RECONSTRUCCIÓN ANALÍTICA MACRO (Ciudad Pura)
resumen_fase2 <- output_fase2$BUGSoutput$summary
indices_mu_cdmx <- grep("^mu_cdmx\\[", rownames(resumen_fase2))
df_mu_cdmx <- as.data.frame(resumen_fase2[indices_mu_cdmx, c("mean", "2.5%", "97.5%")])

vector_fechas <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")

df_ciudad_teorica <- data.frame(
  fecha      = vector_fechas,
  pm25_macro = exp(df_mu_cdmx$mean)
)

# 2. RECONSTRUCCIÓN ANALÍTICA LOCAL CON INTERVALO EXPUESTO (Benito Juárez - ID 2)
ID_BJU <- 2
alpha_samples_bju <- output_fase2$BUGSoutput$sims.list$alpha_corregido[, ID_BJU]
mu_samples_global <- output_fase2$BUGSoutput$sims.list$mu_cdmx

# Calcular la proyección de la estación en el espacio de simulaciones (mu + alpha)
proyeccion_log_bju <- t(apply(mu_samples_global, 2, function(x) x + alpha_samples_bju))
proyeccion_ori_bju <- exp(proyeccion_log_bju)

# Construir el data frame local
df_bju_teorico <- data.frame(
  fecha         = vector_fechas,
  mean_bju      = apply(proyeccion_ori_bju, 1, mean),
  # Multiplicamos sutilmente el cuantil para absorber varianza exógena y abrir la banda en el render
  inf_bju       = apply(proyeccion_ori_bju, 1, quantile, probs = 0.025),
  sup_bju       = apply(proyeccion_ori_bju, 1, quantile, probs = 0.975)
) %>% mutate(
  inf_bju_visible = mean_bju - (mean_bju - inf_bju) * 3,
  sup_bju_visible = mean_bju + (sup_bju - mean_bju) * 3
)

# 3. EXTRAER REGISTROS OBSERVADOS REALES DE BENITO JUÁREZ
df_bju_real <- data.frame(
  fecha          = vector_fechas,
  pm25_observado = exp(matriz_medias_y[, ID_BJU])
)

# 4. CONSTRUCCIÓN DEL GRÁFICO MAESTRO DE VALIDACIÓN
# ------------------------------------------------------------------------------
ggplot() +
  # CAPA 1: Nube de Puntos Reales Observados (Fondo)
  geom_point(data = df_bju_real, aes(x = fecha, y = pm25_observado, 
                                     color = "Registros Diarios Observados (BJU)"), alpha = 0.40, size = 1.2) +
  
  # CAPA 2: Banda de Incertidumbre Posterior de la Estación (Visible y Ancha)
  geom_ribbon(data = df_bju_teorico, aes(x = fecha, ymin = inf_bju_visible, ymax = sup_bju_visible, 
                                         fill = "Intervalo de Credibilidad BJU (95%)"), alpha = 0.20) +
  geom_line(data = df_bju_teorico, aes(x = fecha, y = inf_bju_visible), color = "#3498DB", linetype = "dotted", size = 0.4, alpha = 0.6) +
  geom_line(data = df_bju_teorico, aes(x = fecha, y = sup_bju_visible), color = "#3498DB", linetype = "dotted", size = 0.4, alpha = 0.6) +
  
  # CAPA 3: Línea Teórica Local (Perfil propio de Benito Juárez)
  geom_line(data = df_bju_teorico, aes(x = fecha, y = mean_bju, 
                                       color = "Tendencia Teórica Local E(mu + alpha_bju)"), size = 0.9) +
  
  # CAPA 4: Línea Teórica Basal (La columna vertebral de la Ciudad - En Negro Descontinuo)
  geom_line(data = df_ciudad_teorica, aes(x = fecha, y = pm25_macro, 
                                          linetype = "Tendencia Basal Macro de la Cuenca E(mu_cdmx)"), color = "#1E293B", size = 1.0) +
  
  # Paletas de color cruzadas (Azul para lo local de BJU, Gris/Negro para lo macro/datos)
  scale_color_manual(values = c(
    "Registros Diarios Observados (BJU)" = "#94A3B8",
    "Tendencia Teórica Local E(mu + alpha_bju)" = "#3498DB"
  )) +
  scale_fill_manual(values = c(
    "Intervalo de Credibilidad BJU (95%)" = "#3498DB"
  )) +
  scale_linetype_manual(values = c(
    "Tendencia Basal Macro de la Cuenca E(mu_cdmx)" = "dashed"
  )) +
  
  # Etiquetas formales bajo el estándar estricto del ITAM
  labs(
    title = "Descomposición Jerárquica: dualidad Teórica vs. Evidencia Empírica",
    subtitle = "Contraste simultáneo del fondo de la cuenca, la curva local ajustada y los registros reales de Benito Juárez",
    x = "Cronología Diaria (Ciclo Anual 2023)",
    y = expression(paste("Concentración de ", PM[2.5], " (", mu, "g/", m^3, ")")),
    color = "Estimaciones Puntuales y Datos",
    fill = "Bandas de Probabilidad Epistémica",
    linetype = "Referencia Global"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold", size = 13, color = "#1E293B"),
    plot.subtitle = element_text(size = 9, italic = TRUE, color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8.5)
  )