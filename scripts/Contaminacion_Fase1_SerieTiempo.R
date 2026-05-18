# ==============================================================================
# PROYECTO: MODELACIÓN ESPACIO-TEMPORAL BAYESIANA (PM2.5 EN LA ZMVM)
# FASE 1: PIPELINE COMPLETO DE PREPROCESAMIENTO, SIMULACIÓN Y VALIDACIÓN POSTERIOR
# ==============================================================================

# ==============================================================================
# ENTORNO GLOBAL: DECLARACIÓN DE LIBRERÍAS DE CONTROL Y MODELACIÓN
# ==============================================================================
library(dplyr)      # Manipulación estructural de datos (Tidy Data)
library(sf)         # Procesamiento y proyección de geometrías vectoriales (GIS)
library(tidyr)      # Pivoteo y completitud matricial del panel
library(R2jags)     # Interfaz paralela para simulación MCMC en JAGS
library(boot)       # Soporte estadístico y funciones de remuestreo
library(coda)       # Diagnósticos de convergencia y salida de cadenas MCMC
library(R2WinBUGS)  # Formateo compatible de variables para Gibbs Sampling
library(ggplot2)    # Visualización analítica de alta definición

# ==============================================================================
# PASO 1: VERIFICACIÓN DE ENTORNO Y CARGA DE DATOS SINAICA
# ==============================================================================
# Propósito: Importación y tipado correcto de la serie de tiempo original.

ruta_archivo <- "/Users/monserratochoaparra/Downloads/pm25_valle_mexico_v2.csv"

df_original <- read.csv(ruta_archivo)
df_original$date <- as.Date(df_original$date)

# ==============================================================================
# PASO 2: GEOPROCESAMIENTO Y MATRIZ DE DISTANCIAS EUCLIDIANAS EN KM (UTM 14N)
# ==============================================================================
# Propósito: Proyectar coordenadas esféricas (WGS84) a un plano métrico local 
# para calcular distancias lineales reales no distorsionadas por la curvatura terrestre.
# Esto alimentará la matriz de decaimiento espacial (D) en el proceso latente Gibbs.

nodos_geo <- df_original %>% 
  select(estacion, lat, lon) %>% 
  distinct() %>% 
  arrange(estacion)

# Transformación al sistema proyectado local UTM Zona 14N (EPSG: 32614)
estaciones_utm <- st_as_sf(nodos_geo, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 32614)

# Cálculo matricial de distancias inter-nodales (conversión de metros a kilómetros)
D_km <- as.matrix(st_distance(estaciones_utm)) / 1000

# ==============================================================================
# PASO 3: RECONSTRUCCIÓN DEL PANEL Y ALINEACIÓN DE COVARIABLES EN R
# ==============================================================================
# Propósito: Solucionar el sesgo de selección forzando un panel balanceado de 
# T = 365 días y J = 14 estaciones (N = 5,110 observaciones). Las covariables climáticas
# se imputan mediante medias históricas locales condicionales para blindar el predictor
# lineal en JAGS, aislando los NAs únicamente en la variable respuesta (log_pm25).

dias_ano   <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
estaciones <- nodos_geo$estacion

grid_completo <- expand.grid(date = dias_ano, estacion = estaciones) %>%
  left_join(df_original, by = c("date", "estacion")) %>%
  group_by(estacion) %>%
  mutate(
    temp = ifelse(is.na(temp), mean(temp, na.rm = TRUE), temp),
    hr   = ifelse(is.na(hr), mean(hr, na.rm = TRUE), hr)
  ) %>%
  ungroup() %>%
  arrange(estacion, date)

# Construcción de estructuras matriciales para el motor de simulación de JAGS
matriz_y    <- matrix(log(grid_completo$pm25), nrow = 365)
matriz_temp <- matrix(grid_completo$temp, nrow = 365)
matriz_hr   <- matrix(grid_completo$hr, nrow = 365)

# Parametrización del componente temporal macro mediante base sinusoidal (Fourier)
dia_juliano <- as.numeric(format(dias_ano, "%j"))
vector_sen  <- sin(2 * pi * dia_juliano / 365)
vector_cos  <- cos(2 * pi * dia_juliano / 365)

# Estructuración de la lista oficial de datos
jags_data <- list(
  log_pm25 = matriz_y, temp = matriz_temp, hr = matriz_hr,
  sen_t = vector_sen, cos_t = vector_cos,
  N_dias = 365, N_estaciones = 14, D = D_km
)

print(paste("Estructura Balanceada. Datos faltantes (NAs) preservados para imputación MCMC:", sum(is.na(matriz_y))))

# ==============================================================================
# PASO 4: CALIBRACIÓN ALGORÍTMICA Y EJECUCIÓN DEL MUESTREADOR DE GIBBS
# ==============================================================================
# Propósito: Ejecución de simulación paralela MCMC. Se configuran 10k iteraciones
# con un burn-in robusto del 30% (3k descartadas) para disipar efectos del estado inicial
# y un n.thin de 4 para mitigar la autocorrelación secuencial intra-cadena.

jags_inits <- function() {
  list(beta = rnorm(4, 0, 0.1), alphaj = rep(0, 14), 
       tau_y = 1, tau_alpha = 1, phi = 0.5)
}

jags_parameters <- c("beta", "alphaj", "tau_y", "tau_alpha", "phi", "log_pm25")
ruta_modelo_txt <- "/Users/monserratochoaparra/Downloads/jags_modelo_espacio_temporal.txt"

output_jags <- jags.parallel(
  data               = jags_data, 
  inits              = jags_inits, 
  parameters.to.save = jags_parameters,
  model.file         = ruta_modelo_txt, 
  n.chains           = 2, 
  n.iter             = 10000, 
  n.burnin           = 3000, 
  n.thin             = 4
)

# Impresión de métricas de control y convergencia univariada para beta1
print(output_jags$BUGSoutput$summary["beta[1]", c("mean", "sd", "2.5%", "97.5%", "Rhat", "n.eff")])

# ==============================================================================
# PASO 5: PANEL DIAGNÓSTICO DE ERGODICIDAD Y MIXING (FORMATO REGLAMENTARIO 3x2)
# ==============================================================================
# Propósito: Auditoría visual univariada para evaluar si el estimador de la tendencia
# alcanzó su distribución estacionaria y rompió la memoria de la cadena.

generar_panel_beta1_3x2 <- function(output_jags) {
  beta_c1 <- output_jags$BUGSoutput$sims.array[, 1, "beta[1]"]
  beta_c2 <- output_jags$BUGSoutput$sims.array[, 2, "beta[1]"]
  N_iter  <- length(beta_c1)
  
  par(mfrow = c(3, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))
  
  # 1. Traza (beta1) - Coherencia cromática: C1 Azul, C2 Rojo
  plot(1:N_iter, beta_c1, type = "l", col = "#3498DB", xlab = "Iteración", ylab = "Valor", main = "1. Traza (beta1)")
  lines(1:N_iter, beta_c2, col = "#E74C3C")
  
  # 2. Media Móvil Acumulada (Running Mean) - Estabilidad del primer momento
  mean_b1 <- cumsum(beta_c1) / (1:N_iter)
  mean_b2 <- cumsum(beta_c2) / (1:N_iter)
  plot(1:N_iter, mean_b1, type = "l", col = "#3498DB", ylim = range(c(mean_b1, mean_b2)), xlab = "Iteración", ylab = "Media", main = "2. Media Móvil (beta1)")
  lines(1:N_iter, mean_b2, col = "#E74C3C")
  
  # 3. Histograma Marginal Cadena 1 (Densidad empírica posterior)
  hist(beta_c1, breaks = 25, col = "#3498DB", border = "white", xlab = "Valor", ylab = "Frecuencia", main = "3. Histograma C1")
  
  # 4. Histograma Marginal Cadena 2 (Traslape simétrico estructural)
  hist(beta_c2, breaks = 25, col = "#E74C3C", border = "white", xlab = "Valor", ylab = "Frecuencia", main = "4. Histograma C2")
  
  # 5. Función de Autocorrelación (ACF) Cadena 1 - Monitoreo de memoria independiente (Negro)
  acf_b1 <- acf(beta_c1, plot = FALSE, lag.max = 30)
  plot(acf_b1$lag, acf_b1$acf, type = "h", col = "#000000", lwd = 2, xlab = "Lag", ylab = "ACF", main = "5. Autocorrelación C1")
  abline(h = c(-1.96/sqrt(N_iter), 1.96/sqrt(N_iter)), col = "gray", lty = 2)
  
  # 6. Función de Autocorrelación (ACF) Cadena 2 - Monitoreo de memoria independiente (Negro)
  acf_b2 <- acf(beta_c2, plot = FALSE, lag.max = 30)
  plot(acf_b2$lag, acf_b2$acf, type = "h", col = "#000000", lwd = 2, xlab = "Lag", ylab = "ACF", main = "6. Autocorrelación C2")
  abline(h = c(-1.96/sqrt(N_iter), 1.96/sqrt(N_iter)), col = "gray", lty = 2)
  
  title(main = "Diagnóstico MCMC Estructural: Coeficiente Armónico beta1", outer = TRUE, font = 2)
  par(mfrow = c(1, 1))
}

# Desplegar diagnóstico univariado
generar_panel_beta1_3x2(output_jags)

# ==============================================================================
# PASO 6: INFERENCIA POSTERIOR CON INTERVALOS DE CREDIBILIDAD (CASO TESTIGO AJUSCO)
# ==============================================================================
# Propósito: Mapeo de la distribución posterior e incertidumbre asociada en escala original.

resumen_jags <- output_jags$BUGSoutput$summary
indices_log_pm25 <- grep("^log_pm25", rownames(resumen_jags))
df_log_pm25 <- as.data.frame(resumen_jags[indices_log_pm25, c("mean", "2.5%", "97.5%")])

# Reconstrucción con re-escalamiento no lineal inverso (Exponencial)
df_post_analisis <- grid_completo %>%
  mutate(
    pm25_estimado = exp(df_log_pm25$mean),
    ic_inferior   = exp(df_log_pm25$`2.5%`),
    ic_superior   = exp(df_log_pm25$`97.5%`),
    id_estacion   = as.numeric(as.factor(estacion))
  )

df_ajusco <- df_post_analisis %>% filter(estacion == "Ajusco Medio")

ggplot(df_ajusco, aes(x = date)) +
  geom_ribbon(aes(ymin = ic_inferior, ymax = ic_superior, fill = "Incertidumbre MCMC (95%)"), alpha = 0.25) +
  geom_line(aes(y = pm25_estimado, color = "Imputación MCMC (Media)"), size = 0.8) +
  geom_point(aes(y = pm25, color = "Observados (Con NA)"), alpha = 0.5, size = 1.3) +
  scale_color_manual(values = c("Observados (Con NA)" = "gray30", "Imputación MCMC (Media)" = "#2C3E50")) +
  scale_fill_manual(values = c("Incertidumbre MCMC (95%)" = "#3498DB")) +
  labs(
    title = "Inferencia Posterior e Incertidumbre Estocástica: Ajusco Medio",
    subtitle = "Análisis de la varianza condicional bayesiana ante la ausencia temporal de registros locales",
    x = "Fecha (2023)", y = "Concentración PM2.5 (ug/m3)", color = "Estimación Puntual", fill = "Bandas de Probabilidad"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"), legend.box = "horizontal")

# ==============================================================================
# PASO 6 (EXTENSIÓN): VALIDACIÓN CRUZADA DE INCERTIDUMBRE (3 REGÍMENES ESCENARIO)
# ==============================================================================
# Propósito: Extraer de manera indexada tridimensional desde sims.list para evitar 
# errores de ordenamiento alfabético y demostrar la sensibilidad del modelo ante 
# tres densidades distintas de pérdida de información observada.

sims_log_pm25 <- output_jags$BUGSoutput$sims.list$log_pm25

# Estimaciones directas colapsando la dimensión estocástica de simulaciones MCMC
pm25_mean <- apply(sims_log_pm25, c(2, 3), mean)
pm25_inf  <- apply(sims_log_pm25, c(2, 3), quantile, probs = 0.025)
pm25_sup  <- apply(sims_log_pm25, c(2, 3), quantile, probs = 0.975)

df_post_analisis_v2 <- grid_completo %>%
  mutate(
    pm25_estimado = exp(as.vector(pm25_mean)),
    ic_inferior   = exp(as.vector(pm25_inf)),
    ic_superior   = exp(as.vector(pm25_sup))
  )

estaciones_objetivo <- c("Hospital General de México", "Santiago Acahualtepec", "Benito Juárez")

df_comparativa <- df_post_analisis_v2 %>%
  filter(estacion %in% estaciones_objetivo) %>%
  mutate(
    estacion_regimen = factor(
      estacion, 
      levels = estaciones_objetivo,
      labels = c("1. Escenario Crítico: Hospital General de México (52 obs)", 
                 "2. Escenario Típico: Santiago Acahualtepec (189 obs)", 
                 "3. Escenario Óptimo: Benito Juárez (236 obs)")
    )
  )

ggplot(df_comparativa, aes(x = date)) +
  geom_ribbon(aes(ymin = ic_inferior, ymax = ic_superior, fill = "Incertidumbre MCMC (95%)"), alpha = 0.25) +
  geom_line(aes(y = pm25_estimado, color = "Imputación MCMC (Media)"), size = 0.7) +
  geom_point(aes(y = pm25, color = "Observados (Con NA)"), alpha = 0.4, size = 1.0) +
  scale_color_manual(values = c("Observados (Con NA)" = "gray30", "Imputación MCMC (Media)" = "#2C3E50")) +
  scale_fill_manual(values = c("Incertidumbre MCMC (95%)" = "#3498DB")) +
  facet_wrap(~estacion_regimen, ncol = 1, scales = "free_y") +
  labs(
    title = "Validación Cruzada de la Incertidumbre Posterior por Densidad de Datos",
    subtitle = "Evidencia empírica del comportamiento de la varianza bayesiana frente al volumen de información local",
    x = "Fecha (2023)", y = "Concentración PM2.5 (ug/m3)", color = "Estimación Puntual", fill = "Bandas de Probabilidad"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold", size = 10, color = "white"),
    strip.background = element_rect(fill = "#2C3E50", color = NA),
    panel.spacing = unit(1.2, "lines")
  )