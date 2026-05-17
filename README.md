# Proyecto Final — Regresión Avanzada
## Análisis Bayesiano Espacial de PM2.5 en el Valle de México

**Autoras:** Paulina Garza Allende · Andrea Monserrat Arredondo Rodríguez  
**Curso:** Regresión Avanzada — ITAM, Prof. Luis E. Nieto Barajas  
**Fuente de datos:** SINAICA — Instituto Nacional de Ecología y Cambio Climático  
**Software:** R 4.5.2 + JAGS 4.3.1 (R2jags), terra, dplyr  
**Shapefile:** GADM nivel 2 (municipios/alcaldías)

---

## Pregunta central

> **¿Qué tan importante es la estructura espacial para explicar la variación de PM2.5 en el Valle de México, y podemos usarla para predecir la contaminación en zonas sin sensores?**

1. ¿Qué factores (clima, estacionalidad) explican la variación temporal de PM2.5?
2. ¿Existen diferencias sistemáticas entre alcaldías/municipios no explicadas por clima?
3. ¿Es mejor modelar estas diferencias como efectos fijos o jerárquicos?
4. ¿Podemos interpolar espacialmente para predecir en los 141 polígonos del Valle de México?

---

## Datos

**Dataset v2:** 14 estaciones, 2,617 observaciones diarias (enero–diciembre 2023)

| # | Estación | Municipio | PM2.5 (µg/m³) | n | Entidad |
|---|----------|-----------|---------------|---|---------|
| 1 | Santiago Acahualtepec | Iztapalapa | 23.5 | 189 | CDMX |
| 2 | Hospital General de México | Cuauhtémoc | 22.7 | 52 | CDMX |
| 3 | Gustavo A. Madero | Gustavo A. Madero | 22.7 | 154 | CDMX |
| 4 | Tlalnepantla | Tlalnepantla de Baz | 21.8 | 174 | Edomex |
| 5 | San Agustín | Ecatepec | 20.7 | 84 | Edomex |
| 6 | UAM Iztapalapa | Iztapalapa | 20.6 | 207 | CDMX |
| 7 | Merced | Venustiano Carranza | 20.6 | 235 | CDMX |
| 8 | UAM Xochimilco | Coyoacán | 20.5 | 233 | CDMX |
| 9 | Nezahualcóyotl | Nezahualcóyotl | 20.4 | 218 | Edomex |
| 10 | Benito Juárez | Benito Juárez | 19.8 | 236 | CDMX |
| 11 | FES Aragón | Nezahualcóyotl | 18.0 | 233 | Edomex |
| 12 | Pedregal | Álvaro Obregón | 16.2 | 181 | CDMX |
| 13 | Ajusco Medio | Tlalpan | 16.0 | 188 | CDMX |
| 14 | Biblioteca | Tizayuca | 15.4 | 233 | Hidalgo |

De 56 estaciones SINAICA dentro del Valle de México, solo 14 (25%) reportaron PM2.5 + temperatura + HR de forma simultánea durante 2023.

---

## Variables del modelo

| Variable | Descripción | Rol |
|----------|-------------|-----|
| `pm25` | Concentración diaria promedio (µg/m³) | Respuesta |
| `temp` | Temperatura diaria promedio (°C), estandarizada | Covariable meteorológica |
| `hr` | Humedad relativa diaria promedio (%), estandarizada | Covariable meteorológica |
| `sen_t` | sin(2π·día/365) | Covariable estacional (Fourier) |
| `cos_t` | cos(2π·día/365) | Covariable estacional (Fourier) |
| `lat`, `lon` | Coordenadas geográficas, estandarizadas | Predictor espacial (Modelo D) |
| `estacion` | Estación de monitoreo | Agrupador (Modelos B, C1, E) |

---

## Cinco modelos

### Modelo A — Normal global
```
log(PM2.5_i) ~ N(μ_i, τ)
μ_i = α + β₁·temp + β₂·hr + β₃·sen_t + β₄·cos_t
```
Línea base. Ignora la ubicación geográfica.  
**DIC = 2942.6 | R² = 0.313 | σ = 0.424**

### Modelo B — Efectos fijos por estación
```
μ_i = α̃ + α̃ⱼ + β₁·temp + β₂·hr + β₃·sen_t + β₄·cos_t
```
Restricción suma-cero post-muestreo: α̃ = α + mean(αⱼ), α̃ⱼ = αⱼ − mean(αⱼ).  
**DIC = 2724.2 | R² = 0.375 | σ = 0.406**

### Modelo C1 — Jerárquico Normal *(modelo preferido)*
```
μ_i = α + αⱼ + β₁·temp + β₂·hr + β₃·sen_t + β₄·cos_t
αⱼ ~ N(0, τ_α)
```
**DIC = 2723.7 | R² = 0.375 | σ = 0.406 | σ_α = 0.134**

### Modelo C2 — Gamma global
```
PM2.5_i ~ Gamma(a, a/μ_i)
log(μ_i) = α + β₁·tempc + β₂·hrc + β₃·sen_t + β₄·cos_t
```
Covariables centradas (no estandarizadas). DIC no comparable con A/B/C1/D.  
**DIC = 17636.6 (nc) | R²(log) = 0.310 | a = 6.625**

### Modelo D — Tendencia espacial lineal
```
μ_i = α + β₁·temp + β₂·hr + β₃·sen_t + β₄·cos_t + β₅·lat + β₆·lon
```
Prueba si el patrón espacial es un gradiente lineal.  
**DIC = 2841.6 | R² = 0.341 | β_lat = −0.089 (IC incluye 0)**

### Modelo E — Predicción espacial (GP + kriging bayesiano)
Usa los efectos αⱼ del Modelo C1 como puntos de anclaje. Kernel exponencial con ρ = 0.08° ≈ 8.9 km. Propaga incertidumbre con 2,000 muestras MCMC.  
**Rango: 16.3–19.5 µg/m³ | SD entre polígonos = 0.41**  
Más contaminado: Tláhuac (19.5), La Paz (19.4). Menos: Zumpango (16.3), Temascalapa (16.4).

---

## Comparación de modelos

| Modelo | DIC | Pseudo-R² | σ residual |
|--------|-----|-----------|------------|
| A — Normal global | 2942.6 | 0.313 | 0.424 |
| D — Tendencia lat/lon | 2841.6 | 0.341 | 0.416 |
| B — Efectos fijos | 2724.2 | 0.375 | 0.406 |
| **C1 — Jerárquico** | **2723.7** | **0.375** | **0.406** |
| C2 — Gamma (nc) | — | 0.310 (log) | — |

DIC comparable solo entre A, B, C1, D (misma familia Normal log). ΔDIC(A→C1) = 219 puntos: la estructura espacial es el factor explicativo más importante después del clima.

---

## Estructura del repositorio

```
Contaminacion-CDMX/
├── data/
│   ├── raw/                          # CSVs horarios SINAICA 2023
│   ├── clean/
│   │   ├── pm25_valle_mexico_v2.csv  # Dataset final (14 est, 2617 obs)
│   │   └── centroides_valle.csv      # Centroides de 141 polígonos
│   └── gadm_mexico/                  # Shapefile GADM nivel 2
├── scripts/
│   ├── descarga_masiva_valle.R       # Descarga SINAICA (56 estaciones)
│   ├── limpieza_valle_mexico.R       # Procesamiento y filtrado
│   ├── eda_valle_mexico_v2.R         # Análisis exploratorio
│   ├── modelos_A_B_C1_D_v2_profesor.R  # Modelos A, B, C1, C2, D
│   ├── modelo_E_valle_v2_profesor.R  # Modelo E (GP espacial)
│   ├── mapa_anomalia_E_valle_v2.R    # Mapa de anomalías (paleta divergente)
│   ├── mapa_anomalia_E_valle_v2_doble.R  # Mapas separados anomalías
│   ├── jags_modelo_A_valle.txt
│   ├── jags_modelo_B_valle.txt
│   ├── jags_modelo_C1_valle.txt
│   ├── jags_modelo_C2_gamma_valle.txt
│   └── jags_modelo_D_valle.txt
├── output/figures/
│   ├── modelo_{A,B,C1,C2,D}_v2.RData
│   ├── modelo_E_C1base_v2.RData
│   ├── prediccion_espacial_E_valle_v2.csv
│   ├── mapa_prediccion_E_valle_v2.png
│   ├── mapa_anomalia_E_valle_v2.png
│   └── diag_cadena_*.png
├── reporte_proyecto_v2.tex           # Reporte final (LaTeX)
├── reporte_proyecto_v2.pdf           # Reporte compilado
└── README.md
```

---

## Orden de ejecución

```
1. descarga_masiva_valle.R          (~20 min, solo si se quiere rebajar datos)
2. limpieza_valle_mexico.R          (~1 min)
3. eda_valle_mexico_v2.R            (~1 min)
4. modelos_A_B_C1_D_v2_profesor.R   (~45 min, incluye C2 al final)
5. modelo_E_valle_v2_profesor.R     (~15 min)
6. mapa_anomalia_E_valle_v2.R       (~1 min, lee CSV ya generado)
```

---

## Decisiones metodológicas clave

| Decisión | Motivo |
|----------|--------|
| `log(PM2.5)` como respuesta en modelos Normal | Cola derecha, varianza no constante en escala original |
| Fourier (sen_t, cos_t) en lugar de dummies de mes | Captura ciclo anual continuo con solo 2 parámetros |
| Covariables estandarizadas en modelos Normal | Mejor convergencia MCMC, priors comparables |
| Covariables centradas (no estandarizadas) en C2 | La Gamma en JAGS es más estable con centradas |
| Suma-cero post-hoc en Modelo B | JAGS no permite restricciones duras en priors |
| ρ = 0.08° fijo en Modelo E | Con 14 estaciones, estimar ρ dentro de JAGS es inviable |
| Kriging post-hoc en lugar de GP completo en JAGS | JAGS no maneja bien inversión de matrices dependientes de parámetros |
| Breaks dinámicos en mapa de predicción | Rango real (16.3–19.5) cae en solo 2 de 8 bins con breaks fijos |

---

## Pendientes

- **Validación cruzada espacial (LOO-CV):** dejar fuera una estación, predecir con las 13 restantes, medir RMSE out-of-sample.
- **Sensibilidad de ρ:** probar ρ = 0.04° y ρ = 0.12° para evaluar impacto en suavidad del mapa.
- **Recuperar estaciones con imputación:** Camarones y Centro de Ciencias de la Atmósfera tienen PM2.5 pero no temp/HR; asignar datos de la estación vecina más cercana podría subir a 16 estaciones.

---

## Extensiones posibles

### Predicción temporal (out-of-sample)

Los modelos A–D capturan el ciclo anual vía Fourier, por lo que la posterior del Modelo C1 puede extrapolar PM2.5 a fechas fuera del periodo de entrenamiento con solo proporcionar día del año + temperatura y HR climatológicas esperadas:

```r
dia_futuro <- 300  # ej. 27 de octubre
sen_fut    <- sin(2*pi*dia_futuro/365)
cos_fut    <- cos(2*pi*dia_futuro/365)
temp_fut_s <- (temp_climatologica - mean(df$temp)) / sd(df$temp)
hr_fut_s   <- (hr_climatologica   - mean(df$hr))   / sd(df$hr)

# Predictiva posterior por estacion j
logy_pred <- sims$alpha + sims$beta[,1]*temp_fut_s + sims$beta[,2]*hr_fut_s +
             sims$beta[,3]*sen_fut + sims$beta[,4]*cos_fut + sims$alphaj[,j]
pm25_pred <- exp(logy_pred)
```

Aplicación natural: proyectar PM2.5 para Q4 2023 (oct–dic), periodo sin datos en SINAICA, usando temperatura y HR típicas de esos meses. El intervalo de credibilidad posterior cuantifica la incertidumbre.

**Limitación:** la predicción asume condiciones meteorológicas climatológicas típicas, no la meteorología real del periodo — no captura anomalías (contingencias, eventos El Niño).

---

**Última actualización:** 17 de mayo de 2026
