# Proyecto Final — Regresión Avanzada
## Análisis Bayesiano de la Calidad del Aire en Ciudades Mexicanas

**Equipo:** 2 integrantes  
**Fuente de datos:** SINAICA — Instituto Nacional de Ecología y Cambio Climático  
**URL descarga:** https://sinaica.inecc.gob.mx/data.php?tipo=V  
**Software:** R + JAGS (R2jags)

---

## Pregunta central

¿Cómo varía la concentración de PM2.5 a lo largo del año en las tres zonas metropolitanas más grandes de México, difiere esa dinámica entre ciudades y entre estaciones de monitoreo dentro de cada ciudad, y qué tan importante es la estructura espacial en la predicción de la contaminación?

---

## Ciudades y estaciones

| Ciudad | Red en SINAICA | Estaciones sugeridas |
|--------|---------------|----------------------|
| CDMX | Valle de México | Pedregal, Merced, Tlalnepantla, Benito Juárez (4) |
| Guadalajara | Guadalajara | Centro, Las Águilas, Miravalle (3) |
| Monterrey | Nuevo León | Obispado, San Nicolás, Apodaca, San Pedro (4) |

**Total:** 11 estaciones, año 2023 completo.

**Nota:** Loma Dorada (Guadalajara) fue descartada porque no reportó temperatura ni humedad relativa en 2023.

**Razón de la reducción:** El Modelo E (espacial) requiere coordenadas geográficas y una matriz de distancias manejable. Con ~12 estaciones la matriz es 12×12 y el MCMC converge en tiempos razonables. Más estaciones no mejoran sustancialmente la inferencia pero sí elevan el costo computacional.

---

## Variables

| Variable | Descripción | Rol | Escala |
|----------|-------------|-----|--------|
| `pm25` | Concentración diaria promedio de PM2.5 (µg/m³) | **Y — variable respuesta** | Razón |
| `ciudad` | CDMX / Guadalajara / Monterrey | Agrupador jerárquico | Nominal |
| `estacion` | Nombre de la estación | Agrupador jerárquico anidado | Nominal |
| `lat`, `lon` | Coordenadas geográficas de la estación | Predictor espacial | Intervalo |
| `dia_año` | Día del año (1–365) | Índice temporal | Intervalo |
| `mes` | Mes (1–12) | Estacionalidad (agrupador) | Ordinal |
| `temp` | Temperatura diaria promedio (°C) | Covariable meteorológica | Intervalo |
| `hr` | Humedad relativa diaria promedio (%) | Covariable meteorológica | Intervalo |
| `dia_semana` | Lunes–Domingo (dummy) | Efecto laboral vs. fin de semana | Nominal |
| `sen_t`, `cos_t` | sin(2π·día/365), cos(2π·día/365) | Estacionalidad invernal/veraniega | Intervalo |

**Nota:** Se omite velocidad del viento (`vv`) para reducir la carga de descarga. Temperatura y humedad relativa capturan gran parte de la variabilidad meteorológica relevante para PM2.5.

---

## Cinco objetivos / cinco modelos

### Objetivo 1 — Modelo base Normal global (Modelo A)
**Pregunta:** ¿Existe una relación entre temperatura, humedad y estacionalidad con la concentración de PM2.5, ignorando ciudad y estructura espacial?

```
log(PM2.5_i) ~ N(μ_i, τ)
μ_i = α + β₁·temp_i + β₂·hr_i + β₃·sen_t_i + β₄·cos_t_i
```

- Priors poco informativas: `α, β ~ dnorm(0, 0.001)`, `τ ~ dgamma(0.001, 0.001)`
- MCMC: 10 000 iteraciones, 1 000 burn-in, 2 cadenas (Gibbs, sin thinning)
- Diagnóstico: traza, media ergódica, histograma, ACF para α y cada β
- Métricas: DIC, pseudo-R² = cor(log(Y), Ŷ)²

---

### Objetivo 2 — Modelo con efectos fijos por ciudad (Modelo B)
**Pregunta:** ¿Varía el nivel promedio de PM2.5 y la sensibilidad a las covariables entre CDMX, Guadalajara y Monterrey?

```
log(PM2.5_i) ~ N(μ_i, τ)
μ_i = α + Σⱼ αⱼ·Z_ciudad[i,j] + (β₁ + Σⱼ β₁ⱼ·Z_ciudad[i,j])·temp_i + ...
```

- `Z[i,]` = matriz indicadora de ciudad (3 niveles)
- Restricción de identificabilidad vía **ajuste suma-cero** dentro de JAGS:
  ```
  alpha.adj  <- alpha + mean(alphaj[])
  alphaj.adj[j] <- alphaj[j] - mean(alphaj[])
  ```
- MCMC: 10 000 iter, 1 000 burn-in, 2 cadenas
- Comparación con Modelo A mediante DIC (misma familia)

> **Analogía con el parcial:** idéntica lógica a los efectos por ramo en el modelo (e). Se monitorean `alpha.adj`, `alphaj.adj`, `beta.adj`, `betaj.adj`.

---

### Objetivo 3 — Modelo jerárquico Normal por estación (Modelo C1)
**Pregunta:** ¿Difiere la contaminación entre estaciones dentro de cada ciudad, más allá de las covariables meteorológicas?

```
log(PM2.5_ik) ~ N(μ_ik, τ)
μ_ik = α + αⱼ[ciudad] + αₖ[estación] + β₁·temp_ik + β₂·hr_ik + β₃·sen_t_ik + β₄·cos_t_ik
αₖ | φ ~ N(φⱼ, σ²_est)
φⱼ ~ N(μ₀, σ²_ciudad)
```

- Modelo jerárquico de dos niveles: ciudad → estación
- Intercambiabilidad (Tema 5/6): las estaciones dentro de una ciudad son simétricas a priori
- Priors: `σ_est ~ dunif(0,10)`, `σ_ciudad ~ dunif(0,10)`, `μ₀ ~ dnorm(0, 0.001)`
- MCMC: 50 000 iter, 5 000 burn-in, thin=5 o 10 (por efectos aleatorios)

---

### Objetivo 4 — Modelo jerárquico Gamma por estación (Modelo C2)
**Pregunta:** ¿Mejora el ajuste si modelamos PM2.5 en escala original con una distribución Gamma en lugar de Normal sobre log(PM2.5)?

```
PM2.5_ik ~ Gamma(a, a/μ_ik)
log(μ_ik) = α + αⱼ[ciudad] + αₖ[estación] + β₁·tempc_ik + β₂·hrc_ik + β₃·sen_t_ik + β₄·cos_t_ik
```

- Covariables **centrales y estandarizadas**: `tempc = (temp - mean)/sd`, `hrc = (hr - mean)/sd`
  - **Razón:** evita `exp(β·x)` que desborda si `x` está en escala original (mismo problema que en el parcial, inciso c)
- Prior de β más ajustado: `dnorm(0, 0.01)` en lugar de `0.001`
- Valores iniciales: `alpha = log(mean(y))`, `beta = 0`, `a = 1`
- MCMC: 50 000 iter, 5 000 burn-in, **thin = 10** (Metropolis-Hastings genera alta autocorrelación)
- **DIC no comparable con C1** (familias distintas). Comparación vía **pseudo-R²** en escala original

> **Analogía con el parcial:** misma lógica de incisos c vs e/g. Estandarización obligatoria, inits cuidadosos, thinning alto.

---

### Objetivo 5 — Modelo espacial con Proceso Gaussiano (Modelo E)
**Pregunta:** ¿Existe dependencia espacial no capturada por el agrupamiento ciudad-estación, y mejora la predicción incorporarla?

```
log(PM2.5_i) ~ N(μ_i, τ)
μ_i = α + β₁·temp_i + β₂·hr_i + β₃·sen_t_i + β₄·cos_t_i + f(s_i)
f(s) ~ GP(0, Σ)    donde    Σ_ij = σ²_spatial · exp(-d_ij / ρ)
```

- `s_i` = coordenadas (lat, lon) de la estación de la observación `i`
- `d_ij` = distancia geodésica (km) entre estaciones `i` y `j`
- **Matriz de covarianzas espacial** definida a través de un exponente cuadrático (kernel exponencial)
- En JAGS se implementa como una **Normal multivariada** sobre un vector de efectos por estación, con matriz de covarianzas construida a partir de las distancias
- Priors: `σ_spatial ~ dunif(0, 10)`, `ρ ~ dunif(0, d_max)` donde `d_max` = máxima distancia entre estaciones
- MCMC: 50 000 iter, 5 000 burn-in, thin=10
- Comparación con C1 mediante DIC (ambos Normal)
- Visualización: mapa con contornos de concentración predicha

> **Conexión con temario:** Tema 7 — Modelos espaciales, procesos Gaussianos.

---

## Comparación de modelos

| Modelo | Distribución | Estructura | DIC comparable con | Pseudo-R² |
|--------|-------------|------------|-------------------|-----------|
| A | Normal log | Global (sin grupos) | B, C1, E | Sí |
| B | Normal log | Efectos fijos por ciudad | A, C1, E | Sí |
| C1 | Normal log | Jerárquico ciudad→estación | A, B, E | Sí |
| C2 | Gamma | Jerárquico ciudad→estación | — (sólo R²) | Sí |
| E | Normal log | Espacial GP + covariables | A, B, C1 | Sí |

**Reglas de comparación:**
- DIC solo comparable dentro de la misma familia (Normal log vs Normal log; Gamma vs Gamma)
- Pseudo-R² = cor(Y_obs, Ŷ)² comparable entre **todas** las especificaciones
- Para C2 (Gamma) calcular R² en escala original: `cor(pm25, yf1_mean)²`

---

## Estructura del reporte escrito

Alineada estrictamente con las indicaciones del profesor:

1. **Introducción** — descripción del problema de contaminación en México, contexto de salud pública (OMS, normas mexicanas), ciudades elegidas, objetivos de los 5 incisos.
2. **Descripción de la información** — tabla de variables con escalas de medición, descriptivas (n, media, sd, min, max por ciudad), análisis exploratorio:
   - Serie de tiempo de PM2.5 promedio por ciudad (2023)
   - Boxplot de PM2.5 por mes y ciudad
   - Boxplot de PM2.5 por estación dentro de cada ciudad
   - Histograma de log(PM2.5) para verificar normalidad aproximada
   - Mapa de estaciones con coordenadas (lat, lon)
   - Correlación entre PM2.5 y covariables meteorológicas
3. **Modelado e implementación** — especificación matemática completa de los 5 modelos, distribuciones iniciales con justificación, detalles de corridas JAGS (iteraciones, burn-in, thinning, número de cadenas), diagnóstico de convergencia (trazas, medias ergódicas, ACF).
4. **Interpretación de resultados** — resumen de estimadores (media posterior + IC 95%), selección de variables importantes (IC excluye cero), comparación de modelos (tabla DIC/R²), respuesta a cada objetivo, recomendaciones de política pública.
5. **Referencias** — fuentes consultadas: SINAICA, OMS, papers de calidad del aire, apuntes de clase.
6. **Apéndice** — código R completo y archivos `.txt` de JAGS. **Sin código en las secciones i–iv.**

---

## Instrucciones para Claude Code

Pega esto en Claude Code para arrancar el proyecto:

```
Estoy trabajando en mi proyecto final de Regresión Avanzada (maestría, ITAM).
El tema es análisis bayesiano de calidad del aire en México usando JAGS.

Contexto:
- Datos: concentración diaria de PM2.5 de estaciones SINAICA 
  (CDMX, Guadalajara, Monterrey), año 2023, ~4 estaciones por ciudad
- Variables: PM2.5, temperatura, humedad relativa, coordenadas (lat,lon)
- Software: R + R2jags
- Los datos los voy a descargar manualmente de https://sinaica.inecc.gob.mx/data.php?tipo=V
  y los archivos CSV los pondré en una carpeta llamada /data/raw/

Necesito que me ayudes con lo siguiente en orden:

1. Script de limpieza y unión de los CSVs de SINAICA
   - Leer todos los archivos de /data/raw/
   - Estandarizar nombres de columnas
   - Calcular promedio diario de PM2.5, temperatura y humedad por estación
   - Agregar columnas: ciudad, estacion, lat, lon, dia_año, mes, dia_semana
   - Calcular sen_t = sin(2*pi*dia_año/365), cos_t = cos(2*pi*dia_año/365)
   - Filtrar valores negativos o fuera de rango (PM2.5 > 500 µg/m³, temp < -10 o > 50)
   - Guardar dataset limpio en /data/clean/pm25_clean.csv

2. Script de análisis exploratorio (EDA)
   - Serie de tiempo de PM2.5 promedio por ciudad (2023)
   - Boxplot de PM2.5 por mes y ciudad
   - Boxplot de PM2.5 por estación dentro de cada ciudad
   - Histograma de log(PM2.5) con densidad superpuesta
   - Mapa de estaciones con tamaño proporcional a PM2.5 promedio
   - Matriz de correlación entre PM2.5 y covariables
   - Guardar gráficas en /output/figures/

3. Modelos JAGS — empezar por el Modelo A (Normal global):
   - Archivo ExFinal_A.txt con el modelo BUGS
   - Script R para correr el modelo con R2jags
   - Diagnóstico: traza, promedio ergódico, histograma, ACF para α y βs
   - Resumen de distribución posterior (media, sd, IC 2.5%, IC 97.5%)
   - DIC y pseudo-R²

Los otros modelos (B, C1, C2, E) los iremos haciendo después uno por uno.
Usa la misma estructura de código que en el parcial (adjunto como referencia):
- funciones prob(), cadenas con 2 colores (grey50 y firebrick2)
- guardar imágenes en /output/figures/
- guardar tablas en CSV
```

---

## Descarga de datos — paso a paso

1. Ir a `https://sinaica.inecc.gob.mx/data.php?tipo=V`
2. Seleccionar: **Ciudad de México → Valle de México → [estación] → PM2.5 → 01/01/2023 a 31/12/2023**
3. Descargar CSV → guardar como `cdmx_[estacion]_pm25_2023.csv`
4. Repetir para **temperatura** y **humedad relativa** de las mismas estaciones
5. Repetir para Guadalajara (Jalisco) y Monterrey (Nuevo León)
6. Anotar latitud y longitud de cada estación (aparecen en el portal o en la ficha técnica de SINAICA)
7. Organizar en carpeta `/data/raw/`

**Nombres de archivo sugeridos:**
```
cdmx_pedregal_pm25_2023.csv
cdmx_pedregal_temp_2023.csv
cdmx_pedregal_hr_2023.csv
...
gdl_centro_pm25_2023.csv
mty_obispado_pm25_2023.csv
```

**Archivo auxiliar:** Crear `estaciones.csv` con:
```csv
ciudad,estacion,lat,lon
cdmx,Pedregal,19.32528,-99.20417
cdmx,Merced,19.42472,-99.11972
...
```

---

## Cronograma sugerido

| Semana | Tarea |
|--------|-------|
| 1 | Descarga de datos + limpieza + EDA |
| 2 | Modelo A (Normal global) + Modelo B (efectos fijos por ciudad) |
| 3 | Modelo C1 (jerárquico Normal) + Modelo C2 (jerárquico Gamma) |
| 4 | Modelo E (espacial GP) + comparación global + redacción del reporte |
| 5 | Presentación (10 min: ~2 min intro/EDA + 5 min modelos/comparación + 3 min conclusiones) |

---

## Notas técnicas

- **Transformación:** usar `log(PM2.5)` como Y para cumplir supuesto de normalidad (igual que el parcial usó `log(siniestros)`)
- **Estandarización de covariables:** centrar y escalar temperatura y humedad antes de entrar al modelo Gamma (evita problemas numéricos, como la prima en el modelo Gamma del parcial)
- **Valores faltantes:** SINAICA reporta -99 o celdas vacías para datos sin medición — filtrar antes de modelar
- **Identificabilidad en efectos por ciudad (Modelo B):** aplicar ajuste suma-cero dentro de JAGS usando `mean()`, igual que en el modelo (e) del parcial. Monitorear solo las versiones `.adj`.
- **Thinning:** los modelos jerárquicos (C1, C2) y espacial (E) necesitan adelgazamiento (`thin = 5` o `10`) por autocorrelación en las cadenas
- **Iteraciones sugeridas:**
  - Modelos A y B: 10 000 con 1 000 burn-in
  - Modelos C1, C2 y E: 50 000 con 5 000 burn-in, thin = 10
- **Pseudo-R²:**
  - Modelos A, B, C1, E: `cor(log(pm25), yf1_mean)²`
  - Modelo C2 (Gamma): `cor(pm25, yf1_mean)²` en escala original
- **DIC:** comparable solo dentro de la misma familia (A/B/C1/E son comparables entre sí; C2 solo se compara vía pseudo-R²)
