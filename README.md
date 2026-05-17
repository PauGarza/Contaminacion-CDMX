# Proyecto Final — Regresión Avanzada
## Análisis Bayesiano Espacial de PM2.5 en el Valle de México

**Fuente de datos:** SINAICA — Instituto Nacional de Ecología y Cambio Climático  
**URL:** https://sinaica.inecc.gob.mx/data.php?tipo=V  
**Software:** R 4.5.2 + JAGS 4.3.1 (R2jags), terra, dplyr  
**Shapefile:** GADM nivel 2 (municipios/alcaldías)

---

## Pregunta central

> **¿Qué tan importante es la estructura espacial para explicar la variación de PM2.5 en el Valle de México, y podemos usarla para predecir la contaminación en zonas sin sensores?**

Más específicamente:
1. ¿Qué factores (clima, estacionalidad) explican la variación temporal de PM2.5?
2. ¿Existen diferencias sistemáticas entre alcaldías/municipios que no se explican por clima?
3. ¿Es mejor modelar estas diferencias como efectos fijos o como realizaciones de un proceso aleatorio jerárquico?
4. ¿Podemos interpolar espacialmente para predecir en los 76 territorios del Valle de México?

---

## Área de estudio: Valle de México

El Valle de México es una cuenca hidrológica cerrada que alberga la Zona Metropolitana del Valle de México (ZMVM), la mayor aglomeración urbana de México (~22 millones de habitantes).

### Territorios incluidos

| Entidad | Territorios | Con estaciones SINAICA |
|---|---|---|
| **CDMX** | 16 alcaldías | 16 (38 estaciones) |
| **Edomex** | 59 municipios | 15 (17 estaciones) |
| **Hidalgo** | Tizayuca | 1 (1 estación) |
| **Total** | **76 territorios** | **32 territorios con datos (56 estaciones)** |

**Nota:** 44 municipios de Edomex no tienen estaciones de monitoreo de calidad del aire.

### Estaciones de monitoreo SINAICA (2023)

**Dataset final v2 (14 estaciones con datos completos PM2.5 + temp + HR):**

| # | Estación | Alcaldía/Municipio | PM2.5 (µg/m³) | n | Entidad |
|---|----------|-------------------|---------------|---|---|
| 1 | Santiago Acahualtepec | Iztapalapa | 23.5 | 189 | CDMX |
| 2 | Hospital General de México | Cuauhtémoc | 22.7 | 52 | CDMX |
| 3 | Gustavo A. Madero | Gustavo A. Madero | 22.7 | 154 | CDMX |
| 4 | Tlalnepantla | Tlalnepantla de Baz | 21.8 | 174 | Edomex |
| 5 | San Agustín | Ecatepec de Morelos | 20.7 | 84 | Edomex |
| 6 | UAM Iztapalapa | Iztapalapa | 20.6 | 207 | CDMX |
| 7 | Merced | Venustiano Carranza | 20.6 | 235 | CDMX |
| 8 | UAM Xochimilco | Coyoacán | 20.5 | 233 | CDMX |
| 9 | Nezahualcóyotl | Nezahualcóyotl | 20.4 | 218 | Edomex |
| 10 | Benito Juárez | Benito Juárez | 19.8 | 236 | CDMX |
| 11 | FES Aragón | Nezahualcóyotl | 18.0 | 233 | Edomex |
| 12 | Pedregal | Álvaro Obregón | 16.2 | 181 | CDMX |
| 13 | Ajusco Medio | Tlalpan | 16.0 | 188 | CDMX |
| 14 | Biblioteca | Tizayuca | 15.4 | 233 | Hidalgo |

**Total v2:** 14 estaciones, 2,617 observaciones diarias (ene–dic 2023, mayoría ene–sep)

**Estaciones descartadas de las 56 buscadas (sin datos PM2.5 + temp + HR en 2023):**

| Estación | ID | Motivo de exclusión |
|----------|-----|---------------------|
| Azcapotzalco | 315 | Sin datos 2023 |
| Cuitláhuac | 318 | Sin datos 2023 |
| Coyoacán | 247 | Sin datos 2023 |
| Santa Ursula | 400 | Sin datos 2023 |
| Taxqueña | 335 | Sin datos 2023 |
| Santa Fe | 262 | Sin datos 2023 |
| Lagunilla | 324 | Sin datos 2023 |
| Metro Insurgentes | 399 | Sin datos 2023 |
| Museo de la Ciudad de México | 326 | Sin datos 2023 |
| Aragón | 314 | Sin datos 2023 |
| Instituto Mexicano del Petróleo | 322 | Sin datos 2023 |
| La Villa | 325 | Sin datos 2023 |
| San Juan de Aragón | 261 | Sin datos 2023 |
| Vallejo | 401 | Sin datos 2023 |
| Iztacalco | 252 | Sin datos 2023 |
| Cerro de la estrella | 317 | Sin datos 2023 |
| Lomas | 255 | Sin datos 2023 |
| Secretaría de Hacienda | 264 | Sin datos 2023 |
| Tacuba | 334 | Sin datos 2023 |
| Milpa Alta | 299 | Sin datos 2023 |
| Ajusco | 241 | Sin datos 2023 |
| Tlalpan | 336 | Sin datos 2023 |
| Tláhuac | 265 | Solo temp/HR, sin PM2.5 |
| Hangares | 320 | Sin datos 2023 |
| Plateros | 332 | Sin datos 2023 |
| Acolman | 240 | Sin datos 2023 |
| Atizapán | 243 | Sin datos 2023 |
| Chalco | 246 | Solo temp/HR, sin PM2.5 |
| Villa de las Flores | 270 | Solo temp/HR, sin PM2.5 |
| Cuautitlán | 249 | Solo temp/HR, sin PM2.5 |
| Camarones | — | Solo PM2.5, sin temp/HR |
| Centro de Ciencias de la Atmósfera | — | Solo PM2.5, sin temp/HR |
| Cuajimalpa | 248 | Solo temp/HR, sin PM2.5 |
| Miguel Hidalgo | 263 | 1 registro de PM2.5 (insuficiente) |

> **Nota:** De 56 estaciones localizadas espacialmente dentro del Valle de México, solo 14 (25%) reportaron PM2.5, temperatura y humedad relativa de forma simultánea durante 2023. Esto refleja la grave limitación de cobertura del sistema SINAICA.

---

## Variables

| Variable | Descripción | Rol | Escala |
|----------|-------------|-----|--------|
| `pm25` | Concentración diaria promedio de PM2.5 (µg/m³) | **Y — variable respuesta** | Razón |
| `estacion` | Nombre de la estación de monitoreo | Agrupador | Nominal |
| `municipio` | Alcaldía de CDMX o municipio de Edomex/Hidalgo | Agrupador espacial | Nominal |
| `ciudad` | cdmx / edomex / hidalgo | Agrupador estado | Nominal |
| `lat`, `lon` | Coordenadas geográficas | Predictor espacial | Intervalo |
| `dia_año` | Día del año (1–365) | Índice temporal | Intervalo |
| `temp` | Temperatura diaria promedio (°C) | Covariable meteorológica | Intervalo |
| `hr` | Humedad relativa diaria promedio (%) | Covariable meteorológica | Intervalo |
| `sen_t`, `cos_t` | sin(2π·día/365), cos(2π·día/365) | Fourier estacional | Intervalo |

---

## Notación matemática

| Símbolo | Significado | Tipo |
|---------|-------------|------|
| $i$ | Índice de observación diaria ($i = 1, \dots, n$) | Índice |
| $n$ | Total de observaciones (días con datos válidos) | Escalar |
| $j$ | Índice de estación de monitoreo ($j = 1, \dots, J$) | Índice |
| $J$ | Total de estaciones en el modelo | Escalar |
| $\text{PM2.5}_i$ | Concentración diaria de material particulado fino (µg/m³) | Variable respuesta |
| $\mu_i$ | Media de la distribución para la observación $i$ | Parámetro latente |
| $\alpha$ | Intercepto global (media log-PM2.5 cuando todas las X = 0) | Parámetro |
| $\alpha_j$ | Efecto de la estación $j$ (desvío respecto al intercepto global) | Parámetro |
| $\alpha_{\text{adj}}$ | Intercepto ajustado por restricción suma-cero | Parámetro derivado |
| $\alpha_{j,\text{adj}}$ | Efecto estación ajustado por restricción suma-cero | Parámetro derivado |
| $\beta_1, \beta_2, \beta_3, \beta_4$ | Coeficientes de temp, HR, sen_t, cos_t | Parámetros |
| $\tau$ | Precisión del error residual ($\tau = 1/\sigma^2$) | Parámetro |
| $\sigma$ | Desviación estándar del error residual | Parámetro derivado |
| $\tau_\alpha$ | Precisión de la distribución hiperprior de efectos estación | Hiperparámetro |
| $\sigma_\alpha$ | Desviación estándar entre estaciones ($1/\sqrt{\tau_\alpha}$) | Hiperparámetro derivado |
| $\text{temp}_i$ | Temperatura diaria promedio (°C), estandarizada | Covariable |
| $\text{hr}_i$ | Humedad relativa diaria promedio (%), estandarizada | Covariable |
| $\text{sen\_t}_i$ | $\sin(2\pi \cdot \text{día}_i / 365)$ | Covariable estacional |
| $\text{cos\_t}_i$ | $\cos(2\pi \cdot \text{día}_i / 365)$ | Covariable estacional |
| $w(s_i)$ | Efecto espacial (Proceso Gaussiano) en la ubicación $s_i$ | Proceso aleatorio |
| $\Sigma$ | Matriz de covarianza espacial $J \times J$ | Matriz |
| $\rho$ | Rango espacial: distancia donde correlación cae a $e^{-1} \approx 0.37$ | Hiperparámetro |
| $d_{ij}$ | Distancia geográfica entre estaciones $i$ y $j$ (grados) | Distancia |
| $\sigma_{\text{spatial}}$ | Magnitud de la variación espacial | Hiperparámetro |

---

## Cinco modelos: qué pregunta responde cada uno

### Modelo A — Normal global (sin agrupamiento)
> **Pregunta:** ¿Cuánto de la variación temporal de PM2.5 se explica por clima y estacionalidad, **ignorando por completo** dónde se tomó la medición?

```
log(PM2.5_i) ~ N(μ_i, τ)
μ_i = α + β₁·temp_i + β₂·hr_i + β₃·sen_t_i + β₄·cos_t_i
```

**Variables en este modelo:**
| Variable | Rol | Interpretación |
|----------|-----|----------------|
| $\text{PM2.5}_i$ | Respuesta | Concentración diaria en la observación $i$ |
| $\mu_i$ | Media | Valor esperado de log-PM2.5 para la observación $i$ |
| $\alpha$ | Intercepto | log-PM2.5 promedio global cuando temp=HR=0 y estacionalidad=nula |
| $\beta_1$ | Pendiente | Cambio en log-PM2.5 por cada 1-SD de temperatura |
| $\beta_2$ | Pendiente | Cambio en log-PM2.5 por cada 1-SD de humedad relativa |
| $\beta_3, \beta_4$ | Pendientes | Amplitud y fase del patrón estacional anual |
| $\tau$ | Precisión | Inverso de la varianza residual del modelo global |

- **Uso:** Línea base. Si la estructura espacial no importa, este modelo debería ser competitivo.
- **Resultado:** DIC = 1613.0, pseudo-R² = 0.292
- **Hallazgo:** temp y Fourier son significativos, HR no lo es en modelo global.

---

### Modelo B — Normal con efectos fijos por estación
> **Pregunta:** ¿Existen diferencias sistemáticas entre estaciones que no se explican por clima, y cuánto mejoran la predicción si las tratamos como efectos fijos independientes?

```
log(PM2.5_i) ~ N(μ_i, τ)
μ_i = α.adj + αⱼ.adj[estación[i]] + β₁·temp_i + β₂·hr_i + β₃·sen_t_i + β₄·cos_t_i
```

**Variables en este modelo:**
| Variable | Rol | Interpretación |
|----------|-----|----------------|
| $\text{PM2.5}_i$ | Respuesta | Concentración diaria en la observación $i$ |
| $\alpha_{\text{adj}}$ | Intercepto ajustado | Media global de log-PM2.5 tras centrar efectos estación |
| $\alpha_{j,\text{adj}}$ | Efecto fijo | Desvío del intercepto para la estación $j$ (suma cero) |
| $\beta_1, \beta_2, \beta_3, \beta_4$ | Pendientes | Mismo significado que en A, pero ahora controlando por estación |
| $\tau$ | Precisión | Varianza residual *dentro* de cada estación |

- Restricción suma-cero: $\sum_j \alpha_{j,\text{adj}} = 0$, por lo que $\alpha_{j,\text{adj}} > 0$ significa "más contaminada que el promedio" y $< 0$ "menos contaminada".
- 10 efectos estación estimados libremente.

- **Uso:** Captura toda la heterogeneidad estación-específica. Es el "techo" de lo que podemos explicar con covariables observadas.
- **Resultado:** DIC = 1462.7, pseudo-R² = 0.349
- **Hallazgo:** HR se vuelve significativa al controlar por estación (había correlaciones locales ocultas). Santiago Acahualtepec (+0.25 log) y FES Aragón (−0.19 log) son los extremos.

---

### Modelo C1 — Jerárquico Normal (estación ~ N(0, σ²_α))
> **Pregunta:** ¿Podemos modelar las diferencias entre estaciones como realizaciones de un proceso aleatorio común en lugar de parámetros fijos, y cuánto "shrinkage" produce?

```
log(PM2.5_i) ~ N(μ_i, τ)
μ_i = α + αⱼ[estación[i]] + β₁·temp_i + β₂·hr_i + β₃·sen_t_i + β₄·cos_t_i
αⱼ ~ N(0, τ_α)
```

**Variables en este modelo:**
| Variable | Rol | Interpretación |
|----------|-----|----------------|
| $\text{PM2.5}_i$ | Respuesta | Concentración diaria en la observación $i$ |
| $\alpha$ | Intercepto global | Media de log-PM2.5 cuando $w_j = 0$ |
| $\alpha_j$ | Efecto aleatorio | Desvío de la estación $j$, extraído de $N(0, \sigma_\alpha^2)$ |
| $\tau_\alpha$ | Hiperprecisión | Precisión de la distribución de efectos estación |
| $\sigma_\alpha$ | Hiper-SD | Magnitud típica de las diferencias entre estaciones |
| $\beta_1, \beta_2, \beta_3, \beta_4$ | Pendientes | Efectos globales de clima y estacionalidad |
| $\tau$ | Precisión residual | Varianza dentro de estación (después de quitar $w_j$) |

- $\sigma_\alpha \approx 0.13$ implica que la mayoría de estaciones se desvían ±0.26 log-unidades (≈ ±30%) del promedio global.
- **Uso:** Compromiso entre A (global) y B (fijos). Útil cuando hay muchas estaciones con pocos datos (shrinkage hacia la media).
- **Resultado:** DIC = 1463.2, pseudo-R² = 0.349, σ.α = 0.131
- **Hallazgo:** Con ~200 obs/estación, el shrinkage jerárquico no mejora sobre efectos fijos. Los alphaj son similares a B pero con IC más amplios.

---

### Modelo D — Tendencia espacial directa (lat/lon lineal)
> **Pregunta:** ¿Basta con una tendencia lineal en latitud/longitud para capturar el patrón espacial, o necesitamos un modelo no lineal como el GP?

```
log(PM2.5_i) ~ N(μ_i, τ)
μ_i = α + β₁·temp_i + β₂·hr_i + β₃·sen_t_i + β₄·cos_t_i + β₅·lat_i + β₆·lon_i
```

**Variables en este modelo:**
| Variable | Rol | Interpretación |
|----------|-----|----------------|
| $\text{PM2.5}_i$ | Respuesta | Concentración diaria en escala log |
| $\mu_i$ | Media | Valor esperado de log-PM2.5 |
| $\alpha$ | Intercepto | log-PM2.5 global cuando todas las X = 0 |
| $\beta_1, \beta_2, \beta_3, \beta_4$ | Pendientes | Efectos de clima y estacionalidad (mismo significado que A) |
| $\beta_5$ | Pendiente lat | Cambio en log-PM2.5 por cada 1-SD de latitud |
| $\beta_6$ | Pendiente lon | Cambio en log-PM2.5 por cada 1-SD de longitud |
| $\tau$ | Precisión | Varianza residual |

- Latitud y longitud estandarizadas (media 0, SD 1) para comparabilidad con temp/HR.
- **Uso:** Prueba de si el patrón espacial es un gradiente lineal simple. Si D ≈ A, el gradiente lineal no aporta.
- **Resultado:** DIC = 1614.6, pseudo-R² = 0.294
- **Hallazgo:** β_lat = −0.005 (IC incluye 0), β_lon = 0.011 (IC incluye 0). Ambos no significativos. El patrón espacial **no es un gradiente lineal** — requiere un modelo no lineal (GP).

---

### Modelo C2 — Jerárquico Gamma (escala original)
> **Pregunta:** ¿Es la distribución Gamma (que respeta el soporte positivo de PM2.5) mejor que la Normal log-transformada para predecir en escala original?

```
PM2.5_i ~ Gamma(a, a/μ_i)
log(μ_i) = α + αⱼ[estación[i]] + β₁·tempc_i + β₂·hrc_i + β₃·sen_t_i + β₄·cos_t_i
```

**Variables en este modelo:**
| Variable | Rol | Interpretación |
|----------|-----|----------------|
| $\text{PM2.5}_i$ | Respuesta | Concentración diaria en **escala original** (no log) |
| $\mu_i$ | Media Gamma | Valor esperado de PM2.5 para la observación $i$ |
| $a$ | Forma | Parámetro de forma de la Gamma (controla asimetría) |
| $\alpha$ | Intercepto | log-media cuando todas las covariables = 0 |
| $\alpha_j$ | Efecto estación | Desvío en escala log de la estación $j$ |
| $\text{tempc}_i, \text{hrc}_i$ | Covariables | Temperatura y HR **centradas** (media=0, no estandarizadas) |
| $\beta_1, \beta_2, \beta_3, \beta_4$ | Pendientes | Cambio en log-μ por unidad de covariable centrada |

- Covariables centradas (no divididas por SD) para mejor comportamiento numérico en Gamma.
- **Uso:** Modelo alternativo en escala original. No comparable por DIC con A/B/C1/D/E (familia diferente).
- **Resultado:** *En ejecución*

---

### Modelo E — Espacial con Proceso Gaussiano
> **Pregunta:** Dadas las 10 estaciones observadas, ¿podemos predecir PM2.5 promedio en los 141 municipios/alcaldías del Valle de México usando un modelo de correlación espacial?

```
log(PM2.5_i) ~ N(μ_i, τ)
μ_i = α + β₁·temp_i + β₂·hr_i + β₃·sen_t_i + β₄·cos_t_i + w(s_i)
w(s) ~ GP(0, Σ)    donde    Σ_ij = σ²_spatial · exp(-d_ij / ρ)
```

**Variables en este modelo:**
| Variable | Rol | Interpretación |
|----------|-----|----------------|
| $\text{PM2.5}_i$ | Respuesta | Concentración diaria en la estación observada $i$ |
| $\mu_i$ | Media | Valor esperado combinando covariables + efecto espacial |
| $w(s_i)$ | Efecto espacial | Desvío log-PM2.5 atribuible a la ubicación geográfica $s_i$ |
| $\Sigma$ | Covarianza | Matriz $J \times J$ donde $\Sigma_{ij}$ = covarianza entre estaciones $i$ y $j$ |
| $\sigma_{\text{spatial}}$ | Hiper-SD | Magnitud de la variación espacial (cuánto varía $w$) |
| $\rho$ | Rango espacial | Distancia donde correlación espacial cae a $e^{-1} \approx 0.37$ |
| $d_{ij}$ | Distancia | Distancia euclidiana (grados) entre estaciones $i$ y $j$ |
| $\beta_1, \beta_2, \beta_3, \beta_4$ | Pendientes | Efectos globales de clima y estacionalidad (mismo significado que A) |

- Kernel exponencial: estaciones a $< 9$ km tienen correlación $> 0.37$; a $> 27$ km la correlación es $< 0.05$.
- Kriging bayesiano post-hoc: para cada muestra MCMC de C1, calculamos $w_{\text{pred}} = \Sigma_{\text{pred,obs}} \Sigma_{\text{obs,obs}}^{-1} \alpha_j$.
- **Uso:** Interpolación espacial a zonas sin sensores. Es el objetivo final del proyecto.
- **Resultado (10 estaciones originales):** Mapa de predicción para 141 polígonos. Rango: 17.8–20.6 µg/m³.
- **Hallazgo (10 estaciones):** Tláhuac/Valle de Chalco (+1.8 vs promedio) es la zona más contaminada; Álvaro Obregón/Pedregal (−1.0) la más limpia.
- **Resultado v2 (14 estaciones):** Rango: 16.3–19.5 µg/m³ (SD entre polígonos = 0.41). Tláhuac (19.5) y La Paz (19.4) los más contaminados; Zumpango (16.3) y Temascalapa (16.4) los más limpios. El patrón espacial sigue siendo coherente con topografía (cuenca cerrada, ventilación del poniente), pero el rango se comprime ligeramente al tener más puntos de anclaje.

---

## Comparación de modelos (resultados actuales)

| Modelo | Pregunta | DIC | Pseudo-R² | σ residual / CV |
|--------|----------|-----|-----------|-----------------|
| A | ¿Clima + estacionalidad basta? | 1613.0 | 0.292 | σ = 0.357 |
| B | ¿Efectos fijos por estación mejoran? | **1462.7** | **0.349** | σ = 0.344 |
| C1 | ¿Jerárquico es mejor que fijos? | 1463.2 | 0.349 | σ = 0.344 |
| D | ¿Un gradiente lineal lat/lon captura el espacial? | 1614.6 | 0.294 | σ = 0.357 |
| C2 | ¿Gamma es mejor que Normal log? | — | *pendiente* | — |
| E | ¿Podemos predecir en zonas sin sensor? | — | — | — |

**Conclusión intermedia (datos originales 10 estaciones):**
- La estructura espacial (estación) reduce el DIC en ~150 puntos y aumenta el pseudo-R² de 0.29 a 0.35.
- Efectos fijos (B) y jerárquico (C1) son prácticamente equivalentes con ~200 obs/estación.
- **Modelo D valida la necesidad del GP:** una tendencia lineal lat/lon no es significativa (β_lat y β_lon con IC que incluyen 0) y no mejora sobre el modelo global A. El patrón espacial es **no lineal** — requiere un Proceso Gaussiano (Modelo E) o efectos fijos/jerárquicos.

### Resultados con datos v2 (14 estaciones, 2,617 obs)

| Modelo | Pregunta | DIC | Pseudo-R² | σ residual |
|--------|----------|-----|-----------|------------|
| A | ¿Clima + estacionalidad basta? | 2942.6 | 0.314 | 0.424 |
| B | ¿Efectos fijos por estación mejoran? | **2723.8** | **0.375** | 0.406 |
| C1 | ¿Jerárquico es mejor que fijos? | **2723.4** | 0.375 | 0.406 |
| D | ¿Gradiente lineal lat/lon captura espacial? | 2841.3 | 0.340 | 0.416 |
| E | ¿Podemos predecir en zonas sin sensor? | — | — | — |

**Hallazgos v2:**
- El patrón de comparación A < D < B ≈ C1 se mantiene con 14 estaciones.
- C1 gana marginalmente sobre B (DIC 2723.4 vs 2723.8), confirmando que el shrinkage jerárquico es ligeramente preferible con más estaciones.
- σ residual ligeramente mayor (0.406 vs 0.344) porque las 4 estaciones nuevas aportan más heterogeneidad espacial.
- **Modelo E v2:** predicciones para 141 polígonos con rango 16.3–19.5 µg/m³ (SD = 0.41). Tláhuac es la zona más contaminada (19.5), Zumpango la más limpia (16.3).

---

## Estructura del proyecto

```
Contaminacion-CDMX/
├── data/
│   ├── raw/                  # CSVs horarios de SINAICA (2023)
│   ├── clean/                # Datasets diarios listos para análisis
│   └── gadm_mexico/          # Shapefile GADM nivel 2
├── scripts/
│   ├── descarga_masiva_valle.R
│   ├── limpieza_valle_mexico.R
│   ├── eda_valle_mexico.R
│   ├── mapa_valle_mexico_v2.R
│   ├── jags_modelo_A_valle.txt
│   ├── jags_modelo_B_valle.txt
│   ├── jags_modelo_C1_valle.txt
│   ├── modelo_A_valle.R
│   ├── modelo_B_valle.R
│   ├── modelo_C1_valle.R
│   ├── modelo_E_valle.R
│   └── mapa_E_mejorado.R
├── output/
│   ├── modelo_A_valle.RData
│   ├── modelo_B_valle.RData
│   ├── modelo_C1_valle.RData
│   └── figures/              # Mapas, EDA, diagnósticos
├── archive/                  # Versiones obsoletas
└── README.md
```

---

## Visualización espacial

### Mapa observado (10 estaciones)
- Shapefile GADM nivel 2 (16 alcaldías CDMX + 125 municipios Edomex)
- Coloreo de polígonos con estaciones según PM2.5 promedio
- Puntos proporcionales al valor de PM2.5

### Mapa Modelo E — Predicción espacial (141 polígonos)
- Kriging bayesiano con kernel exponencial (ρ = 9 km)
- Predicción de PM2.5 promedio en cada municipio/alcaldía
- Anomalía respecto al promedio global (18.8 µg/m³)

---

## Decisiones metodológicas (log)

### 1. Ampliación de 10 a 56 estaciones (jul 2025)
**Contexto:** El análisis original usaba 10 estaciones con datos completos. El usuario solicitó expandir a todos los territorios del Valle de México (16 alcaldías CDMX + 59 municipios Edomex + Tizayuca Hidalgo).

**Decisión:** Descargar datos de las 56 estaciones SINAICA dentro de estos 76 territorios.
- *Justificación:* Más estaciones = mejor estimación de la estructura espacial y más precisión en la predicción del Modelo E.
- *Limitación:* Solo 15 de 59 municipios de Edomex tienen estaciones. 44 municipios carecen de monitoreo oficial.

### 2. Descarga mensual por restricción de API (jul 2025)
**Contexto:** `rsinaica::sinaica_station_data()` tiene límite de 1 mes por llamada.

**Decisión:** Descargar mes por mes (12 llamadas/estación/variable) en lugar de trimestres.
- *Justificación:* La API rechaza rangas > 1 mes con error "The maximum amount of data you can download is 1 month".
- *Consecuencia:* La descarga completa toma ~20 minutos para 56 estaciones × 3 variables × 12 meses = 2,016 llamadas.

### 3. Filtro: solo estaciones con PM2.5 + temp + HR
**Contexto:** Algunas estaciones miden solo PM2.5 (ej. Camarones) o tienen datos faltantes en temp/HR.

**Decisión:** Excluir estaciones que no tengan las 3 variables simultáneamente.
- *Justificación:* Todos los modelos requieren temp y HR como covariables. Incluir estaciones incompletas introduciría NA que reducirían la muestra efectiva.
- *Consecuencia:* De 56 estaciones descargadas, probablemente ~15-20 queden excluidas por falta de temp/HR.

### 4. Transformación log(PM2.5)
**Contexto:** PM2.5 es positivo y tiene cola derecha (valores altos en contingencias).

**Decisión:** Usar `log(PM2.5)` como variable respuesta en modelos A, B, C1, E.
- *Justificación:* La distribución log-normal aproxima mejor los datos (residuos más simétricos, varianza estabilizada). Permite usar modelos Normal conjugados en JAGS.
- *Alternativa rechazada:* Modelar en escala original con Gamma (C2) es más fiel al soporte positivo pero complica la comparación con DIC y la implementación espacial.

### 5. Estandarización de temp y HR
**Contexto:** Temperatura (~19°C) y HR (~47%) tienen escalas muy diferentes.

**Decisión:** Estandarizar (restar media, dividir por SD) antes de introducirlas a JAGS.
- *Justificación:* Mejora la convergencia MCMC (menos correlación entre α y β). Los priors `dnorm(0, 0.001)` son más informativos cuando las X están en escala comparable.
- *Interpretación:* β₁ = 0.03 significa que por cada 1-SD de temperatura, log-PM2.5 cambia 0.03.

### 6. Suma-cero en Modelo B
**Contexto:** Modelo con intercepto + efectos fijos por estación es no identificable (J+1 parámetros para J grupos).

**Decisión:** Aplicar restricción suma-cero post-muestreo: `α.adj = α + mean(αⱼ)` y `αⱼ.adj = αⱼ - mean(αⱼ)`.
- *Justificación:* JAGS no permite restricciones duras en priors. El ajuste post-hoc garantiza identificabilidad sin modificar el modelo.
- *Alternativa rechazada:* Dejar un efecto como referencia complica la interpretación cuando no hay una estación "obvia" de control.

### 7. Rango espacial ρ = 0.08° (~9 km) en Modelo E
**Contexto:** El Modelo E requiere especificar la escala espacial del kernel exponencial.

**Decisión:** Fijar ρ = 0.08° (~8.9 km a esta latitud).
- *Justificación:* Con solo 10 estaciones, estimar ρ dentro de JAGS es inviable. El valor se eligió como compromiso entre: (a) la distancia mínima entre estaciones (~5 km, para que estaciones cercanas se correlacionen) y (b) el rango efectivo (~3ρ = 27 km, que cubre la mayoría del Valle de México).
- *Sensibilidad:* Un ρ menor daría mapas más "engranados" (más contraste local); un ρ mayor los suavizaría excesivamente.

### 8. Predicción temporal Q4 (Oct-Dic 2023) — jul 2025
**Contexto:** SINAICA no tiene datos de Oct-Dic 2023 para ninguna estación. El dataset se limita a ene-sep.

**Decisión:** Usar las muestras posteriores del Modelo C1 para predecir PM2.5 en los 92 días de Q4, asumiendo condiciones meteorológicas típicas (climatología de temp/HR).
- *Justificación:* El modelo ya captura la estacionalidad vía Fourier. Con temp/HR representativos, la predictiva es razonable.
- *Método:* temp(Oct-Nov) = temp(Sept promedio); temp(Dic) = temp(Ene promedio). HR similar.
- *Resultado:* Oct=15.5, Nov=18.6, Dic=18.2 µg/m³. Patrón coherente: recuperación hacia niveles invernales tras el mínimo estival.
- *Limitación:* No captura anomalías meteorológicas reales de Q4 2023 (ej. contingencias, eventos El Niño).

### 9. Kriging post-hoc en lugar de GP dentro de JAGS
**Contexto:** Implementar un Proceso Gaussiano completo en JAGS con 10 estaciones + 141 puntos de predicción es computacionalmente inviable.

**Decisión:** Usar kriging bayesiano post-hoc: ajustar Modelo C1 en JAGS, luego interpolar espacialmente en R usando las muestras posteriores de αⱼ.
- *Justificación:* JAGS no maneja bien inversión de matrices de covarianza que dependen de parámetros. El enfoque post-hoc es estándar en Bayesiano espacial ("plug-in" de hiperparámetros).
- *Limitación:* No incorpora la incertidumbre de ρ en la predicción espacial (ρ está fijo).

## Notas técnicas

- **Transformación:** `log(PM2.5)` para modelos Normal (linealidad + homocedasticidad)
- **Estandarización:** temp y HR estandarizados (media 0, SD 1) en todos los modelos
- **Identificabilidad:** suma-cero en efectos fijos (Modelo B)
- **MCMC:** 12,000 iteraciones, 3,000 burn-in, thin=3 (6,000 muestras efectivas)
- **DIC:** comparable solo dentro de familia Normal log (A, B, C1, E)
- **Pseudo-R²:** cor(logy, ŷ)², comparable entre todos los modelos
