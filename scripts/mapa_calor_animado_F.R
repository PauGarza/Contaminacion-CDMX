### ----- REGRESION AVANZADA ----- ###
# Mapa de calor animado (GIF) de PM2.5 en el Valle de Mexico
# Fuente: Modelo F — Fase 2 (modelo jerarquico bayesiano)
# Granularidad: semanal (52 frames)
# Requiere: output/figures/Contaminacion_Fase1_SerieTiempo.RData
#           output/figures/Contaminacion_Fase2_SerieTiempo.RData

library(dplyr)
library(sf)
library(ggplot2)
library(scales)
library(gganimate)
library(gifski)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)
outdir <- "output/figures"

# ============================================================
# 1. Cargar resultados del Modelo F
# ============================================================
load(file.path(outdir, "Contaminacion_Fase1_SerieTiempo.RData"))  # -> output_jags
load(file.path(outdir, "Contaminacion_Fase2_SerieTiempo.RData"))  # -> output_fase2

cat("Modelos cargados.\n")

# ============================================================
# 2. Coordenadas de estaciones (orden alfabetico = orden del modelo)
# ============================================================
df_orig <- read.csv("data/clean/pm25_valle_mexico.csv")
df_orig$date <- as.Date(df_orig$date)

estaciones <- df_orig %>%
  select(estacion, lat, lon) %>%
  distinct() %>%
  arrange(estacion)

cat("Estaciones del modelo (orden=indice JAGS):\n")
print(estaciones)
cat("\n")

# ============================================================
# 3. Reconstruir PM2.5 diario por estacion desde Modelo F Fase 2
#    log(PM2.5_jt) = mu_cdmx[t] + alpha_corregido[j]
# ============================================================
resumen_f2 <- output_fase2$BUGSoutput$summary

idx_mu    <- grep("^mu_cdmx\\[",          rownames(resumen_f2))
idx_alpha <- grep("^alpha_corregido\\[",  rownames(resumen_f2))

mu_mean    <- resumen_f2[idx_mu,    "mean"]   # 365 valores
alpha_mean <- resumen_f2[idx_alpha, "mean"]   # 14 valores

N_dias     <- length(mu_mean)
N_est      <- length(alpha_mean)
fechas     <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")

stopifnot(N_dias == 365, N_est == nrow(estaciones))

# Matriz 365 x 14
pm25_mat <- outer(mu_mean, alpha_mean, FUN = function(m, a) exp(m + a))

# ============================================================
# 4. Construir data frame largo y agregar por semana
# ============================================================
df_diario <- data.frame(
  fecha   = rep(fechas, times = N_est),
  est_idx = rep(seq_len(N_est), each = N_dias),
  pm25    = as.vector(pm25_mat)
) %>%
  mutate(
    estacion = estaciones$estacion[est_idx],
    lat      = estaciones$lat[est_idx],
    lon      = estaciones$lon[est_idx],
    semana   = as.integer(strftime(fecha, format = "%V"))
  )

df_semanal_est <- df_diario %>%
  group_by(semana, estacion, lat, lon) %>%
  summarise(pm25_semana = mean(pm25, na.rm = TRUE), .groups = "drop")

# Escala estirada sobre el rango P5-P95 para maximizar contraste de color.
# Valores fuera de ese rango quedan recortados al extremo de la paleta (squish).
pm25_min <- quantile(df_semanal_est$pm25_semana, 0.05, na.rm = TRUE)
pm25_max <- quantile(df_semanal_est$pm25_semana, 0.95, na.rm = TRUE)
cat(sprintf("Rango PM2.5 semanal (abs): %.1f — %.1f ug/m3\n",
            min(df_semanal_est$pm25_semana), max(df_semanal_est$pm25_semana)))
cat(sprintf("Limites de color (P5-P95): %.1f — %.1f ug/m3\n", pm25_min, pm25_max))

# ============================================================
# 5. Cargar shapefile y filtrar Valle de Mexico
# ============================================================
mex_mun   <- st_read("data/gadm_mexico/gadm41_MEX_2.shp", quiet = TRUE)
valle_map <- mex_mun %>%
  filter(NAME_1 %in% c("Distrito Federal", "México"))

# Centroides para asignar estacion mas cercana
centroides   <- suppressWarnings(st_centroid(valle_map))
coords_cent  <- st_coordinates(centroides)

asignar_estacion <- function(cx, cy) {
  dists <- (estaciones$lon - cx)^2 + (estaciones$lat - cy)^2
  estaciones$estacion[which.min(dists)]
}

valle_map$estacion_cercana <- mapply(
  asignar_estacion,
  coords_cent[, 1],
  coords_cent[, 2]
)

# ============================================================
# 6. Unir datos semanales al mapa
# ============================================================
df_mapa <- valle_map %>%
  left_join(df_semanal_est, by = c("estacion_cercana" = "estacion")) %>%
  filter(!is.na(semana))

# Etiqueta legible para cada semana (fecha del lunes)
semana_a_fecha <- data.frame(semana = 1:52) %>%
  mutate(fecha_ini = as.Date(paste0("2023-W", sprintf("%02d", semana), "-1"),
                             format = "%Y-W%U-%u") + 1)

# Si hay problemas con %V/%U, usamos un vector manual
primer_lunes <- as.Date("2023-01-02")  # primer lunes del año
semana_labels <- data.frame(
  semana      = 1:52,
  semana_str  = paste0(format(primer_lunes + (0:51) * 7, "%d %b"), " – ",
                       format(primer_lunes + (0:51) * 7 + 6, "%d %b %Y"))
)

df_mapa <- df_mapa %>%
  left_join(semana_labels, by = "semana") %>%
  mutate(semana_f = factor(semana_str, levels = semana_labels$semana_str))

# Tabla de estaciones para puntos sobre el mapa
sf_estaciones <- st_as_sf(estaciones, coords = c("lon", "lat"), crs = 4326)

# ============================================================
# 7. Construir grafico animado
# ============================================================
p_anim <- ggplot() +
  geom_sf(data = df_mapa,
          aes(fill = pm25_semana),
          color = "white", linewidth = 0.12) +
  geom_sf(data = sf_estaciones,
          color = "#2C3E50", size = 2, shape = 21,
          fill = "white", stroke = 0.8) +
  scale_fill_gradientn(
    colors = c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C"),
    limits = c(pm25_min, pm25_max),
    oob    = squish,
    name   = expression(PM[2.5]~"(µg/m³)"),
    guide  = guide_colorbar(
      barwidth  = 0.8,
      barheight = 10,
      title.position = "top"
    )
  ) +
  coord_sf(xlim = c(-99.40, -98.90), ylim = c(19.10, 19.70)) +
  labs(
    title    = "Contaminación por PM2.5 en el Valle de México",
    subtitle = "{current_frame}",
    x = "Longitud", y = "Latitud",
    caption  = "Modelo F Bayesiano Jerárquico  |  SINAICA 2023  |  ITAM"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", color = "#2C3E50", size = 14),
    plot.subtitle   = element_text(color = "#E74C3C", size = 11, face = "bold"),
    plot.caption    = element_text(color = "gray50", size = 8),
    legend.position = "right",
    legend.title    = element_text(size = 9, face = "bold"),
    panel.grid      = element_line(color = "gray88", linewidth = 0.25),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  transition_manual(semana_f)

# ============================================================
# 8. Renderizar y guardar GIF
# ============================================================
cat("Renderizando GIF (52 frames a 3 fps)...\n")

anim <- animate(
  p_anim,
  nframes  = 52,
  fps      = 3,
  width    = 820,
  height   = 720,
  res      = 110,
  renderer = gifski_renderer()
)

ruta_gif <- file.path(outdir, "mapa_calor_animado_F.gif")
anim_save(ruta_gif, animation = anim)
cat("GIF guardado en:", ruta_gif, "\n")
