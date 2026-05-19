# ============================================================
# Mapa general — Observado + Prediccion espacial (14 estaciones)
# ============================================================
library(terra)
library(dplyr)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)
outdir <- "output/figures"

# Cargar datos
df <- read.csv("data/clean/pm25_valle_mexico.csv", stringsAsFactors = FALSE)
pred_df <- read.csv(file.path(outdir, "prediccion_espacial_E_valle.csv"), stringsAsFactors = FALSE)

# Promedio observado por estacion
est_obs <- df %>%
  group_by(estacion) %>%
  summarise(lon = mean(lon), lat = mean(lat), pm25_obs = mean(pm25), .groups = "drop")

# Shapefile
mex <- vect("data/gadm_mexico/gadm41_MEX_2.shp")
valle <- mex[mex$NAME_1 %in% c("Distrito Federal", "México"), ]

# Asignar predicciones
pred_order <- match(valle$NAME_2, pred_df$name)
valle$pm25_pred <- pred_df$pm25_mean[pred_order]
valle$pm25_q2.5 <- pred_df$pm25_q2.5[pred_order]
valle$pm25_q97.5 <- pred_df$pm25_q97.5[pred_order]

# Categorias
cats <- c(12, 14, 16, 18, 20, 22, 24, 26, 50)
valle$cat <- cut(valle$pm25_pred, breaks = cats, include.lowest = TRUE)
colors <- c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c",
            "#fc4e2a", "#e31a1c", "#b10026")

# Panel de 2 mapas
png(file.path(outdir, "mapa_general.png"), width = 1800, height = 900, res = 120)
par(mfrow = c(1, 2), mar = c(2, 2, 4, 1))

# Panel A: Prediccion espacial
plot(valle, "cat", col = colors, main = "A) PM2.5 predicho — Modelo E (GP espacial, 14 est)")
points(est_obs$lon, est_obs$lat, pch = 21, bg = "white", col = "black", cex = 1.8, lwd = 2)
text(est_obs$lon, est_obs$lat, labels = est_obs$estacion, pos = 3, cex = 0.5, col = "black", font = 2)
legend("bottomleft", legend = levels(valle$cat), fill = colors,
       title = "PM2.5 (ug/m3)", bg = "white", cex = 0.8)

# Panel B: Observado vs Predicho (anomalia)
valle$anomalia <- NA
for(i in 1:nrow(valle)) {
  nm <- valle$NAME_2[i]
  obs_match <- est_obs$pm25_obs[est_obs$estacion == nm]
  if(length(obs_match) > 0) {
    valle$anomalia[i] <- valle$pm25_pred[i] - obs_match[1]
  }
}

# Para poligonos sin estacion, anomalia = 0 (no hay comparacion)
valle$anomalia[is.na(valle$anomalia)] <- 0

cats_anom <- c(-5, -2, -1, -0.5, 0.5, 1, 2, 5)
valle$cat_anom <- cut(valle$anomalia, breaks = cats_anom, include.lowest = TRUE)
colors_anom <- c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fddbc7", "#f4a582", "#d6604d", "#b2182b")

plot(valle, "cat_anom", col = colors_anom, 
     main = "B) Anomalia: Predicho - Observado (solo poligonos con estacion)")
points(est_obs$lon, est_obs$lat, pch = 21, bg = "white", col = "black", cex = 1.8, lwd = 2)
legend("bottomleft", legend = levels(valle$cat_anom), fill = colors_anom,
       title = "Diferencia (ug/m3)", bg = "white", cex = 0.8)

dev.off()
cat("Mapa general guardado:", file.path(outdir, "mapa_general.png"), "\n")

# Tabla de anomalias
cat("\n=== Anomalias (Predicho - Observado) ===\n")
anom_df <- data.frame(
  estacion = est_obs$estacion,
  pm25_obs = round(est_obs$pm25_obs, 1),
  stringsAsFactors = FALSE
)
anom_df$pm25_pred <- NA
anom_df$anomalia <- NA
for(i in 1:nrow(est_obs)) {
  # Buscar municipio de la estacion en df
  municipio_est <- df$municipio[df$estacion == est_obs$estacion[i]][1]
  pred_val <- pred_df$pm25_mean[pred_df$name == municipio_est]
  if(length(pred_val) > 0) {
    anom_df$pm25_pred[i] <- round(pred_val[1], 1)
    anom_df$anomalia[i] <- round(pred_val[1] - est_obs$pm25_obs[i], 2)
  }
}
print(anom_df)
write.csv(anom_df, file.path(outdir, "anomalias_obs_pred.csv"), row.names = FALSE)
