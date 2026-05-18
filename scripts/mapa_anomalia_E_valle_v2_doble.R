### ----- REGRESION AVANZADA ----- ###
# --- Mapas de anomalias separados — Modelo E v2 --- #

library(terra)
library(dplyr)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)
outdir <- "output/figures"

df      <- read.csv("data/clean/pm25_valle_mexico_v2.csv", stringsAsFactors = FALSE)
pred_df <- read.csv(file.path(outdir, "prediccion_espacial_E_valle_v2.csv"), stringsAsFactors = FALSE)

est_obs <- df %>%
  group_by(estacion) %>%
  summarise(lon = mean(lon), lat = mean(lat), pm25_obs = mean(pm25), .groups = "drop")

mex   <- vect("data/gadm_mexico/gadm41_MEX_2.shp")
valle <- mex[mex$NAME_1 %in% c("Distrito Federal", "México"), ]

pred_order      <- match(valle$NAME_2, pred_df$name)
valle$pm25_pred <- pred_df$pm25_mean[pred_order]

media_global <- mean(valle$pm25_pred, na.rm = TRUE)
cat("Media global de prediccion:", round(media_global, 2), "ug/m3\n")

# ============================================================
# MAPA 1: Anomalias espaciales del GP (poligonos)
# ============================================================
valle$anomalia <- valle$pm25_pred - media_global
cat("Rango de anomalias:", round(min(valle$anomalia, na.rm=TRUE), 2),
    "a", round(max(valle$anomalia, na.rm=TRUE), 2), "\n")

breaks <- c(-3, -2, -1, -0.5, 0, 0.5, 1, 2, 3)
valle$cat_anom <- cut(valle$anomalia, breaks = breaks, include.lowest = TRUE)

colors <- c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
            "#fddbc7", "#f4a582", "#d6604d")

png(file.path(outdir, "mapa_anomalia_espacial_E_valle_v2.png"),
    width = 1200, height = 1000, res = 120)
par(mar = c(2, 2, 4, 1))
plot(valle, "cat_anom", col = colors,
     main = paste0("Anomalia espacial PM2.5 — Prediccion GP v2 (14 est)\n",
                   "Desviacion respecto a media global (", round(media_global, 1), " ug/m3)"))
points(est_obs$lon, est_obs$lat, pch = 21,
       bg = "white", col = "black", cex = 1.8, lwd = 2)

pos_vec <- rep(3, nrow(est_obs))
names(pos_vec) <- est_obs$estacion
if("Ajusco Medio" %in% names(pos_vec)) pos_vec["Ajusco Medio"] <- 2
if("UAM Xochimilco" %in% names(pos_vec)) pos_vec["UAM Xochimilco"] <- 4
if("Santiago Acahualtepec" %in% names(pos_vec)) pos_vec["Santiago Acahualtepec"] <- 1
if("Pedregal" %in% names(pos_vec)) pos_vec["Pedregal"] <- 2
if("Tlalnepantla" %in% names(pos_vec)) pos_vec["Tlalnepantla"] <- 2
if("Hospital General de México" %in% names(pos_vec)) pos_vec["Hospital General de México"] <- 4
if("Biblioteca" %in% names(pos_vec)) pos_vec["Biblioteca"] <- 4
if("FES Aragón" %in% names(pos_vec)) pos_vec["FES Aragón"] <- 4
if("San Agustín" %in% names(pos_vec)) pos_vec["San Agustín"] <- 2
if("Gustavo A. Madero" %in% names(pos_vec)) pos_vec["Gustavo A. Madero"] <- 4
if("Merced" %in% names(pos_vec)) pos_vec["Merced"] <- 2
if("Nezahualcóyotl" %in% names(pos_vec)) pos_vec["Nezahualcóyotl"] <- 2
if("Benito Juárez" %in% names(pos_vec)) pos_vec["Benito Juárez"] <- 4
if("UAM Iztapalapa" %in% names(pos_vec)) pos_vec["UAM Iztapalapa"] <- 2

text(est_obs$lon, est_obs$lat, labels = est_obs$estacion,
     pos = pos_vec, cex = 0.6, col = "black", font = 2, offset = 0.7)
legend("bottomleft",
       legend = c("< -2", "-2 a -1", "-1 a -0.5", "-0.5 a 0",
                  "0 a 0.5", "0.5 a 1", "1 a 2", "> 2"),
       fill = colors,
       title = "Anomalia (ug/m3)", bg = "white", cex = 0.85)
dev.off()
cat("Mapa 1 guardado:", file.path(outdir, "mapa_anomalia_espacial_E_valle_v2.png"), "\n")

# ============================================================
# MAPA 2: Anomalias observadas por estacion (obs - pred)
# ============================================================
est_obs$pm25_pred    <- NA
est_obs$anomalia_obs <- NA

for(i in 1:nrow(est_obs)) {
  # Buscar municipio de la estacion en df original
  municipio_est <- df$municipio[df$estacion == est_obs$estacion[i]][1]
  pred_val <- pred_df$pm25_mean[pred_df$name == municipio_est]
  if(length(pred_val) > 0 && !is.na(pred_val[1])) {
    est_obs$pm25_pred[i] <- pred_val[1]
    est_obs$anomalia_obs[i] <- est_obs$pm25_obs[i] - pred_val[1]
  }
}

cat("\nRango anomalias observadas:",
    round(min(est_obs$anomalia_obs, na.rm=TRUE), 2), "a",
    round(max(est_obs$anomalia_obs, na.rm=TRUE), 2), "\n")

breaks_obs <- c(-6, -4, -2, -1, 0, 1, 2, 4, 6)
est_obs$cat_obs <- cut(est_obs$anomalia_obs, breaks = breaks_obs, include.lowest = TRUE)

colors_obs <- c("#2166ac", "#4393c3", "#92c5de", "#f7f7f7",
                "#f4a582", "#d6604d", "#b2182b", "#67001f")

get_color <- function(x) {
  if(is.na(x)) return("gray50")
  colors_obs[findInterval(x, breaks_obs, all.inside = TRUE)]
}
est_obs$col_punto <- sapply(est_obs$anomalia_obs, get_color)

png(file.path(outdir, "mapa_anomalia_observada_E_valle_v2.png"),
    width = 1200, height = 1000, res = 120)
par(mar = c(2, 2, 4, 1))
plot(valle, col = "gray90", border = "gray70",
     main = paste0("Anomalia observada PM2.5 — Modelo E v2 (14 est)\n",
                   "Observado - Predicho por municipio"))
for(i in 1:nrow(est_obs)) {
  if(!is.na(est_obs$anomalia_obs[i])) {
    municipio_est <- df$municipio[df$estacion == est_obs$estacion[i]][1]
    idx <- which(valle$NAME_2 == municipio_est)
    if(length(idx) > 0) {
      plot(valle[idx], col = est_obs$col_punto[i], border = "black", add = TRUE)
    }
  }
}

points(est_obs$lon, est_obs$lat, pch = 21,
       bg = est_obs$col_punto, col = "black", cex = 2.5, lwd = 2)

pos_vec2 <- rep(3, nrow(est_obs))
names(pos_vec2) <- est_obs$estacion
if("Ajusco Medio" %in% names(pos_vec2)) pos_vec2["Ajusco Medio"] <- 2
if("UAM Xochimilco" %in% names(pos_vec2)) pos_vec2["UAM Xochimilco"] <- 4
if("Santiago Acahualtepec" %in% names(pos_vec2)) pos_vec2["Santiago Acahualtepec"] <- 1
if("Pedregal" %in% names(pos_vec2)) pos_vec2["Pedregal"] <- 2
if("Tlalnepantla" %in% names(pos_vec2)) pos_vec2["Tlalnepantla"] <- 2
if("Hospital General de México" %in% names(pos_vec2)) pos_vec2["Hospital General de México"] <- 4
if("Biblioteca" %in% names(pos_vec2)) pos_vec2["Biblioteca"] <- 4
if("FES Aragón" %in% names(pos_vec2)) pos_vec2["FES Aragón"] <- 4
if("San Agustín" %in% names(pos_vec2)) pos_vec2["San Agustín"] <- 2
if("Gustavo A. Madero" %in% names(pos_vec2)) pos_vec2["Gustavo A. Madero"] <- 4
if("Merced" %in% names(pos_vec2)) pos_vec2["Merced"] <- 2
if("Nezahualcóyotl" %in% names(pos_vec2)) pos_vec2["Nezahualcóyotl"] <- 2
if("Benito Juárez" %in% names(pos_vec2)) pos_vec2["Benito Juárez"] <- 4
if("UAM Iztapalapa" %in% names(pos_vec2)) pos_vec2["UAM Iztapalapa"] <- 2

text(est_obs$lon, est_obs$lat, labels = est_obs$estacion,
     pos = pos_vec2, cex = 0.6, col = "black", font = 2, offset = 0.8)
legend("bottomleft",
       legend = c("< -4", "-4 a -2", "-2 a -1", "-1 a 0",
                  "0 a 1", "1 a 2", "2 a 4", "> 4"),
       fill = colors_obs,
       title = "Obs - Pred (ug/m3)", bg = "white", cex = 0.85)
dev.off()
cat("Mapa 2 guardado:", file.path(outdir, "mapa_anomalia_observada_E_valle_v2.png"), "\n")

resumen <- data.frame(
  poligono = valle$NAME_2,
  pred     = round(valle$pm25_pred, 2),
  anomalia = round(valle$anomalia, 2),
  stringsAsFactors = FALSE
)
cat("\n--- Top 10 mas contaminados ---\n")
print(head(resumen[order(-resumen$anomalia), ], 10))
cat("\n--- Top 10 mas limpios ---\n")
print(tail(resumen[order(-resumen$anomalia), ], 10))

est_resumen <- data.frame(
  estacion = est_obs$estacion,
  obs      = round(est_obs$pm25_obs, 1),
  pred     = round(est_obs$pm25_pred, 1),
  anomalia = round(est_obs$anomalia_obs, 2),
  stringsAsFactors = FALSE
)
cat("\n--- Anomalias observadas por estacion ---\n")
print(est_resumen[order(-est_resumen$anomalia), ])
