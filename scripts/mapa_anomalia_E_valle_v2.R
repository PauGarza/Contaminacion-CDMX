### ----- REGRESION AVANZADA ----- ###
# --- Mapa de anomalias espaciales — Modelo E v2 --- #

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

media_global    <- mean(valle$pm25_pred, na.rm = TRUE)
cat("Media global de prediccion:", round(media_global, 2), "ug/m3\n")

valle$anomalia       <- valle$pm25_pred - media_global
est_obs$anomalia_obs <- est_obs$pm25_obs - media_global

breaks <- c(-3, -2, -1, -0.5, 0, 0.5, 1, 2, 3)
valle$cat_anom <- cut(valle$anomalia, breaks = breaks, include.lowest = TRUE)

colors <- c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
            "#fddbc7", "#f4a582", "#d6604d", "#b2182b")

get_color <- function(x) {
  if(is.na(x)) return("gray50")
  colors[findInterval(x, breaks, all.inside = TRUE)]
}
est_obs$col_punto <- sapply(est_obs$anomalia_obs, get_color)

est_obs        <- est_obs[order(est_obs$estacion), ]
est_obs$numero <- 1:nrow(est_obs)

png(file.path(outdir, "mapa_anomalia_E_valle_v2.png"), width = 1400, height = 1000, res = 120)
par(mar = c(2, 2, 4, 8))

plot(valle, "cat_anom", col = colors, 
     main = paste0("Anomalia espacial PM2.5 — Modelo E v2 (GP, rho=0.08°, 14 est)\n",
                   "Prediccion - Media global (", round(media_global, 1), " ug/m3)"))

points(est_obs$lon, est_obs$lat, pch = 21,
       bg = est_obs$col_punto, col = "black", cex = 2.5, lwd = 2)
text(est_obs$lon, est_obs$lat, labels = est_obs$numero,
     pos = 3, cex = 1.0, col = "black", font = 2, offset = 0.6)
legend("bottomleft", inset = c(0.02, 0.02),
       legend = c("< -2", "-2 a -1", "-1 a -0.5", "-0.5 a 0",
                  "0 a 0.5", "0.5 a 1", "1 a 2", "> 2"),
       fill = colors[-c(1, length(colors))],
       title = "Anomalia (ug/m3)", bg = "white", cex = 0.9)
legend("right", inset = c(-0.02, 0),
       legend = paste0(est_obs$numero, ". ", est_obs$estacion),
       title = "Estaciones", bg = "white", cex = 0.75, xpd = TRUE, bty = "n")

dev.off()
cat("Mapa guardado:", file.path(outdir, "mapa_anomalia_E_valle_v2.png"), "\n")

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
  num          = est_obs$numero,
  estacion     = est_obs$estacion,
  obs          = round(est_obs$pm25_obs, 1),
  anomalia_obs = round(est_obs$anomalia_obs, 2),
  stringsAsFactors = FALSE
)
cat("\n--- Anomalias observadas por estacion ---\n")
print(est_resumen[order(-est_resumen$anomalia_obs), ])
