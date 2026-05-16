options(repos="http://cran.itam.mx/")

library(dplyr)
library(lubridate)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# ============================================================
# EDA v2 — Estructura de clase + mapas reales por ciudad
# ============================================================

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

imgdir <- file.path(wdir, "output/figures/")
dir.create(imgdir, recursive=TRUE, showWarnings=FALSE)

#--- Lectura de datos limpios ---
df <- read.csv("data/clean/pm25_clean.csv", stringsAsFactors=FALSE)
df$date <- as.Date(df$date)

#--- Estadisticos descriptivos por ciudad ---
cat("\n=== Estadisticos descriptivos por ciudad ===\n")
desc <- df %>%
  group_by(ciudad) %>%
  summarise(
    n = n(),
    pm25_mean = round(mean(pm25, na.rm=TRUE), 2),
    pm25_sd = round(sd(pm25, na.rm=TRUE), 2),
    pm25_min = round(min(pm25, na.rm=TRUE), 2),
    pm25_max = round(max(pm25, na.rm=TRUE), 2),
    temp_mean = round(mean(temp, na.rm=TRUE), 2),
    hr_mean = round(mean(hr, na.rm=TRUE), 2),
    .groups = "drop"
  )
print(desc)
write.csv(desc, file.path(imgdir, "eda_descriptivos.csv"), row.names=FALSE)

#--- Descriptivos globales ---
cat("\n=== Descriptivos globales ===\n")
print(summary(df[, c("pm25", "temp", "hr")]))

# ============================================================
# 1. Serie de tiempo de PM2.5 promedio por ciudad
# ============================================================

df_ciudad <- df %>%
  group_by(date, ciudad) %>%
  summarise(pm25_mean = mean(pm25, na.rm=TRUE), .groups="drop")

# Paleta de colores (misma que en el parcial)
pal <- c("cdmx"="firebrick2", "gdl"="steelblue", "mty"="forestgreen")
col_ciudad <- pal[df_ciudad$ciudad]

png(file.path(imgdir, "eda_serie_tiempo.png"), width=1000, height=500)
par(mfrow=c(1,1), mar=c(4,4,3,1))
plot(range(df_ciudad$date), range(df_ciudad$pm25_mean), type="n",
     xlab="Fecha", ylab="PM2.5 (ug/m3)",
     main="PM2.5 promedio diario por ciudad (2023)")
for (c in unique(df_ciudad$ciudad)) {
  idx <- df_ciudad$ciudad == c
  lines(df_ciudad$date[idx], df_ciudad$pm25_mean[idx], col=pal[c], lwd=1.2)
}
legend("topright", legend=c("CDMX","Guadalajara","Monterrey"),
       col=c("firebrick2","steelblue","forestgreen"), lwd=1.5, bty="n")
dev.off()
cat("\nGuardado: eda_serie_tiempo.png\n")

# ============================================================
# 2. Boxplot de PM2.5 por mes y ciudad
# ============================================================

png(file.path(imgdir, "eda_boxplot_mes.png"), width=900, height=500)
par(mfrow=c(1,1), mar=c(4,4,3,1))
boxplot(pm25 ~ mes + ciudad, data=df,
        col=c("firebrick2","steelblue","forestgreen"),
        main="Distribucion de PM2.5 por mes y ciudad",
        xlab="Mes-Ciudad", ylab="PM2.5 (ug/m3)",
        las=2, cex.axis=0.7)
dev.off()
cat("Guardado: eda_boxplot_mes.png\n")

# ============================================================
# 3. Boxplot de PM2.5 por estacion dentro de cada ciudad
# ============================================================

png(file.path(imgdir, "eda_boxplot_estacion.png"), width=1100, height=600)
par(mfrow=c(1,3), mar=c(6,4,3,1))

for (c in c("cdmx","gdl","mty")) {
  df_c <- df[df$ciudad == c, ]
  est_ord <- sort(unique(df_c$estacion))
  boxplot(pm25 ~ estacion, data=df_c,
          col=pal[c],
          main=paste("Ciudad:", toupper(c)),
          xlab="", ylab="PM2.5 (ug/m3)",
          las=2, cex.axis=0.8)
}
dev.off()
cat("Guardado: eda_boxplot_estacion.png\n")

# ============================================================
# 4. Histograma de log(PM2.5)
# ============================================================

png(file.path(imgdir, "eda_hist_logpm25.png"), width=700, height=500)
par(mfrow=c(1,1), mar=c(4,4,3,1))
logy <- log(df$pm25[df$pm25 > 0 & !is.na(df$pm25)])
hist(logy, breaks=30, freq=FALSE, col="grey80", border="white",
     main="Distribucion de log(PM2.5)", xlab="log(PM2.5)", ylab="Densidad")
lines(density(logy), col="firebrick2", lwd=2)
dev.off()
cat("Guardado: eda_hist_logpm25.png\n")

# ============================================================
# 5. Mapas reales por ciudad
# ============================================================

# Obtener shape de Mexico
mex <- ne_states(country="Mexico", returnclass="sf")

# Limites por ciudad
limites <- list(
  cdmx = list(xlim=c(-99.35, -98.95), ylim=c(19.15, 19.65), titulo="CDMX — Valle de Mexico"),
  gdl  = list(xlim=c(-103.55, -103.20), ylim=c(20.55, 20.75), titulo="Guadalajara"),
  mty  = list(xlim=c(-100.55, -100.15), ylim=c(25.60, 25.82), titulo="Monterrey")
)

# Promedio por estacion
df_est <- df %>%
  group_by(estacion, ciudad, lat, lon) %>%
  summarise(pm25_mean = mean(pm25, na.rm=TRUE), .groups="drop")

png(file.path(imgdir, "eda_mapas_ciudades.png"), width=1200, height=450)
par(mfrow=c(1,3), mar=c(3,3,3,1))

for (c in c("cdmx","gdl","mty")) {
  lim <- limites[[c]]
  df_c <- df_est[df_est$ciudad == c, ]
  
  # Recortar shape a los limites
  bbox <- st_bbox(c(xmin=lim$xlim[1], xmax=lim$xlim[2],
                    ymin=lim$ylim[1], ymax=lim$ylim[2]), crs=st_crs(mex))
  mex_c <- st_crop(mex, bbox)
  
  # Plot base con el mapa
  plot(st_geometry(mex_c), col="grey95", border="grey60",
       xlim=lim$xlim, ylim=lim$ylim,
       main=lim$titulo, xlab="Longitud", ylab="Latitud")
  
  # Puntos de estaciones
  puntos <- df_c$pm25_mean
  tamanos <- 1.5 + (puntos - min(puntos)) / (max(puntos) - min(puntos)) * 2.5
  points(df_c$lon, df_c$lat, pch=21, bg=pal[c], col="black", cex=tamanos)
  
  # Etiquetas de estaciones
  text(df_c$lon, df_c$lat, labels=df_c$estacion, pos=3, cex=0.65, offset=0.4)
  
  # Leyenda
  legend("bottomright", legend=paste0("PM2.5=", round(puntos,1)),
         pch=21, pt.bg=pal[c], pt.cex=1.5, bty="n", cex=0.6, title="ug/m3")
}
dev.off()
cat("Guardado: eda_mapas_ciudades.png\n")

# ============================================================
# 6. Correlacion entre variables
# ============================================================

png(file.path(imgdir, "eda_correlacion.png"), width=600, height=500)
M <- cor(df[, c("pm25","temp","hr")], use="complete.obs")
corrplot::corrplot(M, method="color", type="upper", order="original",
                   addCoef.col="black", tl.col="black", tl.srt=45,
                   title="Correlacion entre variables", mar=c(0,0,1,0))
dev.off()
cat("Guardado: eda_correlacion.png\n")

# ============================================================
# 7. PM2.5 vs covariables (scatter con recta de regresion)
# ============================================================

png(file.path(imgdir, "eda_pm25_vs_temp.png"), width=700, height=500)
par(mfrow=c(1,1), mar=c(4,4,3,1))
plot(df$temp, df$pm25, pch=19, cex=0.5, col="grey60",
     xlab="Temperatura (°C)", ylab="PM2.5 (ug/m3)",
     main="PM2.5 vs Temperatura")
abline(lm(pm25 ~ temp, data=df), col="firebrick2", lwd=2)
dev.off()
cat("Guardado: eda_pm25_vs_temp.png\n")

png(file.path(imgdir, "eda_pm25_vs_hr.png"), width=700, height=500)
par(mfrow=c(1,1), mar=c(4,4,3,1))
plot(df$hr, df$pm25, pch=19, cex=0.5, col="grey60",
     xlab="Humedad relativa (%)", ylab="PM2.5 (ug/m3)",
     main="PM2.5 vs Humedad relativa")
abline(lm(pm25 ~ hr, data=df), col="firebrick2", lwd=2)
dev.off()
cat("Guardado: eda_pm25_vs_hr.png\n")

# ============================================================
# 8. Serie de tiempo por estacion (panel)
# ============================================================

png(file.path(imgdir, "eda_serie_por_estacion.png"), width=1200, height=700)
par(mfrow=c(3,4), mar=c(3,3,2,1))

estaciones <- c("pedregal","merced","tlalnepantla","benitojuarez",
                "centro","lasaguilas","miravalle",
                "obispado","sannicolas","apodaca","sanpedro")

for (est in estaciones) {
  df_e <- df[df$estacion == est, ]
  c <- unique(df_e$ciudad)
  plot(df_e$date, df_e$pm25, type="l", col=pal[c], lwd=0.8,
       main=est, xlab="", ylab="PM2.5", xaxt="n")
  axis(1, at=as.Date(c("2023-01-01","2023-04-01","2023-07-01","2023-10-01")),
       labels=c("Ene","Abr","Jul","Oct"))
}
dev.off()
cat("Guardado: eda_serie_por_estacion.png\n")

cat("\n=== EDA v2 completado ===\n")
cat("Imagenes guardadas en:", imgdir, "\n")
