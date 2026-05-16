options(repos="http://cran.itam.mx/")

library(dplyr)
library(terra)
library(classInt)
library(RColorBrewer)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

imgdir <- file.path(wdir, "output/figures/")
dir.create(imgdir, recursive=TRUE, showWarnings=FALSE)

# ============================================================
# 1. Carga de datos
# ============================================================
df <- read.csv("data/clean/pm25_clean.csv", stringsAsFactors=FALSE)

df_est <- df %>%
  group_by(estacion, ciudad, lat, lon) %>%
  summarise(pm25_mean = mean(pm25, na.rm=TRUE), .groups="drop")

# Solo estaciones del Valle de Mexico (cdmx)
est_cdmx <- df_est[df_est$ciudad == "cdmx", ]
cat("Estaciones CDMX:\n")
print(est_cdmx)

# ============================================================
# 2. Carga shapefile GADM nivel 2 (municipios)
# ============================================================
mex_mun <- vect("data/gadm_mexico/gadm41_MEX_2.shp")

# Filtrar CDMX (Distrito Federal) + Estado de Mexico
idx_cdmx <- mex_mun$NAME_1 == "Distrito Federal"
idx_edomex <- mex_mun$NAME_1 == "México"
valle_map <- mex_mun[idx_cdmx | idx_edomex, ]
cat("\nPoligonos Valle de Mexico:", nrow(valle_map), "\n")

# ============================================================
# 3. Asignar estaciones a alcaldias/municipios mas cercanos
# ============================================================
g <- geom(valle_map, wkt=FALSE)
n_pol <- nrow(valle_map)

# Centros de cada poligono
centros <- matrix(0, nrow=n_pol, ncol=2)
for (j in 1:n_pol) {
  centros[j, ] <- apply(g[g[,1]==j, 3:4, drop=FALSE], 2, mean, na.rm=TRUE)
}

idx_municipios <- integer(nrow(est_cdmx))
for (i in 1:nrow(est_cdmx)) {
  est <- est_cdmx[i, ]
  dists <- sqrt((centros[,1] - est$lon)^2 + (centros[,2] - est$lat)^2)
  idx_municipios[i] <- which.min(dists)
  cat("Estacion", est$estacion, "->", valle_map$NAME_2[idx_municipios[i]],
      "(", valle_map$NAME_1[idx_municipios[i]], ") dist=", round(min(dists),4), "\n")
}

# ============================================================
# 4. Coloreo por cortes fijos de PM2.5
# ============================================================
# Intervalos: [0,15), [15,18), [18,21), [21,24), [24,27), [27,Inf)
breaks <- c(0, 15, 18, 21, 24, 27, 100)
labels <- c("0-15","15-18","18-21","21-24","24-27",">27")
plotclr <- brewer.pal(length(labels), "YlOrRd")

get_color <- function(pm) {
  if (is.na(pm)) return("grey92")
  k <- findInterval(pm, breaks, rightmost.closed=TRUE)
  if (k < 1 || k > length(plotclr)) return("grey92")
  return(plotclr[k])
}

# Inicializar: gris claro
colores <- rep("grey92", n_pol)

# Colorear municipios con estacion
pm_vals <- est_cdmx$pm25_mean
for (i in seq_along(pm_vals)) {
  colores[idx_municipios[i]] <- get_color(pm_vals[i])
}

# ============================================================
# 5. Mapa del Valle de Mexico
# ============================================================
png(file.path(imgdir, "mapa_cdmx_valle_gadm.png"), width=1000, height=900)
par(mfrow=c(1,1), mar=c(4,4,5,1))

plot(valle_map, fill=TRUE, col=colores, border="grey50", lwd=0.6,
     xlim=c(-99.35, -98.95), ylim=c(19.15, 19.65),
     main="Valle de Mexico — Alcaldias y municipios con monitoreo de PM2.5 (2023)",
     xlab="Longitud", ylab="Latitud", cex.main=1.15)

# Puntos de estaciones (tamanos proporcionales)
pm_range <- range(pm_vals)
tamanos <- 2.2 + (pm_vals - pm_range[1]) / max(1, diff(pm_range)) * 3.0

points(est_cdmx$lon, est_cdmx$lat,
       pch=21, bg="firebrick2", col="black", cex=tamanos, lwd=1.5)

# Etiquetas de estaciones
# Mover tlalnepantla un poco para que no se encime
pos_vec <- c(4, 4, 4, 2)
offset_vec <- c(0.6, 0.6, 0.6, 0.6)
for (i in 1:nrow(est_cdmx)) {
  text(est_cdmx$lon[i], est_cdmx$lat[i],
       labels=paste0(est_cdmx$estacion[i], " (", round(est_cdmx$pm25_mean[i],1), ")"),
       pos=pos_vec[i], cex=0.85, offset=offset_vec[i], font=2)
}

# Leyenda de colores municipales
legend("bottomright",
       legend=labels,
       fill=plotclr, bty="n", cex=0.85,
       title="PM2.5 por alcaldia/municipio (ug/m3)")

# Leyenda de puntos
legend("bottomleft",
       legend=c("Estacion de monitoreo", "Tamaño proporcional a PM2.5"),
       pch=21, pt.bg="firebrick2", pt.cex=c(2.0, 3.5), bty="n", cex=0.8)

dev.off()
cat("\nGuardado: mapa_cdmx_valle_gadm.png\n")

# ============================================================
# 6. Tabla de asignacion
# ============================================================
tab <- data.frame(
  estacion = est_cdmx$estacion,
  pm25_mean = round(pm_vals, 2),
  alcaldia_municipio = valle_map$NAME_2[idx_municipios],
  estado = valle_map$NAME_1[idx_municipios],
  color = colores[idx_municipios]
)
write.csv(tab, file.path(imgdir, "mapa_cdmx_valle_tabla.csv"), row.names=FALSE)
cat("Tabla guardada: mapa_cdmx_valle_tabla.csv\n")
print(tab)
