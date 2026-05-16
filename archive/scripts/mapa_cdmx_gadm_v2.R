options(repos="http://cran.itam.mx/")

library(dplyr)
library(terra)
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

est_cdmx <- df_est[df_est$ciudad == "cdmx", ]

# ============================================================
# 2. Carga shapefile GADM nivel 2
# ============================================================
mex_mun <- vect("data/gadm_mexico/gadm41_MEX_2.shp")

# Filtrar CDMX + Estado de Mexico (Valle de Mexico)
idx_cdmx <- mex_mun$NAME_1 == "Distrito Federal"
idx_edomex <- mex_mun$NAME_1 == "México"
valle_map <- mex_mun[idx_cdmx | idx_edomex, ]
n_pol <- nrow(valle_map)

# ============================================================
# 3. Asignar estaciones a alcaldias/municipios mas cercanos
# ============================================================
g <- geom(valle_map, wkt=FALSE)
centros <- matrix(0, nrow=n_pol, ncol=2)
for (j in 1:n_pol) {
  centros[j, ] <- apply(g[g[,1]==j, 3:4, drop=FALSE], 2, mean, na.rm=TRUE)
}

idx_municipios <- integer(nrow(est_cdmx))
for (i in 1:nrow(est_cdmx)) {
  est <- est_cdmx[i, ]
  dists <- sqrt((centros[,1] - est$lon)^2 + (centros[,2] - est$lat)^2)
  idx_municipios[i] <- which.min(dists)
}

# ============================================================
# 4. Coloreo por cortes fijos de PM2.5 (mas finos para CDMX)
# ============================================================
# Intervalos ajustados a los valores de CDMX: [0,16), [16,18), [18,20), [20,22), [22,Inf)
breaks <- c(0, 16, 18, 20, 22, 100)
labels <- c("0-16","16-18","18-20","20-22",">22")
plotclr <- c("#FFFFB2","#FED976","#FEB24C","#FD8D3C","#E31A1C")

get_color <- function(pm) {
  if (is.na(pm)) return("grey93")
  k <- findInterval(pm, breaks, rightmost.closed=TRUE)
  if (k < 1 || k > length(plotclr)) return("grey93")
  return(plotclr[k])
}

colores <- rep("grey93", n_pol)
pm_vals <- est_cdmx$pm25_mean
for (i in seq_along(pm_vals)) {
  colores[idx_municipios[i]] <- get_color(pm_vals[i])
}

# ============================================================
# 5. Mapa del Valle de Mexico (zoom en CDMX)
# ============================================================
png(file.path(imgdir, "mapa_cdmx_valle_gadm_v2.png"), width=950, height=850)
par(mfrow=c(1,1), mar=c(4,4,5,8))

plot(valle_map, fill=TRUE, col=colores, border="grey55", lwd=0.5,
     xlim=c(-99.28, -99.03), ylim=c(19.20, 19.58),
     main="CDMX — Alcaldias y municipios con monitoreo de PM2.5 (2023)",
     xlab="Longitud", ylab="Latitud", cex.main=1.2)

# Resaltar bordes de alcaldias con estacion (borde mas grueso)
for (i in seq_along(idx_municipios)) {
  plot(valle_map[idx_municipios[i]], add=TRUE, border="grey20", lwd=1.5)
}

# Puntos de estaciones
tamanos <- c(2.5, 3.0, 2.2, 3.2)
points(est_cdmx$lon, est_cdmx$lat,
       pch=21, bg="firebrick2", col="black", cex=tamanos, lwd=1.5)

# Etiquetas de estaciones con lineas guia
pos_vec <- c(4, 4, 4, 2)
for (i in 1:nrow(est_cdmx)) {
  text(est_cdmx$lon[i], est_cdmx$lat[i],
       labels=paste0(est_cdmx$estacion[i], "\n(", round(est_cdmx$pm25_mean[i],1), ")"),
       pos=pos_vec[i], cex=0.8, offset=0.7, font=2)
}

# Leyenda de colores municipales (afuera del plot a la derecha)
legend(x=-98.98, y=19.55,
       legend=labels,
       fill=plotclr, bty="n", cex=0.9, xpd=TRUE,
       title="PM2.5 (ug/m3)")

# Leyenda de puntos
legend(x=-98.98, y=19.35,
       legend="Estacion de monitoreo",
       pch=21, pt.bg="firebrick2", pt.cex=2.2, bty="n", cex=0.85, xpd=TRUE)

dev.off()
cat("Guardado: mapa_cdmx_valle_gadm_v2.png\n")

# ============================================================
# 6. Tabla de asignacion
# ============================================================
tab <- data.frame(
  estacion = est_cdmx$estacion,
  pm25_mean = round(pm_vals, 2),
  alcaldia_municipio = valle_map$NAME_2[idx_municipios],
  estado = valle_map$NAME_1[idx_municipios],
  rango = sapply(pm_vals, function(pm) labels[findInterval(pm, breaks, rightmost.closed=TRUE)])
)
write.csv(tab, file.path(imgdir, "mapa_cdmx_valle_tabla_v2.csv"), row.names=FALSE)
cat("Tabla guardada: mapa_cdmx_valle_tabla_v2.csv\n")
print(tab)
