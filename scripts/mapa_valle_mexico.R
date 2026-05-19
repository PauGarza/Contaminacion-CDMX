options(repos="http://cran.itam.mx/")

library(dplyr)
library(terra)
library(RColorBrewer)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)

imgdir <- file.path(wdir, "output/figures/")
dir.create(imgdir, recursive=TRUE, showWarnings=FALSE)

# ============================================================
# 1. Carga de datos
# ============================================================
df_est <- read.csv("data/clean/resumen_valle_mexico.csv", stringsAsFactors=FALSE)

# ============================================================
# 2. Shapefile GADM nivel 2
# ============================================================
mex_mun <- vect("data/gadm_mexico/gadm41_MEX_2.shp")
cdmx_map <- mex_mun[mex_mun$NAME_1 == "Distrito Federal", ]
edomex_map <- mex_mun[mex_mun$NAME_1 == "México", ]
valle_map <- rbind(cdmx_map, edomex_map)
n_pol <- nrow(valle_map)

# ============================================================
# 3. Centros de poligonos y asignacion de estaciones
# ============================================================
g <- geom(valle_map, wkt=FALSE)
centros <- matrix(0, nrow=n_pol, ncol=2)
for (j in 1:n_pol) {
  centros[j, ] <- apply(g[g[,1]==j, 3:4, drop=FALSE], 2, mean, na.rm=TRUE)
}

idx_municipios <- integer(nrow(df_est))
for (i in 1:nrow(df_est)) {
  est <- df_est[i, ]
  dists <- sqrt((centros[,1] - est$lon)^2 + (centros[,2] - est$lat)^2)
  idx_municipios[i] <- which.min(dists)
}

# ============================================================
# 4. Coloreo
# ============================================================
breaks <- c(0, 16, 18, 20, 22, 24, 100)
labels <- c("0-16","16-18","18-20","20-22","22-24",">24")
plotclr <- c("#FFFFB2","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C")

get_color <- function(pm) {
  if (is.na(pm)) return("grey93")
  k <- findInterval(pm, breaks, rightmost.closed=TRUE)
  if (k < 1 || k > length(plotclr)) return("grey93")
  return(plotclr[k])
}

colores <- rep("grey93", n_pol)
pm_vals <- df_est$pm25_mean
for (i in seq_along(pm_vals)) {
  colores[idx_municipios[i]] <- get_color(pm_vals[i])
}

# ============================================================
# 5. Mapa con nombres en centroides
# ============================================================
png(file.path(imgdir, "mapa_valle_mexico.png"), width=1100, height=950)
par(mfrow=c(1,1), mar=c(4,4,5,8))

plot(valle_map, fill=TRUE, col=colores, border="grey55", lwd=0.4,
     xlim=c(-99.35, -98.95), ylim=c(19.15, 19.65),
     main="Valle de Mexico — Alcaldias y municipios con monitoreo de PM2.5 (2023)",
     xlab="Longitud", ylab="Latitud", cex.main=1.1)

# Nombres de TODOS los municipios/alcaldias en centroides
for (j in 1:n_pol) {
  # Solo mostrar nombres de CDMX + municipios de Edomex cercanos (dentro de ventana)
  cx <- centros[j,1]
  cy <- centros[j,2]
  if (cx > -99.35 && cx < -98.95 && cy > 19.15 && cy < 19.65) {
    nombre <- valle_map$NAME_2[j]
    # Acortar nombres muy largos
    nombre <- gsub(" de Morelos| de Baz| Contreras", "", nombre)
    text(cx, cy, labels=nombre, cex=0.5, col="grey25", font=1)
  }
}

# Resaltar bordes de poligonos con estacion
for (i in seq_along(idx_municipios)) {
  plot(valle_map[idx_municipios[i]], add=TRUE, border="grey15", lwd=1.5)
}

# Puntos de estaciones
pm_range <- range(pm_vals)
tamanos <- 2.2 + (pm_vals - pm_range[1]) / max(1, diff(pm_range)) * 3.0
puntos_col <- ifelse(df_est$ciudad == "cdmx", "firebrick2", "steelblue")

points(df_est$lon, df_est$lat,
       pch=21, bg=puntos_col, col="black", cex=tamanos, lwd=1.5)

# Etiquetas de estaciones
for (i in 1:nrow(df_est)) {
  text(df_est$lon[i], df_est$lat[i],
       labels=paste0(df_est$estacion[i], " (", round(df_est$pm25_mean[i],1), ")"),
       pos=4, cex=0.65, offset=0.4, font=2)
}

# Leyenda de colores
legend(x=-98.90, y=19.60,
       legend=labels,
       fill=plotclr, bty="n", cex=0.85, xpd=TRUE,
       title="PM2.5 (ug/m3)")

# Leyenda de puntos
legend(x=-98.90, y=19.40,
       legend=c("CDMX","Edomex"),
       pch=21, pt.bg=c("firebrick2","steelblue"), pt.cex=1.8, bty="n", cex=0.8, xpd=TRUE)

dev.off()
cat("Guardado: mapa_valle_mexico.png\n")
