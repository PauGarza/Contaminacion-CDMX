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
df_est <- read.csv("data/clean/resumen_cdmx_completo.csv", stringsAsFactors=FALSE)
cat("Estaciones con datos completos:\n")
print(df_est[, c("estacion","lat","lon","pm25_mean")])

# ============================================================
# 2. Shapefile GADM nivel 2 (alcaldias de CDMX)
# ============================================================
mex_mun <- vect("data/gadm_mexico/gadm41_MEX_2.shp")
cdmx_map <- mex_mun[mex_mun$NAME_1 == "Distrito Federal", ]
n_pol <- nrow(cdmx_map)
cat("\nAlcaldias CDMX:", n_pol, "\n")
print(cdmx_map$NAME_2)

# ============================================================
# 3. Asignar estaciones a alcaldias mas cercanas
# ============================================================
g <- geom(cdmx_map, wkt=FALSE)
centros <- matrix(0, nrow=n_pol, ncol=2)
for (j in 1:n_pol) {
  centros[j, ] <- apply(g[g[,1]==j, 3:4, drop=FALSE], 2, mean, na.rm=TRUE)
}

idx_alcaldias <- integer(nrow(df_est))
for (i in 1:nrow(df_est)) {
  est <- df_est[i, ]
  dists <- sqrt((centros[,1] - est$lon)^2 + (centros[,2] - est$lat)^2)
  idx_alcaldias[i] <- which.min(dists)
  cat("Estacion", est$estacion, "->", cdmx_map$NAME_2[idx_alcaldias[i]],
      "dist=", round(min(dists),4), "\n")
}

# ============================================================
# 4. Coloreo por cortes fijos de PM2.5
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
  colores[idx_alcaldias[i]] <- get_color(pm_vals[i])
}

# ============================================================
# 5. Mapa de CDMX
# ============================================================
png(file.path(imgdir, "mapa_cdmx_completo.png"), width=950, height=850)
par(mfrow=c(1,1), mar=c(4,4,5,8))

plot(cdmx_map, fill=TRUE, col=colores, border="grey50", lwd=0.5,
     xlim=c(-99.25, -99.00), ylim=c(19.25, 19.55),
     main="CDMX — Alcaldias con monitoreo de PM2.5 (2023)\n7 estaciones con datos completos",
     xlab="Longitud", ylab="Latitud", cex.main=1.15)

# Resaltar bordes de alcaldias con estacion
for (i in seq_along(idx_alcaldias)) {
  plot(cdmx_map[idx_alcaldias[i]], add=TRUE, border="grey20", lwd=1.5)
}

# Puntos de estaciones (tamanos proporcionales)
pm_range <- range(pm_vals)
tamanos <- 2.0 + (pm_vals - pm_range[1]) / max(1, diff(pm_range)) * 3.0

points(df_est$lon, df_est$lat,
       pch=21, bg="firebrick2", col="black", cex=tamanos, lwd=1.5)

# Etiquetas
for (i in 1:nrow(df_est)) {
  text(df_est$lon[i], df_est$lat[i],
       labels=paste0(df_est$estacion[i], "\n(", round(df_est$pm25_mean[i],1), ")"),
       pos=4, cex=0.78, offset=0.6, font=2)
}

# Nombres de todas las alcaldias (pequeno)
for (j in 1:n_pol) {
  text(centros[j,1], centros[j,2], labels=cdmx_map$NAME_2[j],
       cex=0.55, col="grey35")
}

# Leyenda de colores
legend(x=-98.96, y=19.53,
       legend=labels,
       fill=plotclr, bty="n", cex=0.85, xpd=TRUE,
       title="PM2.5 (ug/m3)")

# Leyenda de puntos
legend(x=-98.96, y=19.35,
       legend="Estacion de monitoreo",
       pch=21, pt.bg="firebrick2", pt.cex=2.0, bty="n", cex=0.8, xpd=TRUE)

dev.off()
cat("\nGuardado: mapa_cdmx_completo.png\n")

# ============================================================
# 6. Tabla
# ============================================================
tab <- data.frame(
  estacion = df_est$estacion,
  pm25_mean = round(pm_vals, 2),
  alcaldia = cdmx_map$NAME_2[idx_alcaldias],
  rango = sapply(pm_vals, function(pm) labels[findInterval(pm, breaks, rightmost.closed=TRUE)])
)
write.csv(tab, file.path(imgdir, "mapa_cdmx_completo_tabla.csv"), row.names=FALSE)
cat("Tabla guardada: mapa_cdmx_completo_tabla.csv\n")
print(tab)
