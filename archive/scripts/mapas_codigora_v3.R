options(repos="http://cran.itam.mx/")

library(dplyr)
library(terra)
library(classInt)
library(RColorBrewer)

# ============================================================
# MAPAS v3 — Estilo CodigoRA con cortes fijos de PM2.5
# ============================================================

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

imgdir <- file.path(wdir, "output/figures/")
dir.create(imgdir, recursive=TRUE, showWarnings=FALSE)

#--- Lectura de datos ---
df <- read.csv("data/clean/pm25_clean.csv", stringsAsFactors=FALSE)

df_est <- df %>%
  group_by(estacion, ciudad, lat, lon) %>%
  summarise(pm25_mean = mean(pm25, na.rm=TRUE), .groups="drop")

#--- Carga del shapefile municipal ---
mexico.map <- vect("C:/Users/pauli/Documents/RegresionAvanzada/CodigoRA/Mexico/shapes/")

#--- Cortes fijos de PM2.5 (comunes para todas las ciudades) ---
# Intervalos: [0,15), [15,20), [20,25), [25,30), [30,35), [35,Inf)
breaks <- c(0, 15, 20, 25, 30, 35, 100)
labels <- c("0-15","15-20","20-25","25-30","30-35",">35")
plotclr <- brewer.pal(length(labels), "YlOrRd")
# Si brewer.pal no da suficientes colores, interpolamos
if (length(plotclr) < length(labels)) {
  plotclr <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(labels))
}

get_color <- function(pm) {
  if (is.na(pm)) return("grey92")
  k <- findInterval(pm, breaks, rightmost.closed=TRUE)
  if (k < 1 || k > length(plotclr)) return("grey92")
  return(plotclr[k])
}

# ============================================================
# Funcion: mapa por ciudad con municipios coloreados (cortes fijos)
# ============================================================

mapa_ciudad_v3 <- function(ciudad_nombre, stl_id, titulo, xlim, ylim, estaciones_ciudad) {
  # Filtrar municipios del estado
  idx_estado <- mexico.map$`STL-1` == stl_id
  estado.map <- mexico.map[idx_estado, ]
  n_mun <- nrow(estado.map)
  
  # Inicializar: todos los municipios en gris claro
  colores <- rep("grey92", n_mun)
  
  # Para cada estacion, encontrar el municipio mas cercano
  idx_municipios <- integer(nrow(estaciones_ciudad))
  for (i in 1:nrow(estaciones_ciudad)) {
    est <- estaciones_ciudad[i, ]
    g <- geom(estado.map, wkt=FALSE)
    centros <- matrix(0, nrow=n_mun, ncol=2)
    for (j in 1:n_mun) {
      centros[j, ] <- apply(g[g[,1]==j, 3:4, drop=FALSE], 2, mean, na.rm=TRUE)
    }
    dists <- sqrt((centros[,1] - est$lon)^2 + (centros[,2] - est$lat)^2)
    idx_municipios[i] <- which.min(dists)
  }
  
  # Asignar colores fijos a los municipios con estacion
  pm_vals <- estaciones_ciudad$pm25_mean
  for (i in seq_along(pm_vals)) {
    colores[idx_municipios[i]] <- get_color(pm_vals[i])
  }
  
  # Guardar tabla
  tab_mun <- data.frame(
    estacion = estaciones_ciudad$estacion,
    pm25_mean = round(pm_vals, 2),
    municipio_color = colores[idx_municipios]
  )
  write.csv(tab_mun, file.path(imgdir, paste0("mapa_", ciudad_nombre, "_v3_tabla.csv")), row.names=FALSE)
  
  # Plot
  png(file.path(imgdir, paste0("mapa_", ciudad_nombre, "_v3.png")), width=900, height=750)
  par(mfrow=c(1,1), mar=c(4,4,4,1))
  
  plot(estado.map, fill=TRUE, col=colores, border="grey60", lwd=0.5,
       xlim=xlim, ylim=ylim,
       main=titulo, xlab="Longitud", ylab="Latitud", cex.main=1.05)
  
  # Puntos de estaciones (tamanos proporcionales a PM2.5)
  pm_range <- range(pm_vals)
  tamanos <- 1.8 + (pm_vals - pm_range[1]) / max(1, diff(pm_range)) * 2.8
  puntos_col <- c("cdmx"="firebrick2", "gdl"="steelblue", "mty"="forestgreen")[ciudad_nombre]
  
  points(estaciones_ciudad$lon, estaciones_ciudad$lat,
         pch=21, bg=puntos_col, col="black", cex=tamanos, lwd=1.5)
  
  # Etiquetas de estaciones
  text(estaciones_ciudad$lon, estaciones_ciudad$lat,
       labels=estaciones_ciudad$estacion, pos=3, cex=0.9, offset=0.5, font=2)
  
  # Leyenda de colores municipales (cortes fijos)
  legend("bottomright",
         legend=labels,
         fill=plotclr, bty="n", cex=0.8,
         title="PM2.5 por municipio (ug/m3)")
  
  # Leyenda de puntos
  legend("bottomleft",
         legend=paste0(estaciones_ciudad$estacion, " (", round(pm_vals, 1), ")"),
         pch=21, pt.bg=puntos_col, pt.cex=1.3, bty="n", cex=0.75,
         title="Estaciones (ug/m3)")
  
  dev.off()
  cat("Guardado: mapa_", ciudad_nombre, "_v3.png\n", sep="")
}

# ============================================================
# CDMX (STL-1 = 9)
# ============================================================
est_cdmx <- df_est[df_est$ciudad == "cdmx", ]
mapa_ciudad_v3("cdmx", 9,
               "CDMX — Valle de Mexico\nMunicipios con estaciones de monitoreo y PM2.5 promedio (2023)",
               xlim=c(-99.35, -98.95), ylim=c(19.15, 19.65),
               est_cdmx)

# ============================================================
# Guadalajara (STL-1 = 14)
# ============================================================
est_gdl <- df_est[df_est$ciudad == "gdl", ]
mapa_ciudad_v3("gdl", 14,
               "Jalisco — Zona Metropolitana de Guadalajara\nMunicipios con estaciones de monitoreo y PM2.5 promedio (2023)",
               xlim=c(-103.55, -103.20), ylim=c(20.55, 20.75),
               est_gdl)

# ============================================================
# Monterrey (STL-1 = 19)
# ============================================================
est_mty <- df_est[df_est$ciudad == "mty", ]
mapa_ciudad_v3("mty", 19,
               "Nuevo Leon — Zona Metropolitana de Monterrey\nMunicipios con estaciones de monitoreo y PM2.5 promedio (2023)",
               xlim=c(-100.55, -100.15), ylim=c(25.60, 25.82),
               est_mty)

# ============================================================
# Mapa nacional: estados con estaciones de monitoreo
# ============================================================
n_total <- nrow(mexico.map)
colores_nac <- rep("grey92", n_total)

idx_cdmx <- mexico.map$`STL-1` == 9
colores_nac[idx_cdmx] <- "#FC9272"  # CDMX: naranja claro (ya tiene datos)

idx_gdl <- mexico.map$`STL-1` == 14
colores_nac[idx_gdl] <- "#9ECAE1"  # Jalisco: azul claro

idx_mty <- mexico.map$`STL-1` == 19
colores_nac[idx_mty] <- "#A1D99B"  # Nuevo Leon: verde claro

# Centroides para etiquetas
centros_nac <- matrix(0, nrow=32, ncol=2)
for (s in 1:32) {
  idx_s <- mexico.map$`STL-1` == s
  if (sum(idx_s) > 0) {
    g_s <- geom(mexico.map[idx_s, ], wkt=FALSE)
    centros_nac[s, ] <- apply(g_s[,3:4], 2, mean, na.rm=TRUE)
  }
}

png(file.path(imgdir, "mapa_mexico_nacional_v3.png"), width=900, height=650)
par(mfrow=c(1,1), mar=c(3,3,4,1))
plot(mexico.map, fill=TRUE, col=colores_nac, border="grey50", lwd=0.4,
     main="Mexico — Estados con estaciones de monitoreo de calidad del aire (2023)",
     xlab="", ylab="", cex.main=1.1)

# Etiquetas de estados
lbls <- c("9"="CDMX","14"="Jalisco","19"="N.L.")
for (s in c(9,14,19)) {
  text(centros_nac[s,1], centros_nac[s,2],
       labels=lbls[as.character(s)], cex=0.9, font=2, col="black")
}

# Agregar puntos de estaciones (solo un punto por ciudad para no saturar)
ciudad_centro <- data.frame(
  lon = c(-99.13, -103.35, -100.31),
  lat = c(19.43, 20.67, 25.68),
  nombre = c("CDMX","GDL","MTY")
)
points(ciudad_centro$lon, ciudad_centro$lat, pch=23, bg=c("firebrick2","steelblue","forestgreen"),
       col="black", cex=2.0, lwd=1.5)

text(ciudad_centro$lon, ciudad_centro$lat, labels=ciudad_centro$nombre,
     pos=c(4,4,2), cex=0.85, offset=0.7, font=2)

legend("bottomleft",
       legend=c("CDMX (19.6 ug/m3)","Jalisco (26.3 ug/m3)","N.L. (20.7 ug/m3)"),
       fill=c("#FC9272","#9ECAE1","#A1D99B"), bty="n", cex=0.85,
       title="PM2.5 promedio estatal (2023)")
dev.off()
cat("Guardado: mapa_mexico_nacional_v3.png\n")

cat("\n=== Mapas v3 completados ===\n")
cat("Imagenes guardadas en:", imgdir, "\n")
