options(repos="http://cran.itam.mx/")

library(dplyr)
library(terra)
library(classInt)
library(RColorBrewer)

# ============================================================
# MAPAS v2 — Estilo CodigoRA con municipios coloreados
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

# ============================================================
# Funcion: mapa por ciudad con municipios coloreados
# ============================================================

mapa_ciudad_v2 <- function(ciudad_nombre, stl_id, titulo, xlim, ylim, estaciones_ciudad, paleta) {
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
    # Centros de los poligonos
    g <- geom(estado.map, wkt=FALSE)
    centros <- matrix(0, nrow=n_mun, ncol=2)
    for (j in 1:n_mun) {
      centros[j, ] <- apply(g[g[,1]==j, 3:4, drop=FALSE], 2, mean, na.rm=TRUE)
    }
    dists <- sqrt((centros[,1] - est$lon)^2 + (centros[,2] - est$lat)^2)
    idx_municipios[i] <- which.min(dists)
  }
  
  # Asignar colores graduados a los municipios con estacion
  pm_vals <- estaciones_ciudad$pm25_mean
  nclr <- min(5, length(pm_vals))
  plotclr <- brewer.pal(nclr, paleta)
  
  if (length(pm_vals) > 1) {
    class <- classIntervals(pm_vals, nclr, style="quantile")
    colcode <- findColours(class, plotclr)
    colores[idx_municipios] <- as.character(colcode)
  } else {
    colores[idx_municipios] <- plotclr[3]
  }
  
  # Guardar tabla de municipios coloreados
  rango_txt <- if (length(pm_vals) > 1) names(attr(colcode, "table")) else "unico"
  if (is.null(rango_txt) || length(rango_txt) == 0) rango_txt <- rep("definido", length(pm_vals))
  tab_mun <- data.frame(
    estacion = estaciones_ciudad$estacion,
    pm25_mean = round(pm_vals, 2),
    color = colores[idx_municipios],
    rango = rango_txt
  )
  write.csv(tab_mun, file.path(imgdir, paste0("mapa_", ciudad_nombre, "_tabla.csv")), row.names=FALSE)
  
  # Plot
  png(file.path(imgdir, paste0("mapa_", ciudad_nombre, "_v2.png")), width=900, height=750)
  par(mfrow=c(1,1), mar=c(4,4,4,1))
  
  plot(estado.map, fill=TRUE, col=colores, border="grey50", lwd=0.6,
       xlim=xlim, ylim=ylim,
       main=titulo, xlab="Longitud", ylab="Latitud", cex.main=1.1)
  
  # Puntos de estaciones (tamanos proporcionales a PM2.5)
  tamanos <- 2.0 + (pm_vals - min(pm_vals)) / max(1, (max(pm_vals) - min(pm_vals))) * 2.5
  puntos_col <- c("cdmx"="firebrick2", "gdl"="steelblue", "mty"="forestgreen")[ciudad_nombre]
  points(estaciones_ciudad$lon, estaciones_ciudad$lat,
         pch=21, bg=puntos_col, col="black", cex=tamanos, lwd=1.2)
  
  # Etiquetas de estaciones
  text(estaciones_ciudad$lon, estaciones_ciudad$lat,
       labels=estaciones_ciudad$estacion, pos=3, cex=0.85, offset=0.6, font=2)
  
  # Leyenda de colores municipales
  if (length(pm_vals) > 1) {
    legend("bottomright",
           legend=names(attr(colcode, "table")),
           fill=attr(colcode, "palette"), bty="n", cex=0.75,
           title="PM2.5 por municipio (ug/m3)")
  }
  
  # Leyenda de puntos
  legend("bottomleft",
         legend=paste0(estaciones_ciudad$estacion, " (", round(pm_vals, 1), ")"),
         pch=21, pt.bg=puntos_col, pt.cex=1.3, bty="n", cex=0.7,
         title="Estaciones (ug/m3)")
  
  dev.off()
  cat("Guardado: mapa_", ciudad_nombre, "_v2.png\n", sep="")
}

# ============================================================
# CDMX (STL-1 = 9)
# ============================================================
est_cdmx <- df_est[df_est$ciudad == "cdmx", ]
mapa_ciudad_v2("cdmx", 9,
               "CDMX — Valle de Mexico\nMunicipios con estaciones de monitoreo y PM2.5 promedio (2023)",
               xlim=c(-99.35, -98.95), ylim=c(19.15, 19.65),
               est_cdmx, "Reds")

# ============================================================
# Guadalajara (STL-1 = 14)
# ============================================================
est_gdl <- df_est[df_est$ciudad == "gdl", ]
mapa_ciudad_v2("gdl", 14,
               "Jalisco — Zona Metropolitana de Guadalajara\nMunicipios con estaciones de monitoreo y PM2.5 promedio (2023)",
               xlim=c(-103.55, -103.20), ylim=c(20.55, 20.75),
               est_gdl, "Blues")

# ============================================================
# Monterrey (STL-1 = 19)
# ============================================================
est_mty <- df_est[df_est$ciudad == "mty", ]
mapa_ciudad_v2("mty", 19,
               "Nuevo Leon — Zona Metropolitana de Monterrey\nMunicipios con estaciones de monitoreo y PM2.5 promedio (2023)",
               xlim=c(-100.55, -100.15), ylim=c(25.60, 25.82),
               est_mty, "Greens")

# ============================================================
# Mapa nacional: choropleth de PM2.5 por estado
# ============================================================

# Calcular PM2.5 promedio por estado (ponderado por numero de observaciones)
pm_estado <- df %>%
  group_by(ciudad) %>%
  summarise(pm25_mean = mean(pm25, na.rm=TRUE), .groups="drop")

# Asignar STL-1
stl_map <- c("cdmx"=9, "gdl"=14, "mty"=19)
pm_estado$stl <- stl_map[pm_estado$ciudad]

# Colorear todos los municipios: gris base, y los estados con estacion en color
n_total <- nrow(mexico.map)
colores_nac <- rep("grey92", n_total)

# CDMX: rojo
idx_cdmx <- mexico.map$`STL-1` == 9
colores_nac[idx_cdmx] <- "#DE2D26"  # rojo fuerte

# Jalisco: azul
idx_gdl <- mexico.map$`STL-1` == 14
colores_nac[idx_gdl] <- "#3182BD"  # azul fuerte

# Nuevo Leon: verde
idx_mty <- mexico.map$`STL-1` == 19
colores_nac[idx_mty] <- "#31A354"  # verde fuerte

# Centroides para etiquetas
centros_nac <- matrix(0, nrow=32, ncol=2)
for (s in 1:32) {
  idx_s <- mexico.map$`STL-1` == s
  if (sum(idx_s) > 0) {
    g_s <- geom(mexico.map[idx_s, ], wkt=FALSE)
    centros_nac[s, ] <- apply(g_s[,3:4], 2, mean, na.rm=TRUE)
  }
}

png(file.path(imgdir, "mapa_mexico_nacional_v2.png"), width=900, height=650)
par(mfrow=c(1,1), mar=c(3,3,4,1))
plot(mexico.map, fill=TRUE, col=colores_nac, border="grey40", lwd=0.5,
     main="Mexico — Estados con estaciones de monitoreo de calidad del aire (2023)",
     xlab="", ylab="", cex.main=1.2)

# Etiquetas de estados
for (s in c(9,14,19)) {
  text(centros_nac[s,1], centros_nac[s,2],
       labels=c("9"="CDMX","14"="Jalisco","19"="N.L.")[as.character(s)],
       cex=0.9, font=2, col="white")
}

legend("bottomleft",
       legend=c("CDMX (19.6)","Jalisco (26.3)","Nuevo Leon (20.7)"),
       fill=c("#DE2D26","#3182BD","#31A354"), bty="n", cex=0.85,
       title="PM2.5 promedio (ug/m3)")
dev.off()
cat("Guardado: mapa_mexico_nacional_v2.png\n")

cat("\n=== Mapas v2 completados ===\n")
cat("Imagenes guardadas en:", imgdir, "\n")
