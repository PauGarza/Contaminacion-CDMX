options(repos="http://cran.itam.mx/")

library(dplyr)
library(terra)
library(classInt)
library(RColorBrewer)

# ============================================================
# MAPAS ESTILO CODIGORA — Shapefile municipal coloreado
# ============================================================

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

imgdir <- file.path(wdir, "output/figures/")
dir.create(imgdir, recursive=TRUE, showWarnings=FALSE)

#--- Lectura de datos ---
df <- read.csv("data/clean/pm25_clean.csv", stringsAsFactors=FALSE)

# Promedio por estacion
df_est <- df %>%
  group_by(estacion, ciudad, lat, lon) %>%
  summarise(pm25_mean = mean(pm25, na.rm=TRUE), .groups="drop")

#--- Carga del shapefile municipal de Mexico ---
mexico.map <- vect("C:/Users/pauli/Documents/RegresionAvanzada/CodigoRA/Mexico/shapes/")

# Paleta de colores
pal <- c("cdmx"="firebrick2", "gdl"="steelblue", "mty"="forestgreen")

# ============================================================
# Funcion auxiliar: crear mapa por ciudad
# ============================================================

mapa_ciudad <- function(ciudad_nombre, stl_id, titulo, xlim, ylim, estaciones_ciudad) {
  # Filtrar municipios del estado
  idx_estado <- mexico.map$`STL-1` == stl_id
  estado.map <- mexico.map[idx_estado, ]
  
  # Color base: gris claro para municipios sin estacion
  n_mun <- nrow(estado.map)
  colores <- rep("grey90", n_mun)
  
  # Para cada estacion, encontrar el municipio mas cercano y colorear
  for (i in 1:nrow(estaciones_ciudad)) {
    est <- estaciones_ciudad[i, ]
    # Calcular distancia euclidiana a cada municipio
    centros <- geom(estado.map, wkt=FALSE)[, c("x", "y")]
    dists <- sqrt((centros[,1] - est$lon)^2 + (centros[,2] - est$lat)^2)
    idx_mun <- which.min(dists)
    
    # Color basado en PM2.5 (escala de calor)
    pm <- est$pm25_mean
    # Usar paleta RColorBrewer: cuanto mayor PM2.5, mas oscuro
    colores[idx_mun] <- "firebrick2"
  }
  
  # Plot
  png(file.path(imgdir, paste0("mapa_", ciudad_nombre, ".png")), width=800, height=700)
  par(mfrow=c(1,1), mar=c(4,4,3,1))
  plot(estado.map, fill=TRUE, col=colores, border="grey40",
       xlim=xlim, ylim=ylim,
       main=titulo, xlab="Longitud", ylab="Latitud")
  
  # Puntos de estaciones (tamaño proporcional a PM2.5)
  tamanos <- 1.5 + (estaciones_ciudad$pm25_mean - min(estaciones_ciudad$pm25_mean)) /
    (max(estaciones_ciudad$pm25_mean) - min(estaciones_ciudad$pm25_mean)) * 2.5
  points(estaciones_ciudad$lon, estaciones_ciudad$lat, 
         pch=21, bg=pal[ciudad_nombre], col="black", cex=tamanos)
  
  # Etiquetas de estaciones
  text(estaciones_ciudad$lon, estaciones_ciudad$lat, 
       labels=estaciones_ciudad$estacion, pos=3, cex=0.8, offset=0.5, font=2)
  
  # Leyenda
  legend("bottomright", 
         legend=paste0(estaciones_ciudad$estacion, " (", round(estaciones_ciudad$pm25_mean,1), ")"),
         pch=21, pt.bg=pal[ciudad_nombre], pt.cex=1.5, bty="n", cex=0.7, title="PM2.5 (ug/m3)")
  
  dev.off()
  cat("Guardado: mapa_", ciudad_nombre, ".png\n", sep="")
}

# ============================================================
# Mapa 1: CDMX (STL-1 = 9)
# ============================================================
est_cdmx <- df_est[df_est$ciudad == "cdmx", ]
mapa_ciudad("cdmx", 9, "CDMX — Valle de Mexico\nEstaciones de monitoreo y PM2.5 promedio (2023)",
            xlim=c(-99.35, -98.95), ylim=c(19.15, 19.65), est_cdmx)

# ============================================================
# Mapa 2: Guadalajara (STL-1 = 14)
# ============================================================
est_gdl <- df_est[df_est$ciudad == "gdl", ]
mapa_ciudad("gdl", 14, "Jalisco — Guadalajara\nEstaciones de monitoreo y PM2.5 promedio (2023)",
            xlim=c(-103.55, -103.20), ylim=c(20.55, 20.75), est_gdl)

# ============================================================
# Mapa 3: Monterrey (STL-1 = 19)
# ============================================================
est_mty <- df_est[df_est$ciudad == "mty", ]
mapa_ciudad("mty", 19, "Nuevo Leon — Monterrey\nEstaciones de monitoreo y PM2.5 promedio (2023)",
            xlim=c(-100.55, -100.15), ylim=c(25.60, 25.82), est_mty)

# ============================================================
# Mapa 4: Mexico nacional con los 3 estados coloreados
# ============================================================

# Colorear los 3 estados donde hay estaciones
colores_nac <- rep("grey90", nrow(mexico.map))
# CDMX (9) = firebrick2
colores_nac[mexico.map$`STL-1` == 9] <- "firebrick2"
# Jalisco (14) = steelblue
colores_nac[mexico.map$`STL-1` == 14] <- "steelblue"
# Nuevo Leon (19) = forestgreen
colores_nac[mexico.map$`STL-1` == 19] <- "forestgreen"

png(file.path(imgdir, "mapa_mexico_nacional.png"), width=800, height=600)
par(mfrow=c(1,1), mar=c(3,3,3,1))
plot(mexico.map, fill=TRUE, col=colores_nac, border="grey40",
     main="Mexico — Estados con estaciones de monitoreo (2023)", xlab="", ylab="")
legend("bottomleft", legend=c("CDMX","Jalisco","Nuevo Leon"),
       fill=c("firebrick2","steelblue","forestgreen"), bty="n", cex=0.9)
dev.off()
cat("Guardado: mapa_mexico_nacional.png\n")

cat("\n=== Mapas completados ===\n")
cat("Imagenes guardadas en:", imgdir, "\n")
