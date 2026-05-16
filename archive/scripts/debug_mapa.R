options(repos="http://cran.itam.mx/")
library(terra)
library(dplyr)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

mexico.map <- vect("C:/Users/pauli/Documents/RegresionAvanzada/CodigoRA/Mexico/shapes/")

# Filtrar CDMX
idx_estado <- mexico.map$`STL-1` == 9
estado.map <- mexico.map[idx_estado, ]

n_mun <- nrow(estado.map)
cat("Numero de poligonos en CDMX:", n_mun, "\n")
cat("Nombres de municipios (primeros 20):\n")
print(head(estado.map$ADM1, 20))

# Coordenadas estaciones CDMX
df <- read.csv("data/clean/pm25_clean.csv", stringsAsFactors=FALSE)
df_est <- df %>% group_by(estacion, ciudad, lat, lon) %>% summarise(pm25_mean=mean(pm25,na.rm=T),.groups="drop")
est_cdmx <- df_est[df_est$ciudad == "cdmx", ]
print(est_cdmx)

# Centros de poligonos
g <- geom(estado.map, wkt=FALSE)
centros <- matrix(0, nrow=n_mun, ncol=2)
for (j in 1:n_mun) {
  centros[j, ] <- apply(g[g[,1]==j, 3:4, drop=FALSE], 2, mean, na.rm=TRUE)
}

# Encontrar municipio mas cercano para cada estacion
idx_municipios <- integer(nrow(est_cdmx))
for (i in 1:nrow(est_cdmx)) {
  est <- est_cdmx[i, ]
  dists <- sqrt((centros[,1] - est$lon)^2 + (centros[,2] - est$lat)^2)
  idx_municipios[i] <- which.min(dists)
  cat("Estacion:", est$estacion, "-> poligono", idx_municipios[i], 
      "municipio:", estado.map$ADM1[idx_municipios[i]], 
      "dist:", min(dists), "\n")
}

cat("\nIndices unicos:", unique(idx_municipios), "\n")
cat("Colores asignados (4 estaciones):", length(idx_municipios), "\n")
cat("Total poligonos:", n_mun, "\n")
