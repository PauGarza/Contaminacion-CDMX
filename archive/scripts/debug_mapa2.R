options(repos="http://cran.itam.mx/")
library(terra)
library(dplyr)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

mexico.map <- vect("C:/Users/pauli/Documents/RegresionAvanzada/CodigoRA/Mexico/shapes/")
df <- read.csv("data/clean/pm25_clean.csv", stringsAsFactors=FALSE)
df_est <- df %>% group_by(estacion, ciudad, lat, lon) %>% summarise(pm25_mean=mean(pm25,na.rm=T),.groups="drop")

depura <- function(ciudad_nombre, stl_id) {
  cat("\n===", ciudad_nombre, "(STL-1 =", stl_id, ") ===\n")
  idx_estado <- mexico.map$`STL-1` == stl_id
  estado.map <- mexico.map[idx_estado, ]
  n_mun <- nrow(estado.map)
  cat("Numero de poligonos:", n_mun, "\n")
  
  est <- df_est[df_est$ciudad == ciudad_nombre, ]
  cat("Estaciones:\n")
  print(est)
  
  if (n_mun == 0) return()
  
  g <- geom(estado.map, wkt=FALSE)
  centros <- matrix(0, nrow=n_mun, ncol=2)
  for (j in 1:n_mun) {
    centros[j, ] <- apply(g[g[,1]==j, 3:4, drop=FALSE], 2, mean, na.rm=TRUE)
  }
  
  idx_mun <- integer(nrow(est))
  for (i in 1:nrow(est)) {
    dists <- sqrt((centros[,1] - est$lon[i])^2 + (centros[,2] - est$lat[i])^2)
    idx_mun[i] <- which.min(dists)
    cat("Estacion", est$estacion[i], "-> poligono", idx_mun[i], 
        "muni:", estado.map$ADM1[idx_mun[i]], 
        "dist:", round(min(dists),4), "\n")
  }
  
  cat("Indices unicos:", sort(unique(idx_mun)), "\n")
  cat("Rango lon poligonos:", round(range(centros[,1]),4), "\n")
  cat("Rango lat poligonos:", round(range(centros[,2]),4), "\n")
}

depura("cdmx", 9)
depura("gdl", 14)
depura("mty", 19)
