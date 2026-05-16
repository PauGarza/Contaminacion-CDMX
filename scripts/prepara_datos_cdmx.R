options(repos="http://cran.itam.mx/")
library(dplyr)
library(terra)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

# ============================================================
# Preparar datos para JAGS (CDMX completo)
# ============================================================
df <- read.csv("data/clean/pm25_cdmx_completo.csv", stringsAsFactors=FALSE)

#--- 1. Asignar alcaldia a cada observacion ---
mex_mun <- vect("data/gadm_mexico/gadm41_MEX_2.shp")
cdmx_map <- mex_mun[mex_mun$NAME_1 == "Distrito Federal", ]
n_alc <- nrow(cdmx_map)

# Centros de alcaldias
g <- geom(cdmx_map, wkt=FALSE)
centros <- matrix(0, nrow=n_alc, ncol=2)
for (j in 1:n_alc) {
  centros[j, ] <- apply(g[g[,1]==j, 3:4, drop=FALSE], 2, mean, na.rm=TRUE)
}

# Tabla de estaciones con su alcaldia
est_coords <- df %>%
  group_by(estacion, lat, lon) %>%
  summarise(.groups="drop")

est_alc <- data.frame(
  estacion = est_coords$estacion,
  lat = est_coords$lat,
  lon = est_coords$lon,
  alcaldia = character(nrow(est_coords)),
  idx_alc = integer(nrow(est_coords)),
  stringsAsFactors=FALSE
)

for (i in 1:nrow(est_coords)) {
  dists <- sqrt((centros[,1] - est_coords$lon[i])^2 + (centros[,2] - est_coords$lat[i])^2)
  est_alc$idx_alc[i] <- which.min(dists)
  est_alc$alcaldia[i] <- cdmx_map$NAME_2[which.min(dists)]
}

cat("Estaciones y sus alcaldias:\n")
print(est_alc)

# Merge con df
df <- df %>%
  left_join(est_alc[, c("estacion","alcaldia","idx_alc")], by="estacion")

#--- 2. Crear indices para JAGS ---
df$idx_estacion <- as.integer(factor(df$estacion))
df$idx_alc <- as.integer(factor(df$alcaldia))

# Ordenar por estacion y fecha
df <- df %>% arrange(estacion, date)

#--- 3. Estandarizar covariables ---
df$temp_s <- scale(df$temp)[,1]
df$hr_s   <- scale(df$hr)[,1]

#--- 4. Variables para modelos ---
n <- nrow(df)
n_est <- length(unique(df$estacion))
n_alc <- length(unique(df$alcaldia))

cat("\n=== Resumen para JAGS ===\n")
cat("Observaciones (n):", n, "\n")
cat("Estaciones (n_est):", n_est, "\n")
cat("Alcaldias (n_alc):", n_alc, "\n")

#--- 5. Guardar datos procesados ---
write.csv(df, "data/clean/pm25_cdmx_jags.csv", row.names=FALSE)
write.csv(est_alc, "data/clean/estaciones_alcaldias_cdmx.csv", row.names=FALSE)

cat("\nGuardado: data/clean/pm25_cdmx_jags.csv\n")
cat("Guardado: data/clean/estaciones_alcaldias_cdmx.csv\n")
