options(repos="http://cran.itam.mx/")
library(rsinaica)

# Obtener catalogo completo de estaciones
cat("Descargando catalogo de estaciones SINAICA...\n")
catalogo <- stations_sinaica()
cat("Total estaciones:", nrow(catalogo), "\n")
cat("Columnas:", names(catalogo), "\n")

# Ver primeras filas
print(head(catalogo, 10))

# Filtrar por CDMX: ver como se identifica el estado
# Buscar patrones comunes
cat("\n=== Estados unicos (muestra) ===\n")
print(sort(unique(catalogo$state_name))[1:20])

# Filtrar CDMX
cdmx_est <- catalogo[grepl("Ciudad|Distrito|Federal|CDMX|DF", catalogo$state_name, ignore.case=TRUE), ]
cat("\nEstaciones en CDMX:", nrow(cdmx_est), "\n")
print(cdmx_est[, c("station_id", "station_name", "network_name", "lat", "lon", "state_name")])

# Ver que parametros tiene cada estacion
cat("\n=== Parametros disponibles en CDMX ===\n")
for (sid in unique(cdmx_est$station_id)[1:10]) {
  params <- sinaica_station_params(sid)
  cat("Estacion", sid, ":", paste(params$param_code, collapse=", "), "\n")
}

write.csv(cdmx_est, "data/estaciones_cdmx_catalogo.csv", row.names=FALSE)
