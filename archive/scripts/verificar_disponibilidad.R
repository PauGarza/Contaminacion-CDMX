options(repos="http://cran.itam.mx/")
library(rsinaica)

# Todas las estaciones de las 3 ciudades
cdmx_est <- stations_sinaica[stations_sinaica$network_id == 119, c('station_id','station_name')]
gdl_est <- stations_sinaica[stations_sinaica$network_id == 63, c('station_id','station_name')]
mty_est <- stations_sinaica[stations_sinaica$network_id == 72, c('station_id','station_name')]

verificar_pm25 <- function(df_est, nombre_ciudad) {
  resultados <- data.frame(station_id=integer(), station_name=character(), registros=integer(), stringsAsFactors=FALSE)
  for (i in 1:nrow(df_est)) {
    sid <- df_est$station_id[i]
    sname <- df_est$station_name[i]
    tryCatch({
      df <- sinaica_station_data(sid, "PM2.5", "2023-01-01", "2023-01-31", type="Crude")
      n <- ifelse(is.null(df), 0, nrow(df))
      resultados <- rbind(resultados, data.frame(station_id=sid, station_name=sname, registros=n))
      print(paste(nombre_ciudad, sname, "=", n, "registros (enero 2023)"))
    }, error = function(e) {
      resultados <- rbind(resultados, data.frame(station_id=sid, station_name=sname, registros=0))
      print(paste(nombre_ciudad, sname, "= ERROR"))
    })
    Sys.sleep(0.5)
  }
  return(resultados)
}

print("=== CDMX ===")
cdmx_res <- verificar_pm25(cdmx_est, "CDMX")

print("")
print("=== Guadalajara ===")
gdl_res <- verificar_pm25(gdl_est, "GDL")

print("")
print("=== Monterrey ===")
mty_res <- verificar_pm25(mty_est, "MTY")

# Guardar
write.csv(cdmx_res, "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/raw/disponibilidad_cdmx.csv", row.names=FALSE)
write.csv(gdl_res, "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/raw/disponibilidad_gdl.csv", row.names=FALSE)
write.csv(mty_res, "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/raw/disponibilidad_mty.csv", row.names=FALSE)

print("")
print("Estaciones CON datos de PM2.5 en enero 2023:")
print("CDMX:")
print(cdmx_res[cdmx_res$registros > 0,])
print("GDL:")
print(gdl_res[gdl_res$registros > 0,])
print("MTY:")
print(mty_res[mty_res$registros > 0,])
