options(repos="http://cran.itam.mx/")
library(rsinaica)
library(dplyr)

# Configuracion
out_dir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/raw"

# Catalogo de estaciones (segun plan revisado: ~4 por ciudad)
estaciones <- data.frame(
  ciudad = c(rep("cdmx",4), rep("gdl",4), rep("mty",4)),
  nombre = c("pedregal","merced","tlalnepantla","santafe",
             "oblatos","lomadorada","lasaguilas","miravalle",
             "obispado","sannicolas","apodaca","sanpedro"),
  station_id = c(259, 256, 266, 262,
                 107, 105, 103, 106,
                 141, 142, 146, 148),
  stringsAsFactors = FALSE
)

variables <- data.frame(
  codigo = c("PM2.5","TMP","HR"),
  nombre = c("pm25","temp","hr"),
  stringsAsFactors = FALSE
)

fecha_ini <- "2023-01-01"
fecha_fin <- "2023-12-31"
tipo_datos <- "Crude"  # datos crudos, mas disponibles

print("Iniciando descarga con rsinaica...")
print(paste("Total de combinaciones:", nrow(estaciones) * nrow(variables)))

fallidas <- list()
contador <- 0

for (i in 1:nrow(estaciones)) {
  est <- estaciones[i,]
  for (j in 1:nrow(variables)) {
    var <- variables[j,]
    contador <- contador + 1
    
    archivo <- file.path(out_dir, paste0(est$ciudad, "_", est$nombre, "_", var$nombre, "_2023.csv"))
    
    if (file.exists(archivo)) {
      print(paste0("[", contador, "/", nrow(estaciones)*nrow(variables), "] Ya existe: ", basename(archivo)))
      next
    }
    
    print(paste0("[", contador, "/", nrow(estaciones)*nrow(variables), "] Descargando: ", est$ciudad, " / ", est$nombre, " / ", var$codigo))
    
    tryCatch({
      df <- sinaica_station_data(
        station_id = est$station_id,
        parameter = var$codigo,
        start_date = fecha_ini,
        end_date = fecha_fin,
        type = tipo_datos,
        remove_extremes = FALSE
      )
      
      if (is.null(df) || nrow(df) == 0) {
        print(paste("  ADVERTENCIA: sin datos para", est$nombre, var$codigo))
        fallidas[[length(fallidas)+1]] <- list(ciudad=est$ciudad, estacion=est$nombre, variable=var$nombre, razon="sin datos")
      } else {
        write.csv(df, archivo, row.names=FALSE)
        print(paste("  OK:", nrow(df), "registros"))
      }
      
    }, error = function(e) {
      print(paste("  ERROR:", conditionMessage(e)))
      fallidas[[length(fallidas)+1]] <<- list(ciudad=est$ciudad, estacion=est$nombre, variable=var$nombre, razon=conditionMessage(e))
    })
    
    Sys.sleep(1.0)  # pausa educada
  }
}

# Reporte final
print("")
print("========== DESCARGA TERMINADA ==========")
if (length(fallidas) > 0) {
  print(paste("Descargas fallidas:", length(fallidas)))
  df_fall <- do.call(rbind, lapply(fallidas, as.data.frame))
  print(df_fall)
  write.csv(df_fall, file.path(out_dir, "descargas_fallidas.csv"), row.names=FALSE)
} else {
  print("Todas las descargas exitosas.")
}
