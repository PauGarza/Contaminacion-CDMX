options(repos="http://cran.itam.mx/")
library(rsinaica)
library(dplyr)

out_dir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/raw"

# Catalogo de estaciones CON PM2.5 disponible en 2023
estaciones <- data.frame(
  ciudad = c(rep("cdmx",4), rep("gdl",4), rep("mty",4)),
  nombre = c("pedregal","merced","tlalnepantla","benitojuarez",
             "centro","lasaguilas","lomadorada","miravalle",
             "obispado","sannicolas","apodaca","sanpedro"),
  station_id = c(259, 256, 266, 300,
                 102, 103, 105, 106,
                 141, 142, 146, 148),
  stringsAsFactors = FALSE
)

variables <- data.frame(
  codigo = c("PM2.5","TMP","HR"),
  nombre = c("pm25","temp","hr"),
  stringsAsFactors = FALSE
)

# Generar rangos mensuales para 2023
meses_ini <- as.Date(paste0("2023-", sprintf("%02d", 1:12), "-01"))
meses_fin <- c(meses_ini[2:12] - 1, as.Date("2023-12-31"))

print(paste("Total de peticiones:", nrow(estaciones) * nrow(variables) * 12))

fallidas <- list()
contador <- 0

for (i in 1:nrow(estaciones)) {
  est <- estaciones[i,]
  for (j in 1:nrow(variables)) {
    var <- variables[j,]
    
    archivo_final <- file.path(out_dir, paste0(est$ciudad, "_", est$nombre, "_", var$nombre, "_2023.csv"))
    
    if (file.exists(archivo_final)) {
      print(paste0("Ya existe: ", basename(archivo_final)))
      next
    }
    
    datos_mes <- list()
    
    for (m in 1:12) {
      contador <- contador + 1
      fi <- as.character(meses_ini[m])
      ff <- as.character(meses_fin[m])
      
      print(paste0("[", contador, "] ", est$ciudad, "/", est$nombre, "/", var$codigo, " ", fi, " a ", ff))
      
      tryCatch({
        df_mes <- sinaica_station_data(
          station_id = est$station_id,
          parameter = var$codigo,
          start_date = fi,
          end_date = ff,
          type = "Crude",
          remove_extremes = FALSE
        )
        
        if (!is.null(df_mes) && nrow(df_mes) > 0) {
          datos_mes[[length(datos_mes)+1]] <- df_mes
          print(paste("  OK:", nrow(df_mes), "registros"))
        } else {
          print("  Sin datos")
        }
        
      }, error = function(e) {
        msg <- conditionMessage(e)
        print(paste("  ERROR:", msg))
        fallidas[[length(fallidas)+1]] <<- list(ciudad=est$ciudad, estacion=est$nombre, variable=var$nombre, mes=m, razon=msg)
      })
      
      Sys.sleep(0.8)
    }
    
    # Concatenar y guardar
    if (length(datos_mes) > 0) {
      df_total <- bind_rows(datos_mes)
      write.csv(df_total, archivo_final, row.names=FALSE)
      print(paste("  GUARDADO:", basename(archivo_final), "=", nrow(df_total), "registros totales"))
    } else {
      print(paste("  SIN DATOS EN NINGUN MES:", basename(archivo_final)))
    }
  }
}

print("")
print("========== DESCARGA TERMINADA ==========")
if (length(fallidas) > 0) {
  print(paste("Peticiones fallidas:", length(fallidas)))
  df_fall <- do.call(rbind, lapply(fallidas, as.data.frame))
  write.csv(df_fall, file.path(out_dir, "descargas_fallidas.csv"), row.names=FALSE)
} else {
  print("Todas las descargas exitosas.")
}
