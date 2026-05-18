options(repos="http://cran.itam.mx/")
library(rsinaica)
library(dplyr)
library(lubridate)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)

dir.create("data/raw", recursive=TRUE, showWarnings=FALSE)

# ============================================================
# Descarga masiva — 56 estaciones, mes por mes (API limite = 1 mes)
# ============================================================

est <- read.csv("data/raw/estaciones_territorio_valle.csv", stringsAsFactors=FALSE)
cat("Estaciones a descargar:", nrow(est), "\n")

VARS <- c("PM2.5"="PM2.5", "temp"="TMP", "hr"="HR")

# Contadores
n_existen <- 0
n_descargados <- 0
n_sin_datos <- 0
n_errores <- 0

for (i in 1:nrow(est)) {
  sid <- est$station_id[i]
  sname_raw <- est$station_name[i]
  # Limpiar nombre para archivo
  sname <- tolower(gsub("[ \\.]", "", sname_raw))
  sname <- iconv(sname, to="ASCII//TRANSLIT")
  # Prefijo segun estado
  prefijo <- ifelse(est$estado[i] == "Distrito Federal", "cdmx",
             ifelse(est$estado[i] == "México", "edomex", "hidalgo"))
  
  for (vname in names(VARS)) {
    vcode <- VARS[vname]
    fname <- file.path("data/raw", paste0(prefijo, "_", sname, "_", vname, "_2023.csv"))
    
    if (file.exists(fname)) {
      cat("[", i, "/", nrow(est), "] YA EXISTE:", basename(fname), "\n")
      n_existen <- n_existen + 1
      next
    }
    
    cat("[", i, "/", nrow(est), "] Descargando:", sname_raw, "-", vname, "(id=", sid, ")\n")
    
    chunk_list <- list()
    for (mes in 1:12) {
      ini <- as.Date(paste0("2023-", sprintf("%02d", mes), "-01"))
      fin <- as.Date(paste0("2023-", sprintf("%02d", mes), "-", days_in_month(ini)))
      
      tryCatch({
        chunk <- sinaica_station_data(station_id=as.integer(sid), parameter=as.character(vcode),
                                      start_date=ini, end_date=fin)
        if (!is.null(chunk) && nrow(chunk) > 0) {
          chunk_list[[length(chunk_list)+1]] <- chunk
          cat("  Mes", mes, ":", nrow(chunk), "registros\n")
        } else {
          cat("  Mes", mes, ": sin datos\n")
        }
      }, error=function(e) {
        cat("  Mes", mes, ": ERROR -", conditionMessage(e), "\n")
        n_errores <<- n_errores + 1
      })
      
      Sys.sleep(0.5)
    }
    
    if (length(chunk_list) > 0) {
      datos <- bind_rows(chunk_list)
      write.csv(datos, fname, row.names=FALSE)
      cat("  -> GUARDADO:", basename(fname), "(", nrow(datos), "filas )\n")
      n_descargados <- n_descargados + 1
    } else {
      cat("  -> SIN DATOS para esta estacion/variable\n")
      n_sin_datos <- n_sin_datos + 1
    }
  }
}

cat("\n=== RESUMEN DESCARGA ===\n")
cat("Ya existian:", n_existen, "\n")
cat("Descargados:", n_descargados, "\n")
cat("Sin datos:", n_sin_datos, "\n")
cat("Errores:", n_errores, "\n")
cat("========================\n")
