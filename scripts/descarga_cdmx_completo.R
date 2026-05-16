options(repos="http://cran.itam.mx/")
library(rsinaica)
library(dplyr)
library(lubridate)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

dir.create("data/raw", recursive=TRUE, showWarnings=FALSE)

# ============================================================
# 1. Leer disponibilidad y filtrar estaciones con datos
# ============================================================
disp <- read.csv("data/raw/disponibilidad_cdmx.csv", stringsAsFactors=FALSE)

# Estaciones con al menos 300 registros (PM2.5 disponible)
est_con_datos <- disp %>%
  filter(registros >= 300) %>%
  arrange(desc(registros))

cat("Estaciones CDMX con datos (>=300 registros):\n")
print(est_con_datos)

# Excluimos Tlalnepantla (esta en Edomex) y FES Aragon (tambien Edomex)
est_cdmx <- est_con_datos %>%
  filter(!station_name %in% c("Tlalnepantla", "FES Aragón"))

cat("\nEstaciones a descargar (solo CDMX):\n")
print(est_cdmx)

# ============================================================
# 2. Descargar datos por estacion y variable
# ============================================================
VARS <- c("PM2.5"="PM2.5", "temp"="TMP", "hr"="HR")

for (i in 1:nrow(est_cdmx)) {
  sid <- est_cdmx$station_id[i]
  sname <- tolower(gsub("[ \\.]", "", est_cdmx$station_name[i]))
  sname <- iconv(sname, to="ASCII//TRANSLIT")
  
  for (vname in names(VARS)) {
    vcode <- VARS[vname]
    fname <- file.path("data/raw", paste0("cdmx_completo_", sname, "_", vname, "_2023.csv"))
    
    if (file.exists(fname)) {
      cat("Ya existe:", basename(fname), "\n")
      next
    }
    
    cat("Descargando:", est_cdmx$station_name[i], "-", vname, "(id=", sid, ")\n")
    
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
      })
      
      Sys.sleep(0.5)
    }
    
    if (length(chunk_list) > 0) {
      datos <- bind_rows(chunk_list)
      write.csv(datos, fname, row.names=FALSE)
      cat("  -> Guardado:", basename(fname), "(", nrow(datos), "filas )\n")
    } else {
      cat("  -> Sin datos para esta estacion/variable\n")
    }
  }
}

cat("\n=== Descarga completada ===\n")
