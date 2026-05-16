options(repos="http://cran.itam.mx/")
library(dplyr)
library(lubridate)
library(tidyr)

# ============================================================
# LIMPIEZA Y UNION DE DATOS SINAICA
# ============================================================

raw_dir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/raw"
clean_dir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/clean"
out_dir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/output/figures"

dir.create(clean_dir, recursive=TRUE, showWarnings=FALSE)
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)

# Catalogo de estaciones con coordenadas (VERIFICADAS con rsinaica)
estaciones_info <- data.frame(
  ciudad = c(rep("cdmx",4), rep("gdl",3), rep("mty",4)),
  nombre_archivo = c("pedregal","merced","tlalnepantla","benitojuarez",
                     "centro","lasaguilas","miravalle",
                     "obispado","sannicolas","apodaca","sanpedro"),
  station_id = c(259, 256, 266, 300,
                 102, 103, 106,
                 141, 142, 146, 148),
  lat = c(19.32528, 19.42472, 19.52917, 19.37167,
          20.67376, 20.63127, 20.61444,
          25.67598, 25.74499, 25.77722, 25.66528),
  lon = c(-99.20417, -99.11972, -99.20472, -99.15917,
          -103.33334, -103.41643, -103.34333,
          -100.33838, -100.25309, -100.18833, -100.41278),
  stringsAsFactors = FALSE
)

variables <- c("pm25","temp","hr")

# Funcion para leer y promediar un archivo a diario
procesar_archivo <- function(ciudad, estacion, variable) {
  archivo <- file.path(raw_dir, paste0(ciudad, "_", estacion, "_", variable, "_2023.csv"))
  
  if (!file.exists(archivo)) {
    warning(paste("No existe:", archivo))
    return(NULL)
  }
  
  df <- read.csv(archivo, stringsAsFactors=FALSE)
  
  # Filtrar solo validos (valid == 1) y valores no negativos
  df <- df %>% 
    filter(valid == 1, !is.na(value), value >= 0)
  
  # Convertir fecha
  df$date <- as.Date(df$date)
  
  # Promedio diario
  df_dia <- df %>%
    group_by(date) %>%
    summarise(valor = mean(value, na.rm=TRUE), .groups="drop")
  
  df_dia$ciudad <- ciudad
  df_dia$estacion <- estacion
  df_dia$variable <- variable
  
  return(df_dia)
}

# ============================================================
# Leer todos los archivos
# ============================================================

datos_lista <- list()

for (i in 1:nrow(estaciones_info)) {
  est <- estaciones_info[i,]
  for (var in variables) {
    df <- procesar_archivo(est$ciudad, est$nombre_archivo, var)
    if (!is.null(df)) {
      datos_lista[[length(datos_lista)+1]] <- df
    }
  }
}

# Combinar
datos_todos <- bind_rows(datos_lista)

# Verificar que tenemos datos
if (nrow(datos_todos) == 0) {
  stop("No se encontraron datos. Verifica que los archivos CSV esten en data/raw/")
}

print(paste("Total registros leidos:", nrow(datos_todos)))

# ============================================================
# Pivotar a formato ancho (una fila por dia-estacion)
# ============================================================

datos_wide <- datos_todos %>%
  pivot_wider(
    id_cols = c(date, ciudad, estacion),
    names_from = variable,
    values_from = valor
  )

# Agregar coordenadas
datos_wide <- datos_wide %>%
  left_join(estaciones_info, by=c("ciudad","estacion"="nombre_archivo"))

# ============================================================
# Variables temporales
# ============================================================

datos_wide <- datos_wide %>%
  mutate(
    dia_año = yday(date),
    mes = month(date),
    dia_semana = wday(date, label=TRUE, abbr=TRUE),
    sen_t = sin(2 * pi * dia_año / 365),
    cos_t = cos(2 * pi * dia_año / 365)
  )

# ============================================================
# Filtros de calidad
# ============================================================

print("Registros antes de filtros:")
print(nrow(datos_wide))

datos_wide <- datos_wide %>%
  filter(
    is.na(pm25) | (pm25 >= 0 & pm25 <= 500),
    is.na(temp) | (temp >= -10 & temp <= 50)
  )

print("Registros despues de filtros:")
print(nrow(datos_wide))

# ============================================================
# Guardar dataset limpio
# ============================================================

archivo_salida <- file.path(clean_dir, "pm25_clean.csv")
write.csv(datos_wide, archivo_salida, row.names=FALSE)
print(paste("Dataset limpio guardado en:", archivo_salida))
print(paste("Dimensiones finales:", nrow(datos_wide), "filas x", ncol(datos_wide), "columnas"))

# Resumen por ciudad
print("Resumen por ciudad:")
print(datos_wide %>% group_by(ciudad) %>% summarise(n=n(), pm25_mean=mean(pm25,na.rm=TRUE), temp_mean=mean(temp,na.rm=TRUE), hr_mean=mean(hr,na.rm=TRUE)))
