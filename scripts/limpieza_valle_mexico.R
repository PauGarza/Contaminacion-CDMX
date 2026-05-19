options(repos="http://cran.itam.mx/")
library(dplyr)
library(lubridate)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)

# ============================================================
# Limpieza — 56 estaciones en territorio Valle de Mexico
# ============================================================
# Decisiones de limpieza:
#   1. Solo incluir estaciones con PM2.5, temp Y HR disponibles
#   2. Calcular medias diarias para homogeneizar frecuencias
#   3. Filtrar observaciones con complete.cases()
#   4. Estandarizar covariables despues de la limpieza (no antes)
#   5. Usar spatial join pre-calculado para asignar municipio/alcaldia

cat("=== Limpieza: 56 estaciones Valle de Mexico ===\n")

# 1. Cargar metadatos de estaciones
est_meta <- read.csv("data/raw/estaciones_territorio_valle.csv", stringsAsFactors=FALSE)
cat("Estaciones en metadatos:", nrow(est_meta), "\n")

# 2. Leer todos los CSVs disponibles en data/raw/
all_files <- list.files("data/raw", pattern="_(PM2[.]5|temp|hr)_2023[.]csv$", full.names=TRUE)
cat("Archivos CSV encontrados:", length(all_files), "\n")

# Mapear archivo -> estacion + variable
leer_csv <- function(f) {
  # Extraer info del nombre: {prefijo}_{nombre}_{variable}_2023.csv
  bn <- basename(f)
  parts <- strsplit(bn, "_")[[1]]
  # El prefijo es parts[1], la variable es parts[length(parts)-1]
  # El nombre puede tener _ internos
  prefijo <- parts[1]
  variable <- parts[length(parts)-1]
  # Reconstruir nombre de estacion
  nombre_est <- paste(parts[-c(1, length(parts)-1, length(parts))], collapse="_")
  nombre_est <- gsub("_2023", "", nombre_est)
  
  df <- tryCatch({
    read.csv(f, stringsAsFactors=FALSE)
  }, error=function(e) {
    cat("ERROR leyendo", bn, ":", conditionMessage(e), "\n")
    NULL
  })
  
  if (is.null(df)) return(NULL)
  
  # Determinar que columna contiene el valor y la fecha
  # SINAICA usualmente tiene: date, pollutant/parameter, value, unit, etc.
  # Intentar detectar automaticamente
  col_names <- tolower(names(df))
  
  # Buscar columna de fecha
  fecha_col <- grep("fecha|date|hora", col_names, value=TRUE)[1]
  if (is.na(fecha_col)) fecha_col <- names(df)[1]
  
  # Buscar columna de valor
  valor_col <- grep("valor|value|concentracion|conc|pm25|tmp|hr|hum|temp", col_names, value=TRUE)[1]
  if (is.na(valor_col)) valor_col <- names(df)[ncol(df)]
  
  # Renombrar
  df$fecha_raw <- df[[fecha_col]]
  df$valor_raw <- suppressWarnings(as.numeric(as.character(df[[valor_col]])))
  
  # Parsear fecha (SINAICA usa formato ISO o similar)
  df$date <- as.Date(df$fecha_raw)
  
  # Filtrar solo 2023 y valores validos
  df <- df %>%
    filter(!is.na(date), year(date)==2023, !is.na(valor_raw), valor_raw >= 0) %>%
    select(date, valor_raw) %>%
    mutate(estacion_file = nombre_est, variable = variable)
  
  return(df)
}

# Leer todos los archivos
datos_list <- lapply(all_files, leer_csv)
datos_list <- datos_list[!sapply(datos_list, is.null)]

cat("Archivos leidos exitosamente:", length(datos_list), "\n")

if (length(datos_list) == 0) {
  cat("ERROR: No se pudieron leer archivos. Terminando.\n")
  quit(status=1)
}

# 3. Combinar y calcular medias diarias
all_datos <- bind_rows(datos_list)

cat("Total registros brutos:", nrow(all_datos), "\n")
cat("Variables encontradas:", paste(unique(all_datos$variable), collapse=", "), "\n")
cat("Estaciones encontradas:", length(unique(all_datos$estacion_file)), "\n")

# Pivotear: cada fila = estacion + fecha, columnas = PM2.5, temp, hr
daily <- all_datos %>%
  group_by(estacion_file, date, variable) %>%
  summarise(valor_dia = mean(valor_raw, na.rm=TRUE), .groups="drop") %>%
  tidyr::pivot_wider(names_from=variable, values_from=valor_dia)

cat("Observaciones diarias (antes de filtrar):", nrow(daily), "\n")
cat("Columnas:", paste(names(daily), collapse=", "), "\n")

# Renombrar columnas estandar
# SINAICA usa PM2.5, TMP, HR en los nombres de archivo
# Pero las columnas pueden llamarse de otra forma
# Ajustar nombres
names(daily) <- tolower(names(daily))
if ("pm2.5" %in% names(daily)) daily <- daily %>% rename(pm25 = `pm2.5`)
if ("tmp" %in% names(daily)) daily <- daily %>% rename(temp = tmp)

# Verificar que tenemos las 3 variables
vars_presentes <- intersect(c("pm25", "temp", "hr"), names(daily))
cat("Variables presentes:", paste(vars_presentes, collapse=", "), "\n")

if (length(vars_presentes) < 3) {
  cat("ADVERTENCIA: No todas las variables presentes. Filtrando con las disponibles.\n")
}

# 4. Filtrar complete cases para las 3 variables
# Solo si las 3 existen
if (all(c("pm25", "temp", "hr") %in% names(daily))) {
  daily <- daily %>% filter(complete.cases(pm25, temp, hr))
  cat("Observaciones con las 3 variables:", nrow(daily), "\n")
} else {
  # Filtrar con las que tengamos
  daily <- daily %>% filter(complete.cases(across(any_of(c("pm25", "temp", "hr")))))
  cat("Observaciones con variables disponibles:", nrow(daily), "\n")
}

# 5. Mapear estacion_file a metadatos (nombre canonico, lat, lon, municipio)
# Construir nombre canonico para matching
est_meta$estacion_file <- tolower(gsub("[ \\.]", "", est_meta$station_name))
est_meta$estacion_file <- iconv(est_meta$estacion_file, to="ASCII//TRANSLIT")

daily <- daily %>%
  left_join(est_meta %>% select(estacion_file, station_name, municipio, estado, lat, lon),
            by="estacion_file")

# Filtrar las que hicieron match
daily <- daily %>% filter(!is.na(station_name))
cat("Observaciones con match en metadatos:", nrow(daily), "\n")

# 6. Crear variables temporales
daily <- daily %>%
  mutate(
    dia_año = yday(date),
    mes = month(date),
    dia_semana = wday(date),
    sen_t = sin(2 * pi * dia_año / 365),
    cos_t = cos(2 * pi * dia_año / 365),
    estacion = station_name,
    ciudad = ifelse(estado == "Distrito Federal", "cdmx",
             ifelse(estado == "México", "edomex", "hidalgo"))
  )

# 7. Resumen por estacion
resumen <- daily %>%
  group_by(estacion, ciudad, municipio, lat, lon) %>%
  summarise(
    n = n(),
    pm25_mean = mean(pm25),
    pm25_sd = sd(pm25),
    temp_mean = mean(temp),
    hr_mean = mean(hr),
    .groups="drop"
  ) %>%
  arrange(desc(pm25_mean))

cat("\n=== Resumen por estacion ===\n")
print(resumen)

# 8. Guardar
write.csv(daily, "data/clean/pm25_valle_mexico.csv", row.names=FALSE)
write.csv(resumen, "data/clean/resumen_estaciones.csv", row.names=FALSE)

cat("\nGuardado:\n")
cat("  data/clean/pm25_valle_mexico.csv (", nrow(daily), "obs x", ncol(daily), "vars )\n")
cat("  data/clean/resumen_estaciones.csv (", nrow(resumen), "estaciones )\n")

# 9. Comparar con version anterior
if (file.exists("data/clean/pm25_valle_mexico.csv")) {
  old <- read.csv("data/clean/pm25_valle_mexico.csv", stringsAsFactors=FALSE)
  cat("\nComparacion con version anterior:\n")
  cat("  Anterior:", nrow(old), "obs,", length(unique(old$estacion)), "estaciones\n")
  cat("  Nueva:   ", nrow(daily), "obs,", length(unique(daily$estacion)), "estaciones\n")
}
