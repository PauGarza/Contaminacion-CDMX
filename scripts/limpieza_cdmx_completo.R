options(repos="http://cran.itam.mx/")
library(dplyr)
library(lubridate)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

dir.create("data/clean", recursive=TRUE, showWarnings=FALSE)

# ============================================================
# 1. Coordenadas de estaciones (solo CDMX, state_code=9)
# ============================================================
coords <- read.csv("data/coords_estaciones_cdmx.csv", stringsAsFactors=FALSE)
coords <- coords[coords$state_code == 9, ]
coords$estacion_file <- tolower(gsub("[ \\.]", "", coords$station_name))
coords$estacion_file <- iconv(coords$estacion_file, to="ASCII//TRANSLIT")

cat("Estaciones CDMX puras (state_code=9):\n")
print(coords[, c("station_id","station_name","lat","lon")])

# ============================================================
# 2. Leer CSVs de PM2.5
# ============================================================
pm_files <- list.files("data/raw", pattern="cdmx_completo_.*_PM2\\.5_2023\\.csv", full.names=TRUE)
cat("\nArchivos PM2.5:", length(pm_files), "\n")

leer_var <- function(files, varname) {
  out_list <- list()
  for (f in files) {
    df <- read.csv(f, stringsAsFactors=FALSE)
    # Extraer nombre de estacion del archivo
    fname <- basename(f)
    # cdmx_completo_{nombre}_PM2.5_2023.csv -> nombre
    est_file <- gsub("cdmx_completo_|_PM2\\.5_2023\\.csv|_temp_2023\\.csv|_hr_2023\\.csv", "", fname)
    df$estacion_file <- est_file
    df$date <- as.Date(df$date)
    out_list[[length(out_list)+1]] <- df
  }
  bind_rows(out_list)
}

pm_all <- leer_var(pm_files, "pm25")
cat("Registros PM2.5 brutos:", nrow(pm_all), "\n")

#--- Temperatura ---
temp_files <- list.files("data/raw", pattern="cdmx_completo_.*_temp_2023\\.csv", full.names=TRUE)
temp_all <- leer_var(temp_files, "temp")

#--- Humedad relativa ---
hr_files <- list.files("data/raw", pattern="cdmx_completo_.*_hr_2023\\.csv", full.names=TRUE)
hr_all <- leer_var(hr_files, "hr")

# ============================================================
# 3. Calcular medias diarias
# ============================================================
daily_pm <- pm_all %>%
  group_by(date, estacion_file) %>%
  summarise(pm25 = mean(value, na.rm=TRUE), .groups="drop")

daily_temp <- temp_all %>%
  group_by(date, estacion_file) %>%
  summarise(temp = mean(value, na.rm=TRUE), .groups="drop")

daily_hr <- hr_all %>%
  group_by(date, estacion_file) %>%
  summarise(hr = mean(value, na.rm=TRUE), .groups="drop")

# ============================================================
# 4. Unir y filtrar solo estaciones de CDMX
# ============================================================
df_daily <- daily_pm %>%
  left_join(daily_temp, by=c("date","estacion_file")) %>%
  left_join(daily_hr, by=c("date","estacion_file")) %>%
  inner_join(coords[, c("estacion_file","station_name","lat","lon")], by="estacion_file")

# Renombrar
df_daily <- df_daily %>%
  rename(estacion = station_name) %>%
  mutate(ciudad = "cdmx")

# ============================================================
# 5. Filtros
# ============================================================
cat("Antes de filtros:", nrow(df_daily), "\n")
df_daily <- df_daily %>%
  filter(pm25 > 0 & pm25 < 500) %>%
  filter(complete.cases(pm25, temp, hr))
cat("Despues de filtros (complete cases):", nrow(df_daily), "\n")

# ============================================================
# 6. Variables temporales
# ============================================================
df_daily <- df_daily %>%
  mutate(
    dia_año = yday(date),
    mes = month(date),
    dia_semana = wday(date),
    sen_t = sin(2 * pi * dia_año / 365),
    cos_t = cos(2 * pi * dia_año / 365)
  )

# ============================================================
# 7. Resumen por estacion
# ============================================================
resumen <- df_daily %>%
  group_by(estacion, lat, lon) %>%
  summarise(
    n = n(),
    pm25_mean = mean(pm25, na.rm=TRUE),
    pm25_sd = sd(pm25, na.rm=TRUE),
    temp_mean = mean(temp, na.rm=TRUE),
    hr_mean = mean(hr, na.rm=TRUE),
    .groups="drop"
  ) %>%
  arrange(desc(pm25_mean))

cat("\n=== Resumen por estacion ===\n")
print(resumen)

# ============================================================
# 8. Guardar
# ============================================================
write.csv(df_daily, "data/clean/pm25_cdmx_completo.csv", row.names=FALSE)
write.csv(resumen, "data/clean/resumen_cdmx_completo.csv", row.names=FALSE)
cat("\nGuardado: data/clean/pm25_cdmx_completo.csv\n")
cat("Total de observaciones:", nrow(df_daily), "\n")
cat("Estaciones:", length(unique(df_daily$estacion)), "\n")
