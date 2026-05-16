options(repos="http://cran.itam.mx/")
library(dplyr)

# Leer como lo hace limpieza
pm_files <- list.files("data/raw", pattern="cdmx_completo_.*_PM2\\.5_2023\\.csv", full.names=TRUE)
leer_var <- function(files, varname) {
  out_list <- list()
  for (f in files) {
    df <- read.csv(f, stringsAsFactors=FALSE)
    fname <- basename(f)
    est_file <- gsub("cdmx_completo_|_PM2\\.5_2023\\.csv|_temp_2023\\.csv|_hr_2023\\.csv", "", fname)
    df$estacion_file <- est_file
    df$date <- as.Date(df$date)
    out_list[[length(out_list)+1]] <- df
  }
  bind_rows(out_list)
}
pm_all <- leer_var(pm_files, "pm25")
cat("PM2.5 estaciones:", paste(sort(unique(pm_all$estacion_file)), collapse=", "), "\n")

temp_files <- list.files("data/raw", pattern="cdmx_completo_.*_temp_2023\\.csv", full.names=TRUE)
temp_all <- leer_var(temp_files, "temp")
cat("TEMP estaciones:", paste(sort(unique(temp_all$estacion_file)), collapse=", "), "\n")

hr_files <- list.files("data/raw", pattern="cdmx_completo_.*_hr_2023\\.csv", full.names=TRUE)
hr_all <- leer_var(hr_files, "hr")
cat("HR estaciones:", paste(sort(unique(hr_all$estacion_file)), collapse=", "), "\n")

# Verificar coords
coords <- read.csv("data/coords_estaciones_cdmx.csv", stringsAsFactors=FALSE)
coords <- coords[coords$state_code == 9, ]
coords$estacion_file <- tolower(gsub("[ \\.]", "", coords$station_name))
coords$estacion_file <- iconv(coords$estacion_file, to="ASCII//TRANSLIT")
cat("COORDS estaciones:", paste(sort(coords$estacion_file), collapse=", "), "\n")

# Ver quien falta en el join
pm_est <- sort(unique(pm_all$estacion_file))
temp_est <- sort(unique(temp_all$estacion_file))
hr_est <- sort(unique(hr_all$estacion_file))
coords_est <- sort(coords$estacion_file)

cat("\nEn coords pero NO en temp:", paste(setdiff(coords_est, temp_est), collapse=", "), "\n")
cat("En coords pero NO en hr:", paste(setdiff(coords_est, hr_est), collapse=", "), "\n")
cat("En coords pero NO en pm25:", paste(setdiff(coords_est, pm_est), collapse=", "), "\n")

# Verificar nombres exactos de CCA
idx_cca <- grep("ciencias", coords_est, value=TRUE)
cat("\nNombre CCA en coords:", idx_cca, "\n")
ccapm <- grep("ciencias", pm_est, value=TRUE)
cat("Nombre CCA en PM2.5:", ccapm, "\n")
ccatm <- grep("ciencias", temp_est, value=TRUE)
cat("Nombre CCA en temp:", ccatm, "\n")
