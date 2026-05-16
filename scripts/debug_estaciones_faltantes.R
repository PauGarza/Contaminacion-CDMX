df <- read.csv("data/clean/pm25_cdmx_completo.csv")
cat("Estaciones en pm25_cdmx_completo:\n")
print(unique(df$estacion))

cat("\n=== Verificando Camarones ===\n")
pm <- read.csv("data/raw/cdmx_completo_camarones_PM2.5_2023.csv")
cat("Camarones PM2.5 registros:", nrow(pm), "\n")
cat("Camarones PM2.5 fechas unicas:", length(unique(pm$date)), "\n")

cat("\n=== Verificando CCA (Centro de Ciencias) ===\n")
pm2 <- read.csv("data/raw/cdmx_completo_centrodecienciasdelaatmosfera_PM2.5_2023.csv")
cat("CCA PM2.5 registros:", nrow(pm2), "\n")
cat("CCA PM2.5 fechas unicas:", length(unique(pm2$date)), "\n")

# Verificar si existen archivos temp/hr
cat("\n=== Archivos disponibles ===\n")
for (est in c("camarones","centrodecienciasdelaatmosfera")) {
  for (var in c("temp","hr")) {
    f <- paste0("data/raw/cdmx_completo_", est, "_", var, "_2023.csv")
    if (file.exists(f)) {
      df_tmp <- read.csv(f)
      cat(est, var, ":", nrow(df_tmp), "registros,", length(unique(df_tmp$date)), "fechas\n")
    } else {
      cat(est, var, ": NO EXISTE\n")
    }
  }
}

# Verificar overlap de fechas entre PM2.5 y temp/hr para todas las estaciones
cat("\n=== Overlap de fechas (PM2.5 vs temp+hr) ===\n")
for (est in c("benitojuarez","gustavoamadero","merced","pedregal","santiagoacahualtepec",
              "uamiztapalapa","uamxochimilco","camarones","centrodecienciasdelaatmosfera")) {
  f_pm <- paste0("data/raw/cdmx_completo_", est, "_PM2.5_2023.csv")
  f_temp <- paste0("data/raw/cdmx_completo_", est, "_temp_2023.csv")
  f_hr <- paste0("data/raw/cdmx_completo_", est, "_hr_2023.csv")
  
  if (file.exists(f_pm) && file.exists(f_temp) && file.exists(f_hr)) {
    d_pm <- read.csv(f_pm)
    d_temp <- read.csv(f_temp)
    d_hr <- read.csv(f_hr)
    d_pm$date <- as.Date(d_pm$date)
    d_temp$date <- as.Date(d_temp$date)
    d_hr$date <- as.Date(d_hr$date)
    
    # Fechas con datos en las 3 variables
    fechas_pm <- unique(d_pm$date)
    fechas_temp <- unique(d_temp$date)
    fechas_hr <- unique(d_hr$date)
    
    overlap <- intersect(intersect(fechas_pm, fechas_temp), fechas_hr)
    cat(est, ": PM2.5=", length(fechas_pm), "temp=", length(fechas_temp), 
        "hr=", length(fechas_hr), "overlap=", length(overlap), "\n")
  } else {
    cat(est, ": faltan archivos\n")
  }
}
