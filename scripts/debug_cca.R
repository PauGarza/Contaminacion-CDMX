library(dplyr)

# Leer datos de CCA
pm <- read.csv("data/raw/cdmx_completo_centrodecienciasdelaatmosfera_PM2.5_2023.csv", stringsAsFactors=FALSE)
temp <- read.csv("data/raw/cdmx_completo_centrodecienciasdelaatmosfera_temp_2023.csv", stringsAsFactors=FALSE)
hr <- read.csv("data/raw/cdmx_completo_centrodecienciasdelaatmosfera_hr_2023.csv", stringsAsFactors=FALSE)

pm$date <- as.Date(pm$date)
temp$date <- as.Date(temp$date)
hr$date <- as.Date(hr$date)

# Medias diarias
daily_pm <- pm %>% group_by(date) %>% summarise(pm25 = mean(value, na.rm=TRUE))
daily_temp <- temp %>% group_by(date) %>% summarise(temp = mean(value, na.rm=TRUE))
daily_hr <- hr %>% group_by(date) %>% summarise(hr = mean(value, na.rm=TRUE))

# Unir
df <- daily_pm %>% left_join(daily_temp, by="date") %>% left_join(daily_hr, by="date")
cat("CCA dias totales:", nrow(df), "\n")
cat("CCA con complete.cases:", sum(complete.cases(df)), "\n")
cat("CCA con NA en pm25:", sum(is.na(df$pm25)), "\n")
cat("CCA con NA en temp:", sum(is.na(df$temp)), "\n")
cat("CCA con NA en hr:", sum(is.na(df$hr)), "\n")
cat("Primeras fechas:\n")
print(head(df, 10))

# Ahora verificar que pasa con el filtro pm25 > 0 & pm25 < 500
df_filt <- df %>% filter(pm25 > 0 & pm25 < 500)
cat("\nDespues de filtro pm25 > 0 & < 500:", nrow(df_filt), "\n")

# Verificar el dataset final
df_final <- read.csv("data/clean/pm25_cdmx_completo.csv", stringsAsFactors=FALSE)
cat("\nEstaciones en dataset final:", paste(sort(unique(df_final$estacion)), collapse=", "), "\n")
