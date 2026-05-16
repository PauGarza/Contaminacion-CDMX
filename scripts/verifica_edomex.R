options(repos="http://cran.itam.mx/")
library(dplyr)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

# Estaciones de Edomex que descargamos
edomex <- c("tlalnepantla","nezahualcoyotl","sanagustin","fesaragon")

for (est in edomex) {
  cat("\n===", est, "===\n")
  
  f_pm <- paste0("data/raw/cdmx_completo_", est, "_PM2.5_2023.csv")
  f_temp <- paste0("data/raw/cdmx_completo_", est, "_temp_2023.csv")
  f_hr <- paste0("data/raw/cdmx_completo_", est, "_hr_2023.csv")
  
  if (file.exists(f_pm)) {
    pm <- read.csv(f_pm, stringsAsFactors=FALSE)
    pm$date <- as.Date(pm$date)
    cat("PM2.5 registros:", nrow(pm), "fechas:", length(unique(pm$date)), "\n")
    
    if (file.exists(f_temp) && file.exists(f_hr)) {
      temp <- read.csv(f_temp, stringsAsFactors=FALSE)
      hr <- read.csv(f_hr, stringsAsFactors=FALSE)
      temp$date <- as.Date(temp$date)
      hr$date <- as.Date(hr$date)
      
      daily_pm <- pm %>% group_by(date) %>% summarise(pm25 = mean(value, na.rm=TRUE))
      daily_temp <- temp %>% group_by(date) %>% summarise(temp = mean(value, na.rm=TRUE))
      daily_hr <- hr %>% group_by(date) %>% summarise(hr = mean(value, na.rm=TRUE))
      
      df <- daily_pm %>% left_join(daily_temp, by="date") %>% left_join(daily_hr, by="date")
      cat("Dias con overlap completo:", sum(complete.cases(df)), "\n")
      cat("PM2.5 min/max/mean:", round(min(df$pm25,na.rm=T),1), round(max(df$pm25,na.rm=T),1), round(mean(df$pm25,na.rm=T),1), "\n")
      
      if (sum(complete.cases(df)) > 100) {
        cat("** INCLUIBLE ** (>100 dias completos)\n")
      } else {
        cat("** INSUFICIENTE ** (<100 dias)\n")
      }
    } else {
      cat("Faltan archivos temp/hr\n")
    }
  } else {
    cat("NO EXISTE PM2.5\n")
  }
}
