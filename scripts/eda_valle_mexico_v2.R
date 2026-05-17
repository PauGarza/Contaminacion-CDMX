options(repos="http://cran.itam.mx/")
library(dplyr)
library(lubridate)
library(corrplot)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

# ============================================================
# EDA v2 — Exploracion del dataset expandido
# ============================================================

df <- read.csv("data/clean/pm25_valle_mexico_v2.csv", stringsAsFactors=FALSE)
df$date <- as.Date(df$date)

cat("=== EDA v2: Valle de Mexico expandido ===\n")
cat("Observaciones:", nrow(df), "\n")
cat("Estaciones:", length(unique(df$estacion)), "\n")
cat("Periodo:", min(df$date), "a", max(df$date), "\n")

# Resumen por estacion
resumen <- df %>%
  group_by(estacion, ciudad, municipio, lat, lon) %>%
  summarise(
    n = n(),
    pm25_mean = mean(pm25),
    pm25_sd = sd(pm25),
    pm25_min = min(pm25),
    pm25_max = max(pm25),
    temp_mean = mean(temp),
    hr_mean = mean(hr),
    .groups = "drop"
  ) %>%
  arrange(desc(pm25_mean))

cat("\n--- Top 10 estaciones mas contaminadas ---\n")
print(head(resumen[, c("estacion", "municipio", "ciudad", "n", "pm25_mean")], 10))

cat("\n--- Top 10 estaciones menos contaminadas ---\n")
print(tail(resumen[, c("estacion", "municipio", "ciudad", "n", "pm25_mean")], 10))

# Comparacion CDMX vs Edomex vs Hidalgo
comp_ciudad <- df %>%
  group_by(ciudad) %>%
  summarise(
    n = n(),
    estaciones = n_distinct(estacion),
    pm25_mean = mean(pm25),
    pm25_sd = sd(pm25),
    temp_mean = mean(temp),
    hr_mean = mean(hr),
    .groups = "drop"
  )
cat("\n--- Comparacion por ciudad ---\n")
print(comp_ciudad)

# Serie temporal: promedio diario
daily_mean <- df %>%
  group_by(date) %>%
  summarise(pm25 = mean(pm25), temp = mean(temp), hr = mean(hr), .groups="drop")

png("output/figures/eda_valle_v2_series.png", width=1200, height=600)
par(mfrow=c(1,3))
plot(daily_mean$date, daily_mean$pm25, type="l", col="firebrick2",
     main="PM2.5 promedio diario", ylab="µg/m³", xlab="")
plot(daily_mean$date, daily_mean$temp, type="l", col="steelblue",
     main="Temperatura promedio diaria", ylab="°C", xlab="")
plot(daily_mean$date, daily_mean$hr, type="l", col="darkgreen",
     main="HR promedio diaria", ylab="%", xlab="")
dev.off()
cat("\nGuardado: output/figures/eda_valle_v2_series.png\n")

# Boxplot por estacion (ordenadas por PM2.5)
est_ord <- resumen$estacion[order(resumen$pm25_mean)]
df$estacion <- factor(df$estacion, levels=est_ord)

png("output/figures/eda_valle_v2_boxplot_estacion.png", width=1400, height=700)
boxplot(pm25 ~ estacion, data=df, horizontal=TRUE, las=1, cex.axis=0.7,
        main="PM2.5 por estacion (ordenadas por media)", col="grey80",
        xlab="PM2.5 (µg/m³)")
abline(v=mean(df$pm25), col="firebrick2", lwd=2, lty=2)
legend("bottomright", legend=paste("Global:", round(mean(df$pm25),1)), 
       col="firebrick2", lwd=2, lty=2, bg="white")
dev.off()
cat("Guardado: output/figures/eda_valle_v2_boxplot_estacion.png\n")

# Boxplot por mes
meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
df$mes_nombre <- factor(meses_esp[df$mes], levels=meses_esp)
png("output/figures/eda_valle_v2_boxplot_mes.png", width=900, height=500)
boxplot(pm25 ~ mes_nombre, data=df, col="grey80",
        main="PM2.5 por mes", xlab="", ylab="PM2.5 (µg/m³)")
dev.off()
cat("Guardado: output/figures/eda_valle_v2_boxplot_mes.png\n")

# Histogramas
png("output/figures/eda_valle_v2_histogramas.png", width=1200, height=400)
par(mfrow=c(1,4))
hist(df$pm25, main="PM2.5", col="grey70", border="white", xlab="µg/m³")
hist(log(df$pm25), main="log(PM2.5)", col="grey70", border="white", xlab="log")
hist(df$temp, main="Temperatura", col="grey70", border="white", xlab="°C")
hist(df$hr, main="Humedad relativa", col="grey70", border="white", xlab="%")
dev.off()
cat("Guardado: output/figures/eda_valle_v2_histogramas.png\n")

# Correlacion
cor_mat <- cor(df[, c("temp", "hr", "sen_t", "cos_t", "pm25")], use="complete.obs")
png("output/figures/eda_valle_v2_correlacion.png", width=600, height=600)
corrplot(cor_mat, method="color", type="upper", addCoef.col="black",
         tl.col="black", tl.srt=45, diag=FALSE,
         title="Correlacion variables", mar=c(0,0,1,0))
dev.off()
cat("Guardado: output/figures/eda_valle_v2_correlacion.png\n")

# Scatter con lowess
png("output/figures/eda_valle_v2_scatter.png", width=1200, height=400)
par(mfrow=c(1,3))
plot(df$temp, df$pm25, pch=20, col=rgb(0,0,0,0.2), xlab="Temp (°C)", ylab="PM2.5")
lines(lowess(df$temp, df$pm25), col="firebrick2", lwd=2)
plot(df$hr, df$pm25, pch=20, col=rgb(0,0,0,0.2), xlab="HR (%)", ylab="PM2.5")
lines(lowess(df$hr, df$pm25), col="firebrick2", lwd=2)
plot(df$dia_año, df$pm25, pch=20, col=rgb(0,0,0,0.2), xlab="Dia del año", ylab="PM2.5")
lines(lowess(df$dia_año, df$pm25), col="firebrick2", lwd=2)
dev.off()
cat("Guardado: output/figures/eda_valle_v2_scatter.png\n")

# Guardar resumen
write.csv(resumen, "output/figures/eda_valle_v2_resumen.csv", row.names=FALSE)
cat("\nResumen guardado: output/figures/eda_valle_v2_resumen.csv\n")
