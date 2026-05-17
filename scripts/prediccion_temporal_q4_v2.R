# ============================================================
# Prediccion temporal Q4 2023 — Modelo C1 v2 (14 estaciones)
# ============================================================
library(dplyr)
library(lubridate)

outdir <- "output/figures"
df <- read.csv("data/clean/pm25_valle_mexico_v2.csv", stringsAsFactors = FALSE)
df$date <- as.Date(df$date)

# Climatologia mensual de temp y HR (ene-sep 2023)
clim <- df %>%
  filter(month(date) <= 9) %>%
  group_by(mes = month(date)) %>%
  summarise(temp_m = mean(temp), hr_m = mean(hr), .groups = "drop")

# Meses Q4
meses_q4 <- data.frame(
  mes = 10:12,
  mes_nombre = c("Octubre", "Noviembre", "Diciembre"),
  temp = c(clim$temp_m[clim$mes == 9], clim$temp_m[clim$mes == 9], clim$temp_m[clim$mes == 1]),
  hr = c(clim$hr_m[clim$mes == 9], clim$hr_m[clim$mes == 9], clim$hr_m[clim$mes == 1])
)

# Estandarizar con mismos parametros que en el modelo
temp_mean <- mean(df$temp)
temp_sd <- sd(df$temp)
hr_mean <- mean(df$hr)
hr_sd <- sd(df$hr)

meses_q4$temp_s <- (meses_q4$temp - temp_mean) / temp_sd
meses_q4$hr_s <- (meses_q4$hr - hr_mean) / hr_sd

# Fourier Q4
dias_q4 <- ymd(paste0("2023-", 10:12, "-15"))
meses_q4$sen_t <- sin(2 * pi * yday(dias_q4) / 365)
meses_q4$cos_t <- cos(2 * pi * yday(dias_q4) / 365)

# Cargar muestras posteriores de C1 v2
load(file.path(outdir, "modelo_C1_v2.RData"))
sims <- resC1$sims.list
n_sims <- length(sims$alpha)

# Estaciones y efectos aleatorios
estaciones <- levels(as.factor(df$estacion))
J <- length(estaciones)

# Matriz de prediccion: estacion x mes x muestra
pred_array <- array(0, dim = c(J, 3, n_sims))

for(m in 1:n_sims) {
  for(j in 1:J) {
    for(k in 1:3) {
      mu <- sims$alpha[m] + sims$alphaj[m, j] +
            sims$beta[m, 1] * meses_q4$temp_s[k] +
            sims$beta[m, 2] * meses_q4$hr_s[k] +
            sims$beta[m, 3] * meses_q4$sen_t[k] +
            sims$beta[m, 4] * meses_q4$cos_t[k]
      pred_array[j, k, m] <- exp(mu)
    }
  }
}

# Resumen
pred_resumen <- data.frame()
for(j in 1:J) {
  for(k in 1:3) {
    vals <- pred_array[j, k, ]
    pred_resumen <- rbind(pred_resumen, data.frame(
      estacion = estaciones[j],
      mes = meses_q4$mes_nombre[k],
      pm25_mean = mean(vals),
      pm25_q2.5 = quantile(vals, 0.025),
      pm25_q97.5 = quantile(vals, 0.975)
    ))
  }
}

# Promedio mensual global
prom_global <- pred_resumen %>%
  group_by(mes) %>%
  summarise(pm25 = mean(pm25_mean), q2.5 = mean(pm25_q2.5), q97.5 = mean(pm25_q97.5), .groups = "drop")

write.csv(pred_resumen, file.path(outdir, "prediccion_temporal_q4_2023_v2.csv"), row.names = FALSE)
write.csv(prom_global, file.path(outdir, "prediccion_temporal_q4_2023_global_v2.csv"), row.names = FALSE)

cat("=== Prediccion Q4 2023 (v2, 14 estaciones) ===\n")
print(prom_global)

# Grafica
png(file.path(outdir, "prediccion_temporal_q4_v2.png"), width = 900, height = 500, res = 120)
mes_ord <- factor(prom_global$mes, levels = c("Octubre", "Noviembre", "Diciembre"))
plot(as.numeric(mes_ord), prom_global$pm25, type = "b", pch = 19, col = "firebrick2", lwd = 2,
     xlim = c(0.8, 3.2), ylim = c(10, 25), xaxt = "n", xlab = "", ylab = "PM2.5 (ug/m3)",
     main = "Prediccion temporal Q4 2023 — Modelo C1 v2 (climatologia)")
axis(1, at = 1:3, labels = levels(mes_ord))
segments(1:3, prom_global$q2.5, 1:3, prom_global$q97.5, col = "firebrick2", lwd = 2)
points(1:3, prom_global$q2.5, pch = "_", col = "firebrick2", cex = 2)
points(1:3, prom_global$q97.5, pch = "_", col = "firebrick2", cex = 2)
abline(h = mean(df$pm25), col = "steelblue", lty = 2, lwd = 2)
legend("topleft", legend = c("Prediccion Q4", "Promedio anual"), 
       col = c("firebrick2", "steelblue"), lty = c(1, 2), lwd = 2, bg = "white")
dev.off()
cat("\nGuardado:", file.path(outdir, "prediccion_temporal_q4_v2.png"), "\n")
