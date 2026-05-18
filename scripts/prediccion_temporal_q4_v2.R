# ============================================================
# Prediccion temporal Q4 2023 — Modelo C1 v2 (14 estaciones)
# ============================================================
library(dplyr)
library(lubridate)
library(ggplot2)

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

# Grafica — estilo Otho
prom_global$mes <- factor(prom_global$mes, levels = c("Octubre", "Noviembre", "Diciembre"))
promedio_anual  <- mean(df$pm25)

p <- ggplot(prom_global, aes(x = mes, y = pm25, group = 1)) +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5,
                  fill = "Intervalo de Credibilidad (95%)"), alpha = 0.22) +
  geom_line(aes(color = "Predicción Q4 2023"), linewidth = 1.0, lineend = "round") +
  geom_point(aes(color = "Predicción Q4 2023"), size = 3) +
  geom_hline(aes(yintercept = promedio_anual, linetype = "Promedio anual observado"),
             color = "#3498DB", linewidth = 0.8) +
  scale_color_manual(values = c("Predicción Q4 2023" = "#2C3E50")) +
  scale_fill_manual(values  = c("Intervalo de Credibilidad (95%)" = "#E74C3C")) +
  scale_linetype_manual(values = c("Promedio anual observado" = "dashed")) +
  labs(
    title    = "Predicción temporal Q4 2023 — Modelo C1 v2 (climatología)",
    subtitle = "Predicción mensual global para 14 estaciones del Valle de México",
    x        = "Mes",
    y        = expression(paste("Concentración de ", PM[2.5], " (", mu, "g/", m^3, ")")),
    color    = NULL, fill = NULL, linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position    = "bottom",
    legend.box         = "vertical",
    plot.title         = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle      = element_text(size = 9.5, face = "italic", color = "gray30"),
    panel.grid.minor   = element_blank(),
    legend.title       = element_text(face = "bold", size = 9)
  )

ggsave(file.path(outdir, "prediccion_temporal_q4_v2.png"),
       plot = p, width = 7.5, height = 4.5, dpi = 120)
cat("\nGuardado:", file.path(outdir, "prediccion_temporal_q4_v2.png"), "\n")
