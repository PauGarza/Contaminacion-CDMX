### ----- REGRESION AVANZADA ----- ###
# --- Guardar figuras del Modelo F para el reporte LaTeX --- #
# Requiere: output/figures/Contaminacion_Fase1_SerieTiempo.RData
#           output/figures/Contaminacion_Fase2_SerieTiempo.RData

library(dplyr)
library(ggplot2)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)
outdir <- "output/figures"

load(file.path(outdir, "Contaminacion_Fase1_SerieTiempo.RData"))  # -> output_jags
load(file.path(outdir, "Contaminacion_Fase2_SerieTiempo.RData"))  # -> output_fase2

# ============================================================
# FIGURA 1: Diagnostico MCMC Etapa 1 — beta[1]
# ============================================================
beta_c1 <- output_jags$BUGSoutput$sims.array[, 1, "beta[1]"]
beta_c2 <- output_jags$BUGSoutput$sims.array[, 2, "beta[1]"]
N_iter  <- length(beta_c1)

png(file.path(outdir, "diag_cadena_F1_beta1_v2.png"), width = 900, height = 900)
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))

plot(beta_c1, type = "l", col = "#3498DB", xlab = "Iteracion", ylab = "Valor",
     main = "Traza — beta[1] (sen_t)")
lines(beta_c2, col = "#E74C3C")

m1 <- cumsum(beta_c1) / (1:N_iter)
m2 <- cumsum(beta_c2) / (1:N_iter)
plot(m1, type = "l", col = "#3498DB", ylim = range(c(m1, m2)),
     xlab = "Iteracion", ylab = "Media", main = "Media ergodica")
lines(m2, col = "#E74C3C")

hist(beta_c1, breaks = 25, col = "#3498DB", border = "white",
     main = "Posterior cadena 1", xlab = "beta[1]", ylab = "Frecuencia")
hist(beta_c2, breaks = 25, col = "#E74C3C", border = "white",
     main = "Posterior cadena 2", xlab = "beta[1]", ylab = "Frecuencia")

acf(beta_c1, main = "ACF cadena 1")
acf(beta_c2, main = "ACF cadena 2")

title(main = "Modelo F Etapa 1 — Diagnostico MCMC: beta[1]", outer = TRUE, font = 2)
dev.off()
cat("Figura 1 guardada: diag_cadena_F1_beta1_v2.png\n")

# ============================================================
# FIGURA 2: Diagnostico MCMC Etapa 2 — gamma[1]
# ============================================================
g1 <- output_fase2$BUGSoutput$sims.array[, 1, "gamma[1]"]
g2 <- output_fase2$BUGSoutput$sims.array[, 2, "gamma[1]"]
g3 <- output_fase2$BUGSoutput$sims.array[, 3, "gamma[1]"]
N2 <- length(g1)

png(file.path(outdir, "diag_cadena_F2_gamma1_v2.png"), width = 900, height = 900)
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))

rng <- range(c(g1, g2, g3))
plot(g1, type = "l", col = "#3498DB", ylim = rng,
     xlab = "Iteracion", ylab = "Valor", main = "Traza — gamma[1] (intercepto ZMVM)")
lines(g2, col = "#E74C3C"); lines(g3, col = "#2ECC71")

e1 <- cumsum(g1)/(1:N2); e2 <- cumsum(g2)/(1:N2); e3 <- cumsum(g3)/(1:N2)
plot(e1, type = "l", col = "#3498DB", ylim = range(c(e1,e2,e3)),
     xlab = "Iteracion", ylab = "Media", main = "Media ergodica")
lines(e2, col = "#E74C3C"); lines(e3, col = "#2ECC71")

hist(g1, breaks = 25, col = "#3498DB", border = "white",
     main = "Posterior cadena 1", xlab = "gamma[1]", ylab = "Frecuencia")
hist(g2, breaks = 25, col = "#E74C3C", border = "white",
     main = "Posterior cadena 2", xlab = "gamma[1]", ylab = "Frecuencia")

acf_g1 <- acf(g1, plot = FALSE, lag.max = 30)
acf_g2 <- acf(g2, plot = FALSE, lag.max = 30)
acf_g3 <- acf(g3, plot = FALSE, lag.max = 30)
plot(0:30, acf_g1$acf, type = "h", col = "#3498DB", lwd = 2,
     xlab = "Lag", ylab = "ACF", main = "ACF cadenas 1-3", ylim = c(-0.1, 1))
lines(0:30, acf_g2$acf, type = "h", col = "#E74C3C", lwd = 2)
lines(0:30, acf_g3$acf, type = "h", col = "#2ECC71", lwd = 2)
abline(h = c(-1.96/sqrt(N2), 1.96/sqrt(N2)), col = "gray", lty = 2)

hist(g3, breaks = 25, col = "#2ECC71", border = "white",
     main = "Posterior cadena 3", xlab = "gamma[1]", ylab = "Frecuencia")

title(main = "Modelo F Etapa 2 — Diagnostico MCMC: gamma[1]", outer = TRUE, font = 2)
dev.off()
cat("Figura 2 guardada: diag_cadena_F2_gamma1_v2.png\n")

# ============================================================
# FIGURA 3: Validacion imputacion Fase 1 — 3 estaciones
# ============================================================

# Reconstruir datos necesarios para las 3 estaciones de validacion
ruta_csv <- "data/clean/pm25_valle_mexico_v2.csv"
df_orig  <- read.csv(ruta_csv)
df_orig$date <- as.Date(df_orig$date)

nodos_geo <- df_orig %>% select(estacion, lat, lon) %>% distinct() %>% arrange(estacion)
dias_ano  <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")

grid_completo <- expand.grid(date = dias_ano, estacion = nodos_geo$estacion) %>%
  left_join(df_orig, by = c("date", "estacion")) %>%
  group_by(estacion) %>%
  mutate(
    temp = ifelse(is.na(temp), mean(temp, na.rm = TRUE), temp),
    hr   = ifelse(is.na(hr),   mean(hr,   na.rm = TRUE), hr)
  ) %>%
  ungroup() %>%
  arrange(estacion, date)

sims_log_pm25 <- output_jags$BUGSoutput$sims.list$log_pm25
pm25_mean_mat <- apply(sims_log_pm25, c(2, 3), mean)
pm25_inf_mat  <- apply(sims_log_pm25, c(2, 3), quantile, probs = 0.025)
pm25_sup_mat  <- apply(sims_log_pm25, c(2, 3), quantile, probs = 0.975)

df_post <- grid_completo %>%
  mutate(
    pm25_est = exp(as.vector(pm25_mean_mat)),
    ic_inf   = exp(as.vector(pm25_inf_mat)),
    ic_sup   = exp(as.vector(pm25_sup_mat))
  )

est_obj <- c("Hospital General de Mexico", "Santiago Acahualtepec", "Benito Juarez")
df_val  <- df_post %>%
  filter(estacion %in% est_obj) %>%
  mutate(estacion_lab = factor(
    estacion, levels = est_obj,
    labels = c("Hospital General de Mexico (52 obs)",
               "Santiago Acahualtepec (189 obs)",
               "Benito Juarez (236 obs)")
  ))

p_val <- ggplot(df_val, aes(x = date)) +
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), fill = "#3498DB", alpha = 0.25) +
  geom_line(aes(y = pm25_est), color = "#2C3E50", linewidth = 0.7) +
  geom_point(aes(y = pm25), color = "gray40", alpha = 0.45, size = 0.9) +
  facet_wrap(~estacion_lab, ncol = 1, scales = "free_y") +
  labs(
    title    = "Modelo F Etapa 1: Validacion de imputacion MCMC",
    subtitle = "Linea: media posterior  |  Banda: IC 95%  |  Puntos: observados",
    x = "Fecha (2023)", y = expression(PM[2.5]~"("*mu*g/m^3*")")
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle = element_text(size = 9, face = "italic", color = "gray30"),
    strip.text    = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(outdir, "validacion_F1_imputacion_v2.png"),
       plot = p_val, width = 9, height = 8, dpi = 120)
cat("Figura 3 guardada: validacion_F1_imputacion_v2.png\n")

# ============================================================
# FIGURA 4: Serie tiempo macro-ambiental ZMVM (Etapa 2)
# ============================================================
res2          <- output_fase2$BUGSoutput$summary
idx_mu        <- grep("^mu_cdmx\\[", rownames(res2))
df_mu         <- as.data.frame(res2[idx_mu, c("mean", "2.5%", "97.5%")])
vector_fechas <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")

df_zmvm <- data.frame(
  fecha    = vector_fechas,
  pm25     = exp(df_mu$mean),
  ic_inf   = exp(df_mu$`2.5%`),
  ic_sup   = exp(df_mu$`97.5%`)
)

p_zmvm <- ggplot(df_zmvm, aes(x = fecha)) +
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), fill = "#E74C3C", alpha = 0.25) +
  geom_line(aes(y = pm25), color = "#2C3E50", linewidth = 0.9) +
  labs(
    title    = "Modelo F Etapa 2: Tendencia macro-ambiental de la ZMVM",
    subtitle = expression(paste("Media posterior de exp(", mu[t]^{ZMVM}, ")  |  Banda: IC 95%")),
    x = "Fecha (2023)",
    y = expression(PM[2.5]~"("*mu*g/m^3*")")
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle = element_text(size = 9, face = "italic", color = "gray30"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(outdir, "serie_mu_cdmx_v2.png"),
       plot = p_zmvm, width = 9, height = 4.5, dpi = 120)
cat("Figura 4 guardada: serie_mu_cdmx_v2.png\n")

# ============================================================
# FIGURA 5: Efectos espaciales Etapa 2 — alpha_corregido
# ============================================================
estaciones_ord <- sort(unique(df_orig$estacion))
idx_alph  <- grep("^alpha_corregido\\[", rownames(res2))
df_alph   <- as.data.frame(res2[idx_alph, c("mean", "2.5%", "97.5%")])

df_ef <- data.frame(
  estacion = factor(estaciones_ord,
                    levels = estaciones_ord[order(df_alph$mean)]),
  media    = df_alph$mean,
  q025     = df_alph$`2.5%`,
  q975     = df_alph$`97.5%`
)

p_ef <- ggplot(df_ef, aes(x = estacion, y = media)) +
  geom_hline(yintercept = 0, color = "#7F8C8D", linetype = "dashed", linewidth = 0.7) +
  geom_errorbar(aes(ymin = q025, ymax = q975),
                width = 0.3, color = "#E74C3C", linewidth = 0.7) +
  geom_point(color = "#2C3E50", size = 2.5) +
  coord_flip() +
  labs(
    title    = "Modelo F Etapa 2: Efectos espaciales de estacion",
    subtitle = expression(paste("Media posterior e IC 95% de ", alpha[j]^"*", " (suma cero)")),
    x        = NULL,
    y        = expression(paste("Desviacion respecto a la media de la ZMVM (log-escala)"))
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle = element_text(size = 9, face = "italic", color = "gray30"),
    panel.grid.minor = element_blank(),
    axis.text.y   = element_text(size = 9, color = "#2C3E50")
  )

ggsave(file.path(outdir, "efectos_F2_estaciones_v2.png"),
       plot = p_ef, width = 7.5, height = 5.5, dpi = 120)
cat("Figura 5 guardada: efectos_F2_estaciones_v2.png\n")

cat("\n=== Todas las figuras del Modelo F guardadas en", outdir, "===\n")
