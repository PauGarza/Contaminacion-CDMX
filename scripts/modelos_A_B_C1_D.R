### ----- REGRESION AVANZADA ----- ###
# --- Modelos A, B, C1, D (14 estaciones) --- #

options(repos="http://cran.itam.mx/")

library(R2jags)
library(dplyr)
library(ggplot2)

prob <- function(x) {
  min(length(x[x>0])/length(x), length(x[x<0])/length(x))
}

diagnostico_cadena <- function(sim.obj, param.name, outdir, model.name) {
  out.a <- sim.obj$BUGSoutput$sims.array
  param.names <- dimnames(out.a)[[3]]
  idx <- grep(param.name, param.names, fixed = TRUE)
  if(length(idx) == 0) {
    cat("Parametro", param.name, "no encontrado en", model.name, "\n")
    return(NULL)
  }
  idx <- idx[1]
  z1 <- out.a[,1,idx]
  z2 <- out.a[,2,idx]
  png(file.path(outdir, paste0("diag_cadena_", model.name, "_", param.name, ".png")),
      width = 900, height = 900)
  par(mfrow=c(3,2))
  plot(z1, type="l", col="grey50", main = paste("Traza -", param.name))
  lines(z2, col="firebrick2")
  y1 <- cumsum(z1)/(1:length(z1))
  y2 <- cumsum(z2)/(1:length(z2))
  plot(y1, type="l", col="grey50", ylim=c(min(y1,y2), max(y1,y2)), main = "Media ergodica")
  lines(y2, col="firebrick2")
  hist(z1, freq=FALSE, col="grey50", main = paste("Posterior cadena 1 -", param.name))
  hist(z2, freq=FALSE, col="firebrick2", main = paste("Posterior cadena 2 -", param.name))
  acf(z1, main = "ACF cadena 1")
  acf(z2, main = "ACF cadena 2")
  dev.off()
  cat("  Diagnostico guardado:", param.name, "\n")
}

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)
outdir <- "output/figures"

df <- read.csv("data/clean/pm25_valle_mexico.csv", stringsAsFactors = FALSE)

logy   <- log(df$pm25)
temp.s <- scale(df$temp)[,1]
hr.s   <- scale(df$hr)[,1]
sen_t  <- df$sen_t
cos_t  <- df$cos_t
est    <- as.numeric(as.factor(df$estacion))
lat.s  <- scale(df$lat)[,1]
lon.s  <- scale(df$lon)[,1]

n <- length(logy)
J <- length(unique(est))

cat("=== Datos ===\n")
cat("Observaciones:", n, "\n")
cat("Estaciones:", J, "\n")
cat("Estaciones:", paste(levels(as.factor(df$estacion)), collapse = ", "), "\n\n")

# ============================================================
#--- Modelo A: Normal global ---
# ============================================================
cat("=== MODELO A: Normal global ===\n")

#-Defining data-
data.A <- list(n = n, logy = logy, temp = temp.s, hr = hr.s, sen_t = sen_t, cos_t = cos_t)

#-Defining inits-
inits.A <- function() list(alpha = 0, beta = rep(0,4), tau = 1)

#-Selecting parameters to monitor-
pars.A <- c("alpha", "beta", "tau", "sigma", "yf1")

#-Running code-
ejA <- jags(data.A, inits.A, pars.A, model.file = "scripts/jags_modelo_A_valle.txt",
            n.iter = 10000, n.chains = 2, n.burnin = 1000, n.thin = 1)

#-Monitoring chain-
resA <- ejA$BUGSoutput

save(ejA, file = file.path(outdir, "modelo_A.RData"))

cat("  Generando diagnosticos de cadena...\n")
diagnostico_cadena(ejA, "alpha", outdir, "A")
diagnostico_cadena(ejA, "beta[1]", outdir, "A")
diagnostico_cadena(ejA, "sigma", outdir, "A")

out.sum.A <- resA$summary
out.sum.t.A <- out.sum.A[grep("beta", rownames(out.sum.A)), c(1,3,7)]
out.sum.t.A <- cbind(out.sum.t.A, apply(resA$sims.list$beta, 2, prob))
dimnames(out.sum.t.A)[[2]][4] <- "prob"
cat("\n--- Betas Modelo A ---\n")
print(out.sum.t.A)

R2.A <- cor(logy, apply(resA$sims.list$yf1, 2, mean))^2
cat("Modelo A: DIC =", resA$DIC, "| Pseudo-R2 =", round(R2.A, 4), "\n")

out.yf.A <- out.sum.A[grep("yf1", rownames(out.sum.A)), ]
df_pvo <- data.frame(obs = logy, pred = out.yf.A[, 1])
rng <- range(logy, out.yf.A[, c(1, 3, 7)])
p_pvo <- ggplot(df_pvo, aes(x = obs, y = pred)) +
  geom_abline(slope = 1, intercept = 0, color = "gray70", linetype = "dashed", size = 0.7) +
  geom_point(color = "#7F8C8D", alpha = 0.40, size = 1.2) +
  xlim(rng) + ylim(rng) +
  labs(
    title    = "Modelo A: Observado vs Predicho",
    subtitle = "log(PM2.5) observado vs media posterior predictiva",
    x        = "log(PM2.5) observado",
    y        = "log(PM2.5) predicho (media posterior)"
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle    = element_text(size = 9.5, face = "italic", color = "gray30"),
    panel.grid.minor = element_blank()
  )
ggsave(file.path(outdir, "pred_vs_obs_A.png"), plot = p_pvo,
       width = 5, height = 5, dpi = 120)

write.csv(data.frame(parametro = c("alpha","beta_temp","beta_hr","beta_sen","beta_cos","sigma","DIC","pseudo_R2"),
                     media = c(resA$mean$alpha, resA$mean$beta, resA$mean$sigma, resA$DIC, R2.A)),
          file.path(outdir, "resumen_modelo_A.csv"), row.names = FALSE)


# ============================================================
#--- Modelo B: Efectos fijos por estacion ---
# ============================================================
cat("\n=== MODELO B: Efectos fijos por estacion ===\n")

#-Defining data-
data.B <- list(n = n, J = J, logy = logy, temp = temp.s, hr = hr.s, sen_t = sen_t, cos_t = cos_t, est = est)

#-Defining inits-
inits.B <- function() list(alpha = 0, beta = rep(0,4), alphaj = rep(0,J), tau = 1)

#-Selecting parameters to monitor-
pars.B <- c("alpha", "beta", "alphaj", "tau", "sigma", "yf1")

#-Running code-
ejB <- jags(data.B, inits.B, pars.B, model.file = "scripts/jags_modelo_B_valle.txt",
            n.iter = 12000, n.chains = 2, n.burnin = 2000, n.thin = 2)

#-Monitoring chain-
resB <- ejB$BUGSoutput

save(ejB, file = file.path(outdir, "modelo_B.RData"))

cat("  Generando diagnosticos de cadena...\n")
diagnostico_cadena(ejB, "alpha", outdir, "B")
diagnostico_cadena(ejB, "beta[1]", outdir, "B")
diagnostico_cadena(ejB, "sigma", outdir, "B")

out.sum.B <- resB$summary
out.sum.t.B <- out.sum.B[grep("beta", rownames(out.sum.B)), c(1,3,7)]
out.sum.t.B <- cbind(out.sum.t.B, apply(resB$sims.list$beta, 2, prob))
dimnames(out.sum.t.B)[[2]][4] <- "prob"
cat("\n--- Betas Modelo B ---\n")
print(out.sum.t.B)

alphaj_adj <- resB$mean$alphaj - mean(resB$mean$alphaj)
alpha_adj  <- resB$mean$alpha  + mean(resB$mean$alphaj)

out.alphaj.B <- out.sum.B[grep("^alphaj\\.adj\\[", rownames(out.sum.B)), c(1,3,7)]

alphaj_df <- data.frame(estacion = levels(as.factor(df$estacion)),
                        alphaj_media = resB$mean$alphaj,
                        alphaj_adj = alphaj_adj,
                        alphaj_sd = resB$sd$alphaj)
write.csv(alphaj_df, file.path(outdir, "alphaj_modelo_B.csv"), row.names = FALSE)

R2.B <- cor(logy, apply(resB$sims.list$yf1, 2, mean))^2
cat("Modelo B: DIC =", resB$DIC, "| Pseudo-R2 =", round(R2.B, 4), "\n")

write.csv(data.frame(parametro = c("alpha_adj","beta_temp","beta_hr","beta_sen","beta_cos","sigma","DIC","pseudo_R2"),
                     media = c(alpha_adj, resB$mean$beta, resB$mean$sigma, resB$DIC, R2.B)),
          file.path(outdir, "resumen_modelo_B.csv"), row.names = FALSE)


# ============================================================
#--- Modelo C1: Jerarquico Normal ---
# ============================================================
cat("\n=== MODELO C1: Jerarquico Normal ===\n")

#-Defining data-
data.C1 <- list(n = n, J = J, logy = logy, temp = temp.s, hr = hr.s, sen_t = sen_t, cos_t = cos_t, est = est)

#-Defining inits-
inits.C1 <- function() list(alpha = 0, beta = rep(0,4), alphaj = rep(0,J), tau = 1, tau.alphaj = 1)

#-Selecting parameters to monitor-
pars.C1 <- c("alpha", "beta", "alphaj", "tau", "sigma", "tau.alphaj", "sigma.alphaj", "yf1")

#-Running code-
ejC1 <- jags(data.C1, inits.C1, pars.C1, model.file = "scripts/jags_modelo_C1_valle.txt",
             n.iter = 12000, n.chains = 2, n.burnin = 2000, n.thin = 2)

#-Monitoring chain-
resC1 <- ejC1$BUGSoutput

save(ejC1, file = file.path(outdir, "modelo_C1.RData"))

cat("  Generando diagnosticos de cadena...\n")
diagnostico_cadena(ejC1, "alpha", outdir, "C1")
diagnostico_cadena(ejC1, "beta[1]", outdir, "C1")
diagnostico_cadena(ejC1, "sigma", outdir, "C1")
diagnostico_cadena(ejC1, "sigma.alphaj", outdir, "C1")

out.sum.C1 <- resC1$summary
out.sum.t.C1 <- out.sum.C1[grep("beta", rownames(out.sum.C1)), c(1,3,7)]
out.sum.t.C1 <- cbind(out.sum.t.C1, apply(resC1$sims.list$beta, 2, prob))
dimnames(out.sum.t.C1)[[2]][4] <- "prob"
cat("\n--- Betas Modelo C1 ---\n")
print(out.sum.t.C1)

out.alphaj.C1 <- out.sum.C1[grep("^alphaj\\[", rownames(out.sum.C1)), c(1,3,7)]

k <- J
ymin <- min(out.alphaj.B[,2], out.alphaj.C1[,2]) - 0.05
ymax <- max(out.alphaj.B[,3], out.alphaj.C1[,3]) + 0.05

est_labels <- levels(as.factor(df$estacion))

df_efB <- data.frame(
  estacion = factor(est_labels, levels = est_labels),
  media = out.alphaj.B[, 1], q025 = out.alphaj.B[, 2], q975 = out.alphaj.B[, 3]
)
pB <- ggplot(df_efB, aes(x = estacion, y = media, ymin = q025, ymax = q975)) +
  geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 0.6) +
  geom_errorbar(width = 0.3, color = "#3498DB", linewidth = 0.8) +
  geom_point(color = "#3498DB", size = 2.8) +
  ylim(ymin, ymax) +
  labs(title = "Modelo B: Efectos fijos por estación (alphaj, suma-cero)",
       subtitle = "IC 95% posterior — restricción de identificabilidad aplicada en R",
       x = NULL, y = "Efecto por estación (log-unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle = element_text(size = 9, face = "italic", color = "gray30"),
        panel.grid.minor = element_blank())
ggsave(file.path(outdir, "efectos_estacion_B.png"), plot = pB,
       width = 8.5, height = 5, dpi = 120)

df_efC1 <- data.frame(
  estacion = factor(est_labels, levels = est_labels),
  media = out.alphaj.C1[, 1], q025 = out.alphaj.C1[, 2], q975 = out.alphaj.C1[, 3]
)
pC1 <- ggplot(df_efC1, aes(x = estacion, y = media, ymin = q025, ymax = q975)) +
  geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 0.6) +
  geom_errorbar(width = 0.3, color = "#E74C3C", linewidth = 0.8) +
  geom_point(color = "#E74C3C", size = 2.8) +
  ylim(ymin, ymax) +
  labs(title = "Modelo C1: Efectos aleatorios por estación (alphaj)",
       subtitle = "IC 95% posterior — hiperprior N(0, tau_alpha), shrinkage bayesiano",
       x = NULL, y = "Efecto por estación (log-unidades)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle = element_text(size = 9, face = "italic", color = "gray30"),
        panel.grid.minor = element_blank())
ggsave(file.path(outdir, "efectos_estacion_C1.png"), plot = pC1,
       width = 8.5, height = 5, dpi = 120)

alphaj_C1 <- data.frame(estacion = levels(as.factor(df$estacion)),
                        alphaj_media = resC1$mean$alphaj,
                        alphaj_sd = resC1$sd$alphaj)
write.csv(alphaj_C1, file.path(outdir, "alphaj_modelo_C1.csv"), row.names = FALSE)

R2.C1 <- cor(logy, apply(resC1$sims.list$yf1, 2, mean))^2
cat("Modelo C1: DIC =", resC1$DIC, "| Pseudo-R2 =", round(R2.C1, 4), "| sigma.alphaj =", round(resC1$mean$sigma.alphaj, 4), "\n")

write.csv(data.frame(parametro = c("alpha","beta_temp","beta_hr","beta_sen","beta_cos","sigma","sigma.alphaj","DIC","pseudo_R2"),
                     media = c(resC1$mean$alpha, resC1$mean$beta, resC1$mean$sigma, resC1$mean$sigma.alphaj, resC1$DIC, R2.C1)),
          file.path(outdir, "resumen_modelo_C1.csv"), row.names = FALSE)


# ============================================================
#--- Modelo D: Tendencia espacial directa ---
# ============================================================
cat("\n=== MODELO D: Tendencia espacial directa ===\n")

#-Defining data-
data.D <- list(n = n, logy = logy, temp = temp.s, hr = hr.s, sen_t = sen_t, cos_t = cos_t, lat = lat.s, lon = lon.s)

#-Defining inits-
inits.D <- function() list(alpha = 0, beta = rep(0,6), tau = 1)

#-Selecting parameters to monitor-
pars.D <- c("alpha", "beta", "tau", "sigma", "yf1")

#-Running code-
ejD <- jags(data.D, inits.D, pars.D, model.file = "scripts/jags_modelo_D_valle.txt",
            n.iter = 10000, n.chains = 2, n.burnin = 1000, n.thin = 1)

#-Monitoring chain-
resD <- ejD$BUGSoutput

save(ejD, file = file.path(outdir, "modelo_D.RData"))

cat("  Generando diagnosticos de cadena...\n")
diagnostico_cadena(ejD, "alpha", outdir, "D")
diagnostico_cadena(ejD, "beta[1]", outdir, "D")
diagnostico_cadena(ejD, "sigma", outdir, "D")

out.sum.D <- resD$summary
out.sum.t.D <- out.sum.D[grep("beta", rownames(out.sum.D)), c(1,3,7)]
out.sum.t.D <- cbind(out.sum.t.D, apply(resD$sims.list$beta, 2, prob))
dimnames(out.sum.t.D)[[2]][4] <- "prob"
cat("\n--- Betas Modelo D ---\n")
print(out.sum.t.D)

R2.D <- cor(logy, apply(resD$sims.list$yf1, 2, mean))^2
cat("Modelo D: DIC =", resD$DIC, "| Pseudo-R2 =", round(R2.D, 4), "\n")

write.csv(data.frame(parametro = c("alpha","beta_temp","beta_hr","beta_sen","beta_cos","beta_lat","beta_lon","sigma","DIC","pseudo_R2"),
                     media = c(resD$mean$alpha, resD$mean$beta, resD$mean$sigma, resD$DIC, R2.D)),
          file.path(outdir, "resumen_modelo_D.csv"), row.names = FALSE)


# ============================================================
#--- Comparacion de modelos ---
# ============================================================
cat("\n=== COMPARACION DE MODELOS ===\n")

comp <- data.frame(
  Modelo = c("A","B","C1","D"),
  DIC = c(resA$DIC, resB$DIC, resC1$DIC, resD$DIC),
  pseudo_R2 = c(R2.A, R2.B, R2.C1, R2.D),
  sigma = c(resA$mean$sigma, resB$mean$sigma, resC1$mean$sigma, resD$mean$sigma)
)
print(comp)
write.csv(comp, file.path(outdir, "comparacion_modelos.csv"), row.names = FALSE)


# ============================================================
#--- Modelo C2: Gamma global ---
# ============================================================
cat("\n=== MODELO C2: Gamma global ===\n")

#-Variables centradas (no estandarizadas: mas estable para Gamma en JAGS)-
tempc <- df$temp - mean(df$temp)
hrc   <- df$hr   - mean(df$hr)

#-Defining data-
data.C2 <- list(n = n, pm25 = df$pm25, tempc = tempc, hrc = hrc, sen_t = sen_t, cos_t = cos_t)

#-Defining inits-
inits.C2 <- function() list(alpha = log(mean(df$pm25)), beta = rep(0,4), a = 1)

#-Selecting parameters to monitor-
pars.C2 <- c("alpha", "beta", "a", "sigma.C2", "yf1")

#-Running code-
ejC2 <- jags(data.C2, inits.C2, pars.C2, model.file = "scripts/jags_modelo_C2_gamma_valle.txt",
             n.iter = 15000, n.chains = 2, n.burnin = 3000, n.thin = 3)

#-Monitoring chain-
resC2 <- ejC2$BUGSoutput

save(ejC2, file = file.path(outdir, "modelo_C2.RData"))

cat("  Generando diagnosticos de cadena...\n")
diagnostico_cadena(ejC2, "alpha",   outdir, "C2")
diagnostico_cadena(ejC2, "beta[1]", outdir, "C2")
diagnostico_cadena(ejC2, "a",       outdir, "C2")

out.sum.C2 <- resC2$summary
out.sum.t.C2 <- out.sum.C2[grep("beta", rownames(out.sum.C2)), c(1,3,7)]
out.sum.t.C2 <- cbind(out.sum.t.C2, apply(resC2$sims.list$beta, 2, prob))
dimnames(out.sum.t.C2)[[2]][4] <- "prob"
cat("\n--- Betas Modelo C2 ---\n")
print(out.sum.t.C2)

# pseudo-R2 en escala log para comparar con modelos Normal
R2.C2 <- cor(log(df$pm25), log(apply(resC2$sims.list$yf1, 2, mean)))^2
cat("Modelo C2 (Gamma): DIC =", resC2$DIC, "| Pseudo-R2 (log) =", round(R2.C2, 4), "\n")
cat("NOTA: DIC de C2 NO es comparable con A/B/C1/D (familia diferente).\n")
cat("      Usar pseudo-R2 en escala log para comparar entre familias.\n")

write.csv(data.frame(parametro = c("alpha","beta_temp","beta_hr","beta_sen","beta_cos","a","DIC","pseudo_R2_log"),
                     media = c(resC2$mean$alpha, resC2$mean$beta, resC2$mean$a, resC2$DIC, R2.C2)),
          file.path(outdir, "resumen_modelo_C2.csv"), row.names = FALSE)
