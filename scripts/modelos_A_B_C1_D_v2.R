# ============================================================
# Modelos A, B, C1, D — Reentrenados con datos v2 (14 estaciones)
# ============================================================
library(R2jags)
library(dplyr)

outdir <- "output/figures"
df <- read.csv("data/clean/pm25_valle_mexico_v2.csv", stringsAsFactors = FALSE)

# Preparar datos
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

cat("=== Datos v2 ===\n")
cat("Observaciones:", n, "\n")
cat("Estaciones:", J, "\n")
cat("Estaciones:", paste(levels(as.factor(df$estacion)), collapse = ", "), "\n\n")

# ============================================================
# MODELO A — Normal global
# ============================================================
cat("--- Ejecutando Modelo A ---\n")
data.A <- list(n = n, logy = logy, temp = temp.s, hr = hr.s, sen_t = sen_t, cos_t = cos_t)
inits.A <- function() list(alpha = 0, beta = rep(0,4), tau = 1)
pars.A <- c("alpha", "beta", "tau", "sigma", "yf1")
ejA <- jags(data.A, inits.A, pars.A, model.file = "scripts/jags_modelo_A_valle.txt",
            n.iter = 10000, n.chains = 2, n.burnin = 1000, n.thin = 1)
resA <- ejA$BUGSoutput
R2.A <- cor(logy, apply(resA$sims.list$yf1, 2, mean))^2
cat("Modelo A: DIC =", resA$DIC, "| Pseudo-R2 =", round(R2.A, 4), "\n")
write.csv(data.frame(parametro = c("alpha","beta_temp","beta_hr","beta_sen","beta_cos","sigma","DIC","pseudo_R2"),
                     media = c(resA$mean$alpha, resA$mean$beta, resA$mean$sigma, resA$DIC, R2.A)),
          file.path(outdir, "resumen_modelo_A_v2.csv"), row.names = FALSE)

# ============================================================
# MODELO B — Efectos fijos por estacion
# ============================================================
cat("\n--- Ejecutando Modelo B ---\n")
data.B <- list(n = n, J = J, logy = logy, temp = temp.s, hr = hr.s, sen_t = sen_t, cos_t = cos_t, est = est)
inits.B <- function() list(alpha = 0, beta = rep(0,4), alphaj = rep(0,J), tau = 1)
pars.B <- c("alpha", "beta", "alphaj", "tau", "sigma", "yf1")
ejB <- jags(data.B, inits.B, pars.B, model.file = "scripts/jags_modelo_B_valle.txt",
            n.iter = 12000, n.chains = 2, n.burnin = 2000, n.thin = 2)
resB <- ejB$BUGSoutput
R2.B <- cor(logy, apply(resB$sims.list$yf1, 2, mean))^2
cat("Modelo B: DIC =", resB$DIC, "| Pseudo-R2 =", round(R2.B, 4), "\n")

# Suma-cero post-hoc
alphaj_adj <- resB$mean$alphaj - mean(resB$mean$alphaj)
alpha_adj  <- resB$mean$alpha + mean(resB$mean$alphaj)
alphaj_df <- data.frame(estacion = levels(as.factor(df$estacion)),
                        alphaj_media = resB$mean$alphaj,
                        alphaj_adj = alphaj_adj,
                        alphaj_sd = resB$sd$alphaj)
write.csv(alphaj_df, file.path(outdir, "alphaj_modelo_B_v2.csv"), row.names = FALSE)

write.csv(data.frame(parametro = c("alpha_adj","beta_temp","beta_hr","beta_sen","beta_cos","sigma","DIC","pseudo_R2"),
                     media = c(alpha_adj, resB$mean$beta, resB$mean$sigma, resB$DIC, R2.B)),
          file.path(outdir, "resumen_modelo_B_v2.csv"), row.names = FALSE)

# ============================================================
# MODELO C1 — Jerarquico Normal
# ============================================================
cat("\n--- Ejecutando Modelo C1 ---\n")
data.C1 <- list(n = n, J = J, logy = logy, temp = temp.s, hr = hr.s, sen_t = sen_t, cos_t = cos_t, est = est)
inits.C1 <- function() list(alpha = 0, beta = rep(0,4), alphaj = rep(0,J), tau = 1, tau_alpha = 1)
pars.C1 <- c("alpha", "beta", "alphaj", "tau", "sigma", "tau_alpha", "sigma_alpha", "yf1")
ejC1 <- jags(data.C1, inits.C1, pars.C1, model.file = "scripts/jags_modelo_C1_valle.txt",
             n.iter = 12000, n.chains = 2, n.burnin = 2000, n.thin = 2)
resC1 <- ejC1$BUGSoutput
R2.C1 <- cor(logy, apply(resC1$sims.list$yf1, 2, mean))^2
cat("Modelo C1: DIC =", resC1$DIC, "| Pseudo-R2 =", round(R2.C1, 4), "| sigma_alpha =", round(resC1$mean$sigma_alpha, 4), "\n")

alphaj_C1 <- data.frame(estacion = levels(as.factor(df$estacion)),
                        alphaj_media = resC1$mean$alphaj,
                        alphaj_sd = resC1$sd$alphaj)
write.csv(alphaj_C1, file.path(outdir, "alphaj_modelo_C1_v2.csv"), row.names = FALSE)

write.csv(data.frame(parametro = c("alpha","beta_temp","beta_hr","beta_sen","beta_cos","sigma","sigma_alpha","DIC","pseudo_R2"),
                     media = c(resC1$mean$alpha, resC1$mean$beta, resC1$mean$sigma, resC1$mean$sigma_alpha, resC1$DIC, R2.C1)),
          file.path(outdir, "resumen_modelo_C1_v2.csv"), row.names = FALSE)

# ============================================================
# MODELO D — Tendencia espacial directa (lat/lon lineal)
# ============================================================
cat("\n--- Ejecutando Modelo D ---\n")
data.D <- list(n = n, logy = logy, temp = temp.s, hr = hr.s, sen_t = sen_t, cos_t = cos_t, lat = lat.s, lon = lon.s)
inits.D <- function() list(alpha = 0, beta = rep(0,6), tau = 1)
pars.D <- c("alpha", "beta", "tau", "sigma", "yf1")
ejD <- jags(data.D, inits.D, pars.D, model.file = "scripts/jags_modelo_D_valle.txt",
            n.iter = 10000, n.chains = 2, n.burnin = 1000, n.thin = 1)
resD <- ejD$BUGSoutput
R2.D <- cor(logy, apply(resD$sims.list$yf1, 2, mean))^2
cat("Modelo D: DIC =", resD$DIC, "| Pseudo-R2 =", round(R2.D, 4), "\n")

write.csv(data.frame(parametro = c("alpha","beta_temp","beta_hr","beta_sen","beta_cos","beta_lat","beta_lon","sigma","DIC","pseudo_R2"),
                     media = c(resD$mean$alpha, resD$mean$beta, resD$mean$sigma, resD$DIC, R2.D)),
          file.path(outdir, "resumen_modelo_D_v2.csv"), row.names = FALSE)

# ============================================================
# RESUMEN COMPARATIVO
# ============================================================
cat("\n=== COMPARACION V2 ===\n")
comp <- data.frame(
  Modelo = c("A","B","C1","D"),
  DIC = c(resA$DIC, resB$DIC, resC1$DIC, resD$DIC),
  pseudo_R2 = c(R2.A, R2.B, R2.C1, R2.D),
  sigma = c(resA$mean$sigma, resB$mean$sigma, resC1$mean$sigma, resD$mean$sigma)
)
print(comp)
write.csv(comp, file.path(outdir, "comparacion_modelos_v2.csv"), row.names = FALSE)
