# ============================================================
# Modelo C2 — Jerarquico Gamma simplificado (datos v2)
# ============================================================
library(R2jags)
library(dplyr)

outdir <- "output/figures"
df <- read.csv("data/clean/pm25_valle_mexico_v2.csv", stringsAsFactors = FALSE)

y      <- df$pm25
temp.c <- scale(df$temp, center = TRUE, scale = FALSE)[,1]
hr.c   <- scale(df$hr, center = TRUE, scale = FALSE)[,1]
sen_t  <- df$sen_t
cos_t  <- df$cos_t
est    <- as.numeric(as.factor(df$estacion))

n <- length(y)
J <- length(unique(est))

data.C2 <- list(n = n, J = J, y = y, temp = temp.c, hr = hr.c,
                sen_t = sen_t, cos_t = cos_t, est = est)

inits.C2 <- function() {
  list(alpha = 2.8, beta = rep(0,4), alphaj = rep(0,J), r = 8, tau.alphaj = 1)
}

pars.C2 <- c("alpha","beta","alphaj","r","tau.alphaj","sigma.alphaj","cv","yf1")

cat("Ejecutando Modelo C2 Gamma (v2, simplificado)...\n")
ejC2 <- jags(data.C2, inits.C2, pars.C2,
             model.file = "scripts/jags_modelo_C2_valle.txt",
             n.iter = 8000, n.chains = 2, n.burnin = 3000, n.thin = 2)

resC2 <- ejC2$BUGSoutput
DIC.C2 <- resC2$DIC
media_yf1 <- apply(resC2$sims.list$yf1, 2, mean)
R2.C2 <- cor(y, media_yf1)^2

cat("DIC:", DIC.C2, "\n")
cat("Pseudo-R2:", R2.C2, "\n")
cat("r (forma):", resC2$mean$r, "| CV:", resC2$mean$cv, "\n")

resumen.C2 <- data.frame(
  parametro = c("alpha","beta_temp","beta_hr","beta_sen","beta_cos",
                "r","cv","sigma_alphaj","DIC","pseudo_R2"),
  media = c(resC2$mean$alpha, resC2$mean$beta[1], resC2$mean$beta[2],
            resC2$mean$beta[3], resC2$mean$beta[4],
            resC2$mean$r, resC2$mean$cv, resC2$mean$sigma.alphaj,
            DIC.C2, R2.C2),
  sd = c(resC2$sd$alpha, resC2$sd$beta[1], resC2$sd$beta[2],
         resC2$sd$beta[3], resC2$sd$beta[4],
         resC2$sd$r, resC2$sd$cv, resC2$sd$sigma.alphaj, NA, NA)
)
write.csv(resumen.C2, file.path(outdir, "resumen_modelo_C2_v2.csv"), row.names = FALSE)

# Ajuste
png(file.path(outdir, "ajuste_C2_v2.png"), width = 800, height = 600, res = 120)
plot(y, media_yf1, pch = 20, col = rgb(0,0,0.6,0.3),
     xlab = "PM2.5 observado (ug/m3)", ylab = "PM2.5 predictivo medio (ug/m3)")
abline(0, 1, col = "red", lwd = 2)
title(main = paste0("Modelo C2 Gamma v2\nPseudo-R2 = ", round(R2.C2, 3)))
dev.off()

cat("Resultados C2 guardados.\n")
