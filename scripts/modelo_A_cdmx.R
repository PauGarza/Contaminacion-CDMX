options(repos="http://cran.itam.mx/")
library(R2jags)
library(dplyr)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

# ============================================================
# Modelo A — Normal global log-log (solo CDMX)
# ============================================================

df <- read.csv("data/clean/pm25_cdmx_jags.csv", stringsAsFactors=FALSE)

# Datos
n <- nrow(df)
logy <- log(df$pm25)
temp.s <- df$temp_s
hr.s   <- df$hr_s
sen_t  <- df$sen_t
cos_t  <- df$cos_t

#--- JAGS ---
cat("Modelo A — Normal global log-log\n")
cat("Observaciones:", n, "\n")

data.A <- list(n=n, logy=logy, temp=temp.s, hr=hr.s, sen_t=sen_t, cos_t=cos_t)
inits.A <- function() list(alpha=0, beta=rep(0,4), tau=1)
pars.A <- c("alpha","beta","tau","yf1","DIC")

ejA.sim <- jags(data.A, inits.A, pars.A,
                model.file="scripts/jags_modelo_A_cdmx.txt",
                n.iter=20000, n.chains=2, n.burnin=5000, n.thin=5)

print(ejA.sim)

# Guardar resultados
save(ejA.sim, file="output/modelo_A_cdmx.RData")

# Pseudo-R2
outA <- ejA.sim$BUGSoutput
y_hat <- outA$mean$yf1
cor_y <- cor(logy, y_hat)^2
cat("\nPseudo-R2 (correlacion logy vs yf1)^2:", round(cor_y, 4), "\n")
cat("DIC:", outA$DIC, "\n")

# --- Diagnosticos ---
png("output/figures/diag_A_cdmx.png", width=900, height=600)
par(mfrow=c(3,3))

# Traces
traceplot(ejA.sim, varname="alpha")
traceplot(ejA.sim, varname="beta")
traceplot(ejA.sim, varname="tau")

# Histogramas
hist(outA$sims.list$alpha, main="alpha", col="grey70", border="white")
abline(v=outA$mean$alpha, col="firebrick2", lwd=2)

for (j in 1:4) {
  hist(outA$sims.list$beta[,j], main=paste0("beta[",j,"]"), col="grey70", border="white")
  abline(v=outA$mean$beta[j], col="firebrick2", lwd=2)
}

dev.off()
cat("\nDiagnosticos guardados: output/figures/diag_A_cdmx.png\n")
