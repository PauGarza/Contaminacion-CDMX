options(repos="http://cran.itam.mx/")
library(R2jags)

# ============================================================
# MODELO A — Normal log-log global con covariables
# ============================================================

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

imgdir <- file.path(wdir, "output/figures/")
dir.create(imgdir, recursive=TRUE, showWarnings=FALSE)

#--- Funcion util (misma que en el parcial) ---
prob <- function(x) {
  out <- min(length(x[x>0])/length(x), length(x[x<0])/length(x))
  out
}

#--- Lectura de datos limpios ---
df <- read.csv("data/clean/pm25_clean.csv", stringsAsFactors=FALSE)

# Filtrar completos para el modelo A (sin NA en variables del modelo)
df_modelo <- df[complete.cases(df[, c("pm25","temp","hr","sen_t","cos_t")]), ]

n <- nrow(df_modelo)
logy <- log(df_modelo$pm25)
temp <- df_modelo$temp
hr <- df_modelo$hr
sen_t <- df_modelo$sen_t
cos_t <- df_modelo$cos_t

cat("Observaciones usadas:", n, "\n")

#--- Datos para JAGS ---
data.a <- list("n"=n, "logy"=logy, "temp"=temp, "hr"=hr, "sen_t"=sen_t, "cos_t"=cos_t)

#--- Iniciales ---
inits.a <- function() {
  list(alpha=0, beta=c(0,0,0,0), tau=1)
}

#--- Parametros a monitorear ---
pars.a <- c("alpha", "beta", "tau", "yf1")

#--- Corrida JAGS ---
cat(">>> Corriendo modelo A (Normal global)...\n")
eja.sim <- jags(data.a, inits.a, pars.a, model.file="scripts/ExFinal_A.txt",
                n.iter=10000, n.chains=2, n.burnin=1000, n.thin=1)

#--- Extraer cadenas ---
out.a   <- eja.sim$BUGSoutput$sims.list
out.a.a <- eja.sim$BUGSoutput$sims.array
out.a.s <- eja.sim$BUGSoutput$summary

#--- Diagnostico de cadenas: alpha ---
png(file.path(imgdir,"A_cadenas_alpha.png"), width=900, height=700)
par(mfrow=c(3,2))
z1 <- out.a.a[,1,"alpha"]; z2 <- out.a.a[,2,"alpha"]
ymin <- min(z1,z2); ymax <- max(z1,z2)
plot(z1, type="l", col="grey50", ylim=c(ymin,ymax), main="Traza alpha", ylab="alpha")
lines(z2, col="firebrick2")
y1 <- cumsum(z1)/(1:length(z1)); y2 <- cumsum(z2)/(1:length(z2))
plot(y1, type="l", col="grey50", ylim=c(min(y1,y2),max(y1,y2)), main="Media ergodica alpha")
lines(y2, col="firebrick2")
hist(z1, freq=FALSE, col="grey70", border="white", main="Hist cadena 1 — alpha", xlab="alpha")
hist(z2, freq=FALSE, col="firebrick2", border="white", main="Hist cadena 2 — alpha", xlab="alpha")
acf(z1, main="ACF cadena 1 — alpha")
acf(z2, main="ACF cadena 2 — alpha")
dev.off()

#--- Diagnostico de cadenas: beta[1] (temp) ---
png(file.path(imgdir,"A_cadenas_beta1.png"), width=900, height=700)
par(mfrow=c(3,2))
z1 <- out.a.a[,1,"beta[1]"]; z2 <- out.a.a[,2,"beta[1]"]
ymin <- min(z1,z2); ymax <- max(z1,z2)
plot(z1, type="l", col="grey50", ylim=c(ymin,ymax), main="Traza beta[1] (temp)", ylab="beta[1]")
lines(z2, col="firebrick2")
y1 <- cumsum(z1)/(1:length(z1)); y2 <- cumsum(z2)/(1:length(z2))
plot(y1, type="l", col="grey50", ylim=c(min(y1,y2),max(y1,y2)), main="Media ergodica beta[1]")
lines(y2, col="firebrick2")
hist(z1, freq=FALSE, col="grey70", border="white", main="Hist cadena 1 — beta[1]", xlab="beta[1]")
hist(z2, freq=FALSE, col="firebrick2", border="white", main="Hist cadena 2 — beta[1]", xlab="beta[1]")
acf(z1, main="ACF cadena 1 — beta[1]")
acf(z2, main="ACF cadena 2 — beta[1]")
dev.off()

#--- Diagnostico de cadenas: beta[2] (hr) ---
png(file.path(imgdir,"A_cadenas_beta2.png"), width=900, height=700)
par(mfrow=c(3,2))
z1 <- out.a.a[,1,"beta[2]"]; z2 <- out.a.a[,2,"beta[2]"]
ymin <- min(z1,z2); ymax <- max(z1,z2)
plot(z1, type="l", col="grey50", ylim=c(ymin,ymax), main="Traza beta[2] (hr)", ylab="beta[2]")
lines(z2, col="firebrick2")
y1 <- cumsum(z1)/(1:length(z1)); y2 <- cumsum(z2)/(1:length(z2))
plot(y1, type="l", col="grey50", ylim=c(min(y1,y2),max(y1,y2)), main="Media ergodica beta[2]")
lines(y2, col="firebrick2")
hist(z1, freq=FALSE, col="grey70", border="white", main="Hist cadena 1 — beta[2]", xlab="beta[2]")
hist(z2, freq=FALSE, col="firebrick2", border="white", main="Hist cadena 2 — beta[2]", xlab="beta[2]")
acf(z1, main="ACF cadena 1 — beta[2]")
acf(z2, main="ACF cadena 2 — beta[2]")
dev.off()

#--- Diagnostico de cadenas: tau ---
png(file.path(imgdir,"A_cadenas_tau.png"), width=900, height=700)
par(mfrow=c(3,2))
z1 <- out.a.a[,1,"tau"]; z2 <- out.a.a[,2,"tau"]
ymin <- min(z1,z2); ymax <- max(z1,z2)
plot(z1, type="l", col="grey50", ylim=c(ymin,ymax), main="Traza tau", ylab="tau")
lines(z2, col="firebrick2")
y1 <- cumsum(z1)/(1:length(z1)); y2 <- cumsum(z2)/(1:length(z2))
plot(y1, type="l", col="grey50", ylim=c(min(y1,y2),max(y1,y2)), main="Media ergodica tau")
lines(y2, col="firebrick2")
hist(z1, freq=FALSE, col="grey70", border="white", main="Hist cadena 1 — tau", xlab="tau")
hist(z2, freq=FALSE, col="firebrick2", border="white", main="Hist cadena 2 — tau", xlab="tau")
acf(z1, main="ACF cadena 1 — tau")
acf(z2, main="ACF cadena 2 — tau")
dev.off()

#--- Resumen posterior ---
cat("\n=== Modelo A: Resumen posterior ===\n")
idx.a <- c("alpha","beta[1]","beta[2]","beta[3]","beta[4]","tau")
print(round(out.a.s[idx.a, c(1,3,7)], 4))

#--- Ajustados vs observados ---
yf1.a <- out.a.s[grep("^yf1", rownames(out.a.s)), 1]
r2.a <- cor(logy, yf1.a)^2
cat("Pseudo-R² (escala log):", round(r2.a, 4), "\n")

png(file.path(imgdir,"A_ajustados.png"), width=700, height=600)
plot(logy, yf1.a, pch=19, cex=0.7, col="steelblue",
     xlab="log(PM2.5) observado", ylab="log(PM2.5) ajustado",
     main=paste0("Modelo A — Ajustados vs Obs  (R²=",round(r2.a,3),")"))
abline(0, 1, col="grey30", lwd=2, lty=2)
dev.off()

#--- DIC ---
dic.a <- eja.sim$BUGSoutput$DIC
cat("DIC modelo A:", round(dic.a, 2), "\n")

#--- Guardar resultados ---
res_a <- data.frame(
  parametro = c("alpha","beta_temp","beta_hr","beta_sen_t","beta_cos_t","tau"),
  media = round(out.a.s[idx.a, 1], 4),
  q2.5 = round(out.a.s[idx.a, 3], 4),
  q97.5 = round(out.a.s[idx.a, 7], 4)
)
write.csv(res_a, file.path(imgdir, "A_resumen_posterior.csv"), row.names=FALSE)

cat("\nModelo A completado. Imagenes en output/figures/\n")
