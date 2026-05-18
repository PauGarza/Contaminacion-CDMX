### ----- REGRESION AVANZADA ----- ###
# --- Regenerar graficas de efectos B y C1 (sin re-correr MCMC) --- #

library(dplyr)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)
outdir <- "output/figures"

# Cargar datos solo para obtener J (numero de estaciones)
df <- read.csv("data/clean/pm25_valle_mexico_v2.csv", stringsAsFactors = FALSE)
J  <- length(unique(df$estacion))
estaciones <- sort(unique(df$estacion))

# Cargar modelos guardados
cat("Cargando modelo B...\n")
load(file.path(outdir, "modelo_B_v2.RData"))   # -> ejB
resB    <- ejB$BUGSoutput
out.sum.B <- resB$summary

cat("Cargando modelo C1...\n")
load(file.path(outdir, "modelo_C1_v2.RData"))  # -> ejC1
resC1    <- ejC1$BUGSoutput
out.sum.C1 <- resC1$summary

# Modelo B: aplicar restriccion suma-cero a cada muestra MCMC, luego resumir
# (alphaj.adj no fue monitorado — se reconstruye de sims.list$alphaj)
alphaj_sims     <- resB$sims.list$alphaj               # n_sims x J
alphaj_adj_sims <- alphaj_sims - rowMeans(alphaj_sims) # suma-cero por fila
out.alphaj.B <- data.frame(
  media = colMeans(alphaj_adj_sims),
  q025  = apply(alphaj_adj_sims, 2, quantile, 0.025),
  q975  = apply(alphaj_adj_sims, 2, quantile, 0.975)
)

# Modelo C1: alphaj[j] directamente del summary
out.alphaj.C1 <- out.sum.C1[grep("^alphaj\\[", rownames(out.sum.C1)), c(1, 3, 7)]
out.alphaj.C1 <- as.data.frame(out.alphaj.C1)
colnames(out.alphaj.C1) <- c("media", "q025", "q975")

cat("Filas B (adj):", nrow(out.alphaj.B), "\n")
cat("Filas C1:     ", nrow(out.alphaj.C1), "\n")

# ylim compartido para comparabilidad visual
ymin <- min(out.alphaj.B$q025, out.alphaj.C1$q025) - 0.05
ymax <- max(out.alphaj.B$q975, out.alphaj.C1$q975) + 0.05
k    <- J

# ============================================================
# Grafica B: Efectos fijos por estacion (suma-cero)
# ============================================================
png(file.path(outdir, "efectos_estacion_B_v2.png"), width = 1000, height = 600, res = 120)
par(mar = c(8, 4, 4, 2))
plot(1:k, out.alphaj.B$media,
     xlab = "", ylab = "log-unidades",
     xaxt = "n", ylim = c(ymin, ymax), pch = 19, col = "steelblue",
     main = "Modelo B: Efectos fijos por estacion (alphaj, suma-cero)")
segments(1:k, out.alphaj.B$q025, 1:k, out.alphaj.B$q975, col = "steelblue")
abline(h = 0, col = "grey70", lty = 2)
axis(1, at = 1:k, labels = estaciones, las = 2, cex.axis = 0.7)
dev.off()
cat("Guardado:", file.path(outdir, "efectos_estacion_B_v2.png"), "\n")

# ============================================================
# Grafica C1: Efectos aleatorios por estacion
# ============================================================
png(file.path(outdir, "efectos_estacion_C1_v2.png"), width = 1000, height = 600, res = 120)
par(mar = c(8, 4, 4, 2))
plot(1:k, out.alphaj.C1$media,
     xlab = "", ylab = "log-unidades",
     xaxt = "n", ylim = c(ymin, ymax), pch = 19, col = "firebrick2",
     main = "Modelo C1: Efectos aleatorios por estacion (alphaj)")
segments(1:k, out.alphaj.C1$q025, 1:k, out.alphaj.C1$q975, col = "firebrick2")
abline(h = 0, col = "grey70", lty = 2)
axis(1, at = 1:k, labels = estaciones, las = 2, cex.axis = 0.7)
dev.off()
cat("Guardado:", file.path(outdir, "efectos_estacion_C1_v2.png"), "\n")
