options(repos="http://cran.itam.mx/")
library(dplyr)
library(lubridate)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)

imgdir <- "output/figures/"
dir.create(imgdir, recursive=TRUE, showWarnings=FALSE)

df <- read.csv("data/clean/pm25_cdmx_jags.csv", stringsAsFactors=FALSE)
df$date <- as.Date(df$date)

# ============================================================
# 1. SERIE TEMPORAL POR ESTACION
# ============================================================
png(file.path(imgdir, "eda_cdmx_series.png"), width=900, height=550)
par(mfrow=c(1,1), mar=c(4,4,3,1))

estaciones <- sort(unique(df$estacion))
colores_est <- c("firebrick2","steelblue","forestgreen","darkorange",
                 "purple","chocolate4","grey40")

plot(range(df$date), range(df$pm25), type="n",
     main="PM2.5 diario por estacion — CDMX (2023)",
     xlab="Fecha", ylab="PM2.5 (ug/m3)", cex.main=1.1)

for (i in seq_along(estaciones)) {
  sub <- df[df$estacion == estaciones[i], ]
  lines(sub$date, sub$pm25, col=colores_est[i], lwd=0.8)
  points(sub$date, sub$pm25, col=colores_est[i], pch=20, cex=0.4)
}

abline(h=mean(df$pm25, na.rm=TRUE), col="grey50", lty=2, lwd=1.5)
legend("topright", legend=estaciones, col=colores_est, lwd=1.5, pch=20,
       cex=0.65, bty="n", ncol=2)
dev.off()
cat("Guardado: eda_cdmx_series.png\n")

# ============================================================
# 2. BOXPLOT POR ESTACION (ordenado por mediana)
# ============================================================
ord_est <- df %>% group_by(estacion) %>% summarise(med=median(pm25)) %>% arrange(med) %>% pull(estacion)
df$estacion <- factor(df$estacion, levels=ord_est)

png(file.path(imgdir, "eda_cdmx_boxplot_estacion.png"), width=800, height=500)
par(mfrow=c(1,1), mar=c(7,4,3,1))
boxplot(pm25 ~ estacion, data=df, outline=TRUE,
        main="Distribucion de PM2.5 por estacion — CDMX",
        xlab="", ylab="PM2.5 (ug/m3)", las=2, cex.axis=0.75,
        col="grey85", border="grey30", medcol="firebrick2", medlwd=2,
        whisklty=1, whiskcol="grey50", staplecol="grey50")
abline(h=mean(df$pm25, na.rm=TRUE), col="steelblue", lty=2, lwd=1.5)
legend("topleft", legend="Media global", col="steelblue", lty=2, lwd=1.5, bty="n", cex=0.7)
dev.off()
cat("Guardado: eda_cdmx_boxplot_estacion.png\n")

# ============================================================
# 3. HISTOGRAMA + DENSIDAD GLOBAL Y POR ESTACION
# ============================================================
png(file.path(imgdir, "eda_cdmx_histogramas.png"), width=900, height=600)
par(mfrow=c(3,3), mar=c(3,3,2,1))

# Global
hist(df$pm25, breaks=25, main="PM2.5 — Global", xlab="", ylab="",
     col="grey80", border="white", freq=FALSE)
lines(density(df$pm25, na.rm=TRUE), col="firebrick2", lwd=2)
abline(v=median(df$pm25, na.rm=TRUE), col="steelblue", lty=2, lwd=2)

# Por estacion
for (e in ord_est) {
  sub <- df[df$estacion == e, ]$pm25
  hist(sub, breaks=15, main=e, xlab="", ylab="",
       col="grey80", border="white", freq=FALSE)
  lines(density(sub, na.rm=TRUE), col="firebrick2", lwd=1.5)
  abline(v=median(sub, na.rm=TRUE), col="steelblue", lty=2, lwd=1.5)
}
dev.off()
cat("Guardado: eda_cdmx_histogramas.png\n")

# ============================================================
# 4. MATRIZ DE CORRELACION
# ============================================================
png(file.path(imgdir, "eda_cdmx_correlacion.png"), width=600, height=600)
vars_num <- df[, c("pm25","temp","hr","sen_t","cos_t")]
M <- cor(vars_num, use="complete.obs")

corr_colors <- colorRampPalette(c("steelblue","white","firebrick2"))(20)
image(1:ncol(M), 1:nrow(M), t(M)[, nrow(M):1], col=corr_colors,
      xaxt="n", yaxt="n", main="Matriz de correlacion — CDMX", cex.main=1.1)
axis(1, at=1:ncol(M), labels=colnames(M), las=2, cex.axis=0.9)
axis(2, at=1:nrow(M), labels=rev(rownames(M)), las=1, cex.axis=0.9)

for (i in 1:ncol(M)) {
  for (j in 1:nrow(M)) {
    text(i, nrow(M)-j+1, round(M[j,i], 2), cex=1.1, font=2)
  }
}
dev.off()
cat("Guardado: eda_cdmx_correlacion.png\n")

# ============================================================
# 5. SCATTER PLOTS (PM2.5 vs covariables)
# ============================================================
png(file.path(imgdir, "eda_cdmx_scatter.png"), width=900, height=450)
par(mfrow=c(1,3), mar=c(4,4,3,1))

# PM2.5 vs Temp
plot(df$temp, df$pm25, pch=20, col=rgb(0,0,0,0.3), cex=0.6,
     main="PM2.5 vs Temperatura", xlab="Temperatura (C)", ylab="PM2.5 (ug/m3)")
abline(lm(pm25 ~ temp, data=df), col="firebrick2", lwd=2)
legend("topright", legend=paste0("r = ", round(cor(df$pm25, df$temp), 3)),
       bty="n", cex=0.9)

# PM2.5 vs HR
plot(df$hr, df$pm25, pch=20, col=rgb(0,0,0,0.3), cex=0.6,
     main="PM2.5 vs Humedad Relativa", xlab="HR (%)", ylab="PM2.5 (ug/m3)")
abline(lm(pm25 ~ hr, data=df), col="firebrick2", lwd=2)
legend("topright", legend=paste0("r = ", round(cor(df$pm25, df$hr), 3)),
       bty="n", cex=0.9)

# PM2.5 vs dia del año
plot(df$dia_año, df$pm25, pch=20, col=rgb(0,0,0,0.3), cex=0.6,
     main="PM2.5 vs Dia del año", xlab="Dia del año", ylab="PM2.5 (ug/m3)")
lines(lowess(df$dia_año, df$pm25), col="firebrick2", lwd=2)
legend("topright", legend="lowess", col="firebrick2", lwd=2, bty="n", cex=0.9)

dev.off()
cat("Guardado: eda_cdmx_scatter.png\n")

# ============================================================
# 6. BOXPLOT TEMPORAL (por mes)
# ============================================================
png(file.path(imgdir, "eda_cdmx_boxplot_mes.png"), width=700, height=400)
par(mfrow=c(1,1), mar=c(4,4,3,1))
meses <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
df$mes_nombre <- meses[df$mes]
boxplot(pm25 ~ mes, data=df, outline=FALSE,
        main="PM2.5 por mes — CDMX (2023)",
        xlab="Mes", ylab="PM2.5 (ug/m3)",
        col="grey85", border="grey30", medcol="firebrick2", medlwd=2)
dev.off()
cat("Guardado: eda_cdmx_boxplot_mes.png\n")

# ============================================================
# 7. Resumen estadistico
# ============================================================
resumen <- df %>%
  group_by(estacion, alcaldia) %>%
  summarise(
    n = n(),
    pm25_mean = mean(pm25, na.rm=TRUE),
    pm25_sd = sd(pm25, na.rm=TRUE),
    pm25_med = median(pm25, na.rm=TRUE),
    pm25_min = min(pm25, na.rm=TRUE),
    pm25_max = max(pm25, na.rm=TRUE),
    temp_mean = mean(temp, na.rm=TRUE),
    hr_mean = mean(hr, na.rm=TRUE),
    .groups="drop"
  ) %>%
  arrange(desc(pm25_mean))

write.csv(resumen, file.path(imgdir, "eda_cdmx_resumen.csv"), row.names=FALSE)
cat("\n=== Resumen estadistico ===\n")
print(resumen)
cat("\nEDA completado. Imagenes en output/figures/\n")
