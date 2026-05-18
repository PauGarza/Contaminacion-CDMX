### ----- REGRESION AVANZADA ----- ###
# --- Modelo E v2: GP espacial (14 estaciones) --- #

options(repos="http://cran.itam.mx/")

library(R2jags)
library(terra)
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
  png(file.path(outdir, paste0("diag_cadena_", model.name, "_", param.name, "_v2.png")),
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

df <- read.csv("data/clean/pm25_valle_mexico_v2.csv", stringsAsFactors = FALSE)

logy   <- log(df$pm25)
temp.s <- scale(df$temp)[,1]
hr.s   <- scale(df$hr)[,1]
sen_t  <- df$sen_t
cos_t  <- df$cos_t
est    <- as.numeric(as.factor(df$estacion))
n <- length(logy)
J <- length(unique(est))

cat("=== Modelo E v2 ===\n")
cat("Observaciones:", n, "| Estaciones:", J, "\n")

# ============================================================
#--- Modelo C1 (base para GP espacial) ---
# ============================================================
cat("\n=== Ajustando Modelo C1 v2 (base del GP) ===\n")

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

save(ejC1, file = file.path(outdir, "modelo_E_C1base_v2.RData"))

cat("  Generando diagnosticos de cadena...\n")
diagnostico_cadena(ejC1, "alpha", outdir, "E_C1")
diagnostico_cadena(ejC1, "beta[1]", outdir, "E_C1")
diagnostico_cadena(ejC1, "sigma", outdir, "E_C1")
diagnostico_cadena(ejC1, "sigma.alphaj", outdir, "E_C1")

out.sum.C1 <- resC1$summary
out.sum.t.C1 <- out.sum.C1[grep("beta", rownames(out.sum.C1)), c(1,3,7)]
out.sum.t.C1 <- cbind(out.sum.t.C1, apply(resC1$sims.list$beta, 2, prob))
dimnames(out.sum.t.C1)[[2]][4] <- "prob"
cat("\n--- Betas C1 (base E) ---\n")
print(out.sum.t.C1)

out.alphaj <- out.sum.C1[grep("alphaj", rownames(out.sum.C1)), c(1,3,7)]
out.alphaj <- out.alphaj[1:J, ]

estaciones_ord <- sort(unique(df$estacion))
df_ef <- data.frame(
  estacion = factor(estaciones_ord, levels = estaciones_ord[order(out.alphaj[, 1])]),
  media    = out.alphaj[, 1],
  q025     = out.alphaj[, 2],
  q975     = out.alphaj[, 3]
)
p_ef <- ggplot(df_ef, aes(x = estacion, y = media)) +
  geom_hline(yintercept = 0, color = "#7F8C8D", linetype = "dashed", linewidth = 0.7) +
  geom_errorbar(aes(ymin = q025, ymax = q975),
                width = 0.3, color = "#9B59B6", linewidth = 0.7) +
  geom_point(color = "#2C3E50", size = 2.5) +
  coord_flip() +
  labs(
    title    = "Modelo E: Efectos espaciales (alphaj) — base C1",
    subtitle = "Media posterior e IC 95% — cada punto es una estación de monitoreo",
    x        = NULL,
    y        = expression(paste("Efecto espacial aditivo en log(", PM[2.5], ")"))
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle    = element_text(size = 9.5, face = "italic", color = "gray30"),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 9, color = "#2C3E50")
  )
ggsave(file.path(outdir, "efectos_espaciales_E_v2.png"),
       plot = p_ef, width = 7.5, height = 5.5, dpi = 120)

R2.C1 <- cor(logy, apply(resC1$sims.list$yf1, 2, mean))^2
cat("C1 base: DIC =", resC1$DIC, "| Pseudo-R2 =", round(R2.C1, 4), "\n")


# ============================================================
#--- Prediccion espacial (Kriging bayesiano) ---
# ============================================================
cat("\n=== Prediccion espacial (GP) ===\n")

est_coords <- df %>%
  group_by(estacion) %>%
  summarise(lon = mean(lon), lat = mean(lat), .groups = "drop")
est_coord <- as.matrix(est_coords[, c("lon", "lat")])

centroids_df <- read.csv("data/clean/centroides_valle.csv", stringsAsFactors = FALSE)
K <- nrow(centroids_df)
cent_coord <- as.matrix(centroids_df[, c("lon", "lat")])
cat("Poligonos a predecir:", K, "\n")

D_obs <- as.matrix(dist(est_coord))
D_pred_obs <- matrix(0, K, J)
for(k in 1:K) {
  for(j in 1:J) {
    D_pred_obs[k, j] <- sqrt((cent_coord[k,1] - est_coord[j,1])^2 +
                              (cent_coord[k,2] - est_coord[j,2])^2)
  }
}

rho <- 0.08
cat("Rango espacial rho:", rho, "grados (~", round(rho * 111, 1), "km)\n")

sims <- resC1$sims.list
n_sims_total <- length(sims$alpha)
set.seed(42)
idx_sims <- sample(1:n_sims_total, min(2000, n_sims_total))
n_sims <- length(idx_sims)
cat("Muestras MCMC usadas:", n_sims, "\n")

alpha_s  <- sims$alpha[idx_sims]
beta_s   <- sims$beta[idx_sims, ]
alphaj_s <- sims$alphaj[idx_sims, ]

temp_bar_s <- 0  # covariables en su media: estandarizada = 0
hr_bar_s   <- 0
sen_bar    <- mean(df$sen_t)
cos_bar    <- mean(df$cos_t)

Sigma_obs_obs     <- exp(-D_obs / rho)
Sigma_obs_obs_inv <- solve(Sigma_obs_obs)
Sigma_pred_obs    <- exp(-D_pred_obs / rho)

logy_pred <- matrix(0, K, n_sims)
for(m in 1:n_sims) {
  w_pred    <- Sigma_pred_obs %*% (Sigma_obs_obs_inv %*% alphaj_s[m, ])
  mu_fixed  <- alpha_s[m] + beta_s[m,1]*temp_bar_s + beta_s[m,2]*hr_bar_s +
               beta_s[m,3]*sen_bar + beta_s[m,4]*cos_bar
  logy_pred[, m] <- mu_fixed + w_pred[, 1]
}

pm25_mean  <- exp(rowMeans(logy_pred))
pm25_sd    <- exp(apply(logy_pred, 1, sd))
pm25_q2.5  <- exp(apply(logy_pred, 1, quantile, probs = 0.025))
pm25_q97.5 <- exp(apply(logy_pred, 1, quantile, probs = 0.975))

pred_df <- data.frame(
  name = centroids_df$name,
  name_1 = centroids_df$name_1,
  lon = centroids_df$lon,
  lat = centroids_df$lat,
  pm25_mean = pm25_mean,
  pm25_sd = pm25_sd,
  pm25_q2.5 = pm25_q2.5,
  pm25_q97.5 = pm25_q97.5,
  stringsAsFactors = FALSE
)

write.csv(pred_df, file.path(outdir, "prediccion_espacial_E_valle_v2.csv"), row.names = FALSE)
cat("Predicciones guardadas:\n")


# ============================================================
#--- Mapas ---
# ============================================================
cat("\n=== Generando mapas ===\n")

mex <- vect("data/gadm_mexico/gadm41_MEX_2.shp")
valle <- mex[mex$NAME_1 %in% c("Distrito Federal", "México"), ]

pred_order <- match(valle$NAME_2, pred_df$name)
valle$pm25_mean <- pred_df$pm25_mean[pred_order]
valle$pm25_q2.5 <- pred_df$pm25_q2.5[pred_order]
valle$pm25_q97.5 <- pred_df$pm25_q97.5[pred_order]

# Cortes adaptados al rango real de predicciones (intervalos de 0.5 ug/m3)
pred_min <- floor(min(pred_df$pm25_mean, na.rm = TRUE) * 2) / 2
pred_max <- ceiling(max(pred_df$pm25_mean, na.rm = TRUE) * 2) / 2
cats   <- seq(pred_min, pred_max, by = 0.5)
n_cats <- length(cats) - 1
colors <- colorRampPalette(c("#ffffcc", "#fd8d3c", "#b10026"))(n_cats)
valle$cat <- cut(valle$pm25_mean, breaks = cats, include.lowest = TRUE)

est_unique <- aggregate(cbind(lon, lat) ~ estacion, data = df, FUN = mean)
est_unique <- est_unique[order(est_unique$estacion), ]
est_unique$num <- 1:nrow(est_unique)

png(file.path(outdir, "mapa_prediccion_E_valle_v2.png"), width = 1400, height = 1000, res = 120)
par(oma = c(0, 0, 2, 0), mar = c(2, 2, 1, 9))
plot(valle, "cat", col = colors)
mtext("PM2.5 predicho — Modelo E v2 (GP espacial, 14 estaciones)",
      outer = TRUE, side = 3, line = 0.5, font = 2, cex = 1.05, col = "#2C3E50")
points(est_unique$lon, est_unique$lat, pch = 21, bg = "white", col = "black", cex = 2, lwd = 2)
text(est_unique$lon, est_unique$lat, labels = est_unique$num,
     pos = 3, cex = 0.9, col = "black", font = 2, offset = 0.5)
legend("right", inset = c(-0.04, 0),
       legend = paste0(est_unique$num, ". ", est_unique$estacion),
       title = "Estaciones", bg = "white", cex = 0.7, xpd = TRUE, bty = "n")
legend("bottomleft", inset = c(0.06, 0.02), legend = levels(valle$cat), fill = colors,
       title = "PM2.5 (µg/m³)", bg = "white", cex = 0.85)
dev.off()
cat("Mapa de prediccion guardado\n")


# ============================================================
#--- Resumenes ---
# ============================================================
cat("\n=== Resumen ===\n")
resumen_estado <- aggregate(pm25_mean ~ name_1, data = pred_df, FUN = function(x) 
  c(mean = mean(x), sd = sd(x), min = min(x), max = max(x)))
print(resumen_estado)

cat("\n--- Top 10 mas contaminados ---\n")
print(head(pred_df[order(-pred_df$pm25_mean), c("name", "name_1", "pm25_mean", "pm25_q2.5", "pm25_q97.5")], 10))

cat("\n--- Top 10 menos contaminados ---\n")
print(head(pred_df[order(pred_df$pm25_mean), c("name", "name_1", "pm25_mean", "pm25_q2.5", "pm25_q97.5")], 10))

cat("\n--- Rango total ---\n")
cat("Min:", round(min(pred_df$pm25_mean), 1), "| Max:", round(max(pred_df$pm25_mean), 1), 
    "| SD entre poligonos:", round(sd(pred_df$pm25_mean), 2), "\n")
