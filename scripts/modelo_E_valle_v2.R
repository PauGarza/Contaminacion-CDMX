# ============================================================
# Modelo E — GP espacial reentrenado con datos v2 (14 estaciones)
# ============================================================
library(R2jags)
library(terra)
library(dplyr)

wdir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/"
setwd(wdir)
outdir <- "output/figures"

# 1. Cargar datos v2
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

# 2. Ajustar C1 v2 (si no existe RData previo)
cat("\n--- Ajustando Modelo C1 v2 ---\n")
data.C1 <- list(n = n, J = J, logy = logy, temp = temp.s, hr = hr.s, sen_t = sen_t, cos_t = cos_t, est = est)
inits.C1 <- function() list(alpha = 0, beta = rep(0,4), alphaj = rep(0,J), tau = 1, tau.alphaj = 1)
pars.C1 <- c("alpha", "beta", "alphaj", "tau", "sigma", "tau.alphaj", "sigma.alphaj", "yf1")
ejC1 <- jags(data.C1, inits.C1, pars.C1, model.file = "scripts/jags_modelo_C1_valle.txt",
             n.iter = 12000, n.chains = 2, n.burnin = 2000, n.thin = 2)
resC1 <- ejC1$BUGSoutput
R2.C1 <- cor(logy, apply(resC1$sims.list$yf1, 2, mean))^2
cat("C1 v2: DIC =", resC1$DIC, "| Pseudo-R2 =", round(R2.C1, 4), "\n")

# Guardar C1 v2
save(resC1, file = file.path(outdir, "modelo_C1_v2.RData"))

# 3. Coordenadas estaciones (promedio por estacion)
est_coords <- df %>%
  group_by(estacion) %>%
  summarise(lon = mean(lon), lat = mean(lat), .groups = "drop")
est_coord <- as.matrix(est_coords[, c("lon", "lat")])

# 4. Cargar centroides de poligonos
centroids_df <- read.csv("data/clean/centroides_valle.csv", stringsAsFactors = FALSE)
K <- nrow(centroids_df)
cat("Poligonos a predecir:", K, "\n")

cent_coord <- as.matrix(centroids_df[, c("lon", "lat")])

# 5. Matrices de distancia
D_obs <- as.matrix(dist(est_coord))
D_pred_obs <- matrix(0, K, J)
for(k in 1:K) {
  for(j in 1:J) {
    D_pred_obs[k, j] <- sqrt((cent_coord[k,1] - est_coord[j,1])^2 + 
                              (cent_coord[k,2] - est_coord[j,2])^2)
  }
}

# 6. Parametros kernel
rho <- 0.08
cat("Rango espacial rho:", rho, "grados (~", round(rho * 111, 1), "km)\n")

# 7. Muestras posteriores
sims <- resC1$sims.list
n_sims_total <- length(sims$alpha)
set.seed(42)
idx_sims <- sample(1:n_sims_total, min(2000, n_sims_total))
n_sims <- length(idx_sims)
cat("Muestras MCMC usadas:", n_sims, "\n")

alpha_s <- sims$alpha[idx_sims]
beta_s <- sims$beta[idx_sims, ]
alphaj_s <- sims$alphaj[idx_sims, ]

# 8. Covariables promedio (usar promedios globales v2)
temp_bar <- mean(df$temp)
hr_bar <- mean(df$hr)
sen_bar <- mean(df$sen_t)
cos_bar <- mean(df$cos_t)

temp_mean <- mean(df$temp)
temp_sd <- sd(df$temp)
hr_mean <- mean(df$hr)
hr_sd <- sd(df$hr)

temp_bar_s <- (temp_bar - temp_mean) / temp_sd
hr_bar_s <- (hr_bar - hr_mean) / hr_sd

cat("Covariables promedio: temp=", round(temp_bar,2), " hr=", round(hr_bar,2),
    " sen=", round(sen_bar,4), " cos=", round(cos_bar,4), "\n")

# 9. Kriging bayesiano
Sigma_obs_obs <- exp(-D_obs / rho)
Sigma_obs_obs_inv <- solve(Sigma_obs_obs)
Sigma_pred_obs <- exp(-D_pred_obs / rho)

logy_pred <- matrix(0, K, n_sims)

for(m in 1:n_sims) {
  w_pred <- Sigma_pred_obs %*% (Sigma_obs_obs_inv %*% alphaj_s[m, ])
  mu_fixed <- alpha_s[m] + 
              beta_s[m, 1] * temp_bar_s + 
              beta_s[m, 2] * hr_bar_s + 
              beta_s[m, 3] * sen_bar + 
              beta_s[m, 4] * cos_bar
  logy_pred[, m] <- mu_fixed + w_pred[, 1]
}

# 10. Resumen posterior
pm25_mean <- exp(rowMeans(logy_pred))
pm25_sd <- exp(apply(logy_pred, 1, sd))
pm25_q2.5 <- exp(apply(logy_pred, 1, quantile, probs = 0.025))
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
cat("\nPredicciones guardadas:", file.path(outdir, "prediccion_espacial_E_valle_v2.csv"), "\n")

# 11. Mapa
mex <- vect("data/gadm_mexico/gadm41_MEX_2.shp")
valle <- mex[mex$NAME_1 %in% c("Distrito Federal", "México"), ]

pred_order <- match(valle$NAME_2, pred_df$name)
valle$pm25_mean <- pred_df$pm25_mean[pred_order]
valle$pm25_q2.5 <- pred_df$pm25_q2.5[pred_order]
valle$pm25_q97.5 <- pred_df$pm25_q97.5[pred_order]

cats <- c(12, 14, 16, 18, 20, 22, 24, 26, 50)
valle$cat <- cut(valle$pm25_mean, breaks = cats, include.lowest = TRUE)

colors <- c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c",
            "#fc4e2a", "#e31a1c", "#b10026")

png(file.path(outdir, "mapa_prediccion_E_valle_v2.png"), width = 1200, height = 900)
plot(valle, "cat", col = colors, main = "PM2.5 predicho — Modelo E v2 (GP espacial, 14 estaciones)")
# Puntos estaciones v2
est_unique <- aggregate(cbind(lon, lat, pm25) ~ estacion, data = df, FUN = mean)
points(est_unique$lon, est_unique$lat, pch = 21, bg = "white", col = "black", cex = 2, lwd = 2)
text(est_unique$lon, est_unique$lat, labels = est_unique$estacion, pos = 3, cex = 0.6, col = "black")
legend("bottomleft", legend = levels(valle$cat), fill = colors,
       title = "PM2.5 (µg/m³)", bg = "white")
dev.off()
cat("Mapa guardado:", file.path(outdir, "mapa_prediccion_E_valle_v2.png"), "\n")

# 12. Resumenes
cat("\n--- Resumen por estado ---\n")
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
