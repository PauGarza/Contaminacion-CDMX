# Leer anomalías v2
anom_v2 <- read.csv('output/figures/anomalias_obs_pred_v2.csv')
cat('=== ANOMALÍAS v2 (obs - pred) ===\n')
print(anom_v2[order(-anom_v2$anomalia),])

# Leer alphaj de C1 v2
alphaj_v2 <- read.csv('output/figures/alphaj_modelo_C1_v2.csv')
cat('\n=== ALPHAJ v2 (ordenados) ===\n')
print(alphaj_v2[order(-alphaj_v2$alphaj_mean),])
