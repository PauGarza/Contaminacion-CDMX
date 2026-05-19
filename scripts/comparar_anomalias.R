# Leer anomalías
anom <- read.csv('output/figures/anomalias_obs_pred.csv')
cat('=== ANOMALÍAS (obs - pred) ===\n')
print(anom[order(-anom$anomalia),])

# Leer alphaj de C1
alphaj <- read.csv('output/figures/alphaj_modelo_C1.csv')
cat('\n=== ALPHAJ (ordenados) ===\n')
print(alphaj[order(-alphaj$alphaj_mean),])
