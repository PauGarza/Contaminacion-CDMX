# Comparacion de predicciones espaciales: archivo (10 est.) vs. actual (14 est.)
pred_10est <- read.csv('archive/output/figures/prediccion_espacial_E_valle.csv')
pred_14est <- read.csv('output/figures/prediccion_espacial_E_valle.csv')

# Estadísticas descriptivas
cat('=== MODELO E archivo (10 estaciones) ===\n')
cat('Media:', mean(pred_10est$pm25_mean), '\n')
cat('Mediana:', median(pred_10est$pm25_mean), '\n')
cat('Min:', min(pred_10est$pm25_mean), '\n')
cat('Max:', max(pred_10est$pm25_mean), '\n')
cat('Rango:', max(pred_10est$pm25_mean) - min(pred_10est$pm25_mean), '\n')
cat('SD:', sd(pred_10est$pm25_mean), '\n')
cat('CV:', sd(pred_10est$pm25_mean)/mean(pred_10est$pm25_mean)*100, '%\n\n')

cat('=== MODELO E actual (14 estaciones) ===\n')
cat('Media:', mean(pred_14est$pm25_mean), '\n')
cat('Mediana:', median(pred_14est$pm25_mean), '\n')
cat('Min:', min(pred_14est$pm25_mean), '\n')
cat('Max:', max(pred_14est$pm25_mean), '\n')
cat('Rango:', max(pred_14est$pm25_mean) - min(pred_14est$pm25_mean), '\n')
cat('SD:', sd(pred_14est$pm25_mean), '\n')
cat('CV:', sd(pred_14est$pm25_mean)/mean(pred_14est$pm25_mean)*100, '%\n\n')

# Top 10 más contaminados
cat('=== TOP 10 MÁS CONTAMINADOS (archivo) ===\n')
print(head(pred_10est[order(-pred_10est$pm25_mean), c('name', 'pm25_mean')], 10))

cat('\n=== TOP 10 MÁS CONTAMINADOS (actual) ===\n')
print(head(pred_14est[order(-pred_14est$pm25_mean), c('name', 'pm25_mean')], 10))

# Top 10 más limpios
cat('\n=== TOP 10 MÁS LIMPIOS (archivo) ===\n')
print(head(pred_10est[order(pred_10est$pm25_mean), c('name', 'pm25_mean')], 10))

cat('\n=== TOP 10 MÁS LIMPIOS (actual) ===\n')
print(head(pred_14est[order(pred_14est$pm25_mean), c('name', 'pm25_mean')], 10))

# Comparar diferencias polígono por polígono
merged <- merge(pred_10est[, c('name', 'pm25_mean')], pred_14est[, c('name', 'pm25_mean')],
                by = 'name', suffixes = c('_10est', '_14est'))
merged$diff    <- merged$pm25_mean_14est - merged$pm25_mean_10est
merged$pct_diff <- merged$diff / merged$pm25_mean_10est * 100

cat('\n=== DIFERENCIA actual - archivo (mayores caídas) ===\n')
print(head(merged[order(merged$diff), c('name', 'pm25_mean_10est', 'pm25_mean_14est', 'diff', 'pct_diff')], 10))

cat('\n=== DIFERENCIA actual - archivo (mayores aumentos) ===\n')
print(head(merged[order(-merged$diff), c('name', 'pm25_mean_10est', 'pm25_mean_14est', 'diff', 'pct_diff')], 10))

cat('\n=== ESTADÍSTICAS DE LA DIFERENCIA ===\n')
cat('Media diff:', mean(merged$diff), '\n')
cat('SD diff:', sd(merged$diff), '\n')
cat('Min diff:', min(merged$diff), '\n')
cat('Max diff:', max(merged$diff), '\n')
