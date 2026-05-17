# Leer datos
v1 <- read.csv('archive/output/figures/prediccion_espacial_E_valle.csv')
v2 <- read.csv('output/figures/prediccion_espacial_E_valle_v2.csv')

# Estadísticas descriptivas
cat('=== MODELO E v1 (10 estaciones) ===\n')
cat('Media:', mean(v1$pm25_mean), '\n')
cat('Mediana:', median(v1$pm25_mean), '\n')
cat('Min:', min(v1$pm25_mean), '\n')
cat('Max:', max(v1$pm25_mean), '\n')
cat('Rango:', max(v1$pm25_mean) - min(v1$pm25_mean), '\n')
cat('SD:', sd(v1$pm25_mean), '\n')
cat('CV:', sd(v1$pm25_mean)/mean(v1$pm25_mean)*100, '%\n\n')

cat('=== MODELO E v2 (14 estaciones) ===\n')
cat('Media:', mean(v2$pm25_mean), '\n')
cat('Mediana:', median(v2$pm25_mean), '\n')
cat('Min:', min(v2$pm25_mean), '\n')
cat('Max:', max(v2$pm25_mean), '\n')
cat('Rango:', max(v2$pm25_mean) - min(v2$pm25_mean), '\n')
cat('SD:', sd(v2$pm25_mean), '\n')
cat('CV:', sd(v2$pm25_mean)/mean(v2$pm25_mean)*100, '%\n\n')

# Top 10 más contaminados v1
cat('=== TOP 10 MÁS CONTAMINADOS v1 ===\n')
print(head(v1[order(-v1$pm25_mean), c('name', 'pm25_mean')], 10))

cat('\n=== TOP 10 MÁS CONTAMINADOS v2 ===\n')
print(head(v2[order(-v2$pm25_mean), c('name', 'pm25_mean')], 10))

# Top 10 más limpios v1
cat('\n=== TOP 10 MÁS LIMPIOS v1 ===\n')
print(head(v1[order(v1$pm25_mean), c('name', 'pm25_mean')], 10))

cat('\n=== TOP 10 MÁS LIMPIOS v2 ===\n')
print(head(v2[order(v2$pm25_mean), c('name', 'pm25_mean')], 10))

# Comparar diferencias polígono por polígono
merged <- merge(v1[, c('name', 'pm25_mean')], v2[, c('name', 'pm25_mean')], by='name', suffixes=c('_v1', '_v2'))
merged$diff <- merged$pm25_mean_v2 - merged$pm25_mean_v1
merged$pct_diff <- merged$diff / merged$pm25_mean_v1 * 100

cat('\n=== DIFERENCIA v2 - v1 (mayores caídas) ===\n')
print(head(merged[order(merged$diff), c('name', 'pm25_mean_v1', 'pm25_mean_v2', 'diff', 'pct_diff')], 10))

cat('\n=== DIFERENCIA v2 - v1 (mayores aumentos) ===\n')
print(head(merged[order(-merged$diff), c('name', 'pm25_mean_v1', 'pm25_mean_v2', 'diff', 'pct_diff')], 10))

cat('\n=== ESTADÍSTICAS DE LA DIFERENCIA ===\n')
cat('Media diff:', mean(merged$diff), '\n')
cat('SD diff:', sd(merged$diff), '\n')
cat('Min diff:', min(merged$diff), '\n')
cat('Max diff:', max(merged$diff), '\n')
