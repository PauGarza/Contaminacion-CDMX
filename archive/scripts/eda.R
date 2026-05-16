options(repos="http://cran.itam.mx/")
library(dplyr)
library(ggplot2)
library(tidyr)

# ============================================================
# ANALISIS EXPLORATORIO DE DATOS (EDA)
# ============================================================

clean_dir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/clean"
out_dir <- "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/output/figures"

# Leer datos limpios
df <- read.csv(file.path(clean_dir, "pm25_clean.csv"), stringsAsFactors=FALSE)
df$date <- as.Date(df$date)

# Paleta de colores
pal_ciudad <- c("cdmx"="firebrick2", "gdl"="steelblue", "mty"="forestgreen")

# ============================================================
# 1. Serie de tiempo de PM2.5 promedio por ciudad
# ============================================================

df_ciudad_dia <- df %>%
  group_by(date, ciudad) %>%
  summarise(pm25_mean = mean(pm25, na.rm=TRUE), .groups="drop")

p1 <- ggplot(df_ciudad_dia, aes(x=date, y=pm25_mean, color=ciudad)) +
  geom_line(alpha=0.8, linewidth=0.7) +
  scale_color_manual(values=pal_ciudad) +
  labs(title="PM2.5 promedio diario por ciudad (2023)",
       x="Fecha", y="PM2.5 (ug/m3)", color="Ciudad") +
  theme_bw() +
  theme(legend.position="top")

ggsave(file.path(out_dir, "a_serie_tiempo_ciudad.png"), p1, width=10, height=5, dpi=150)
print("Guardado: a_serie_tiempo_ciudad.png")

# ============================================================
# 2. Boxplot de PM2.5 por mes y ciudad
# ============================================================

df$mes_nombre <- factor(df$mes, levels=1:12,
                        labels=c("Ene","Feb","Mar","Abr","May","Jun",
                                 "Jul","Ago","Sep","Oct","Nov","Dic"))

p2 <- ggplot(df, aes(x=mes_nombre, y=pm25, fill=ciudad)) +
  geom_boxplot(alpha=0.8, outlier.size=0.8) +
  scale_fill_manual(values=pal_ciudad) +
  labs(title="Distribucion de PM2.5 por mes y ciudad",
       x="Mes", y="PM2.5 (ug/m3)", fill="Ciudad") +
  theme_bw() +
  theme(legend.position="top")

ggsave(file.path(out_dir, "b_boxplot_mes_ciudad.png"), p2, width=10, height=5, dpi=150)
print("Guardado: b_boxplot_mes_ciudad.png")

# ============================================================
# 3. Boxplot de PM2.5 por estacion dentro de cada ciudad
# ============================================================

p3 <- ggplot(df, aes(x=reorder(estacion, pm25, median, na.rm=TRUE), y=pm25, fill=ciudad)) +
  geom_boxplot(alpha=0.8, outlier.size=0.8) +
  scale_fill_manual(values=pal_ciudad) +
  facet_wrap(~ciudad, scales="free_x") +
  labs(title="Distribucion de PM2.5 por estacion de monitoreo",
       x="Estacion", y="PM2.5 (ug/m3)") +
  theme_bw() +
  theme(legend.position="none", axis.text.x=element_text(angle=45, hjust=1))

ggsave(file.path(out_dir, "c_boxplot_estacion.png"), p3, width=12, height=5, dpi=150)
print("Guardado: c_boxplot_estacion.png")

# ============================================================
# 4. Histograma de log(PM2.5)
# ============================================================

p4 <- ggplot(df %>% filter(pm25 > 0), aes(x=log(pm25))) +
  geom_histogram(aes(y=after_stat(density)), bins=30, fill="grey70", color="white") +
  geom_density(color="firebrick2", linewidth=1) +
  labs(title="Distribucion de log(PM2.5)",
       x="log(PM2.5)", y="Densidad") +
  theme_bw()

ggsave(file.path(out_dir, "d_hist_logpm25.png"), p4, width=7, height=5, dpi=150)
print("Guardado: d_hist_logpm25.png")

# ============================================================
# 5. Mapa de estaciones con PM2.5 promedio
# ============================================================

df_est <- df %>%
  group_by(estacion, ciudad, lat, lon) %>%
  summarise(pm25_mean = mean(pm25, na.rm=TRUE), .groups="drop")

# Mapa simple con puntos
p5 <- ggplot(df_est, aes(x=lon, y=lat)) +
  borders("world", regions="Mexico", colour="grey80", fill="grey95") +
  geom_point(aes(size=pm25_mean, color=ciudad), alpha=0.8) +
  scale_color_manual(values=pal_ciudad) +
  coord_fixed(xlim=c(-105, -98), ylim=c(19, 27)) +
  labs(title="Estaciones de monitoreo y PM2.5 promedio (2023)",
       x="Longitud", y="Latitud", size="PM2.5 promedio", color="Ciudad") +
  theme_bw()

ggsave(file.path(out_dir, "e_mapa_estaciones.png"), p5, width=8, height=7, dpi=150)
print("Guardado: e_mapa_estaciones.png")

# ============================================================
# 6. Matriz de correlacion
# ============================================================

df_corr <- df %>% select(pm25, temp, hr) %>% drop_na()
M <- cor(df_corr)

png(file.path(out_dir, "f_correlacion.png"), width=600, height=500)
corrplot::corrplot(M, method="color", type="upper", order="original",
                   addCoef.col="black", tl.col="black", tl.srt=45,
                   title="Correlacion entre variables", mar=c(0,0,1,0))
dev.off()
print("Guardado: f_correlacion.png")

# ============================================================
# 7. PM2.5 vs covariables
# ============================================================

df_long <- df %>%
  select(date, ciudad, pm25, temp, hr) %>%
  pivot_longer(cols=c(temp, hr), names_to="variable", values_to="valor")

p7 <- ggplot(df_long %>% drop_na(), aes(x=valor, y=pm25, color=ciudad)) +
  geom_point(alpha=0.3, size=0.8) +
  geom_smooth(method="lm", se=FALSE, linewidth=0.8) +
  scale_color_manual(values=pal_ciudad) +
  facet_wrap(~variable, scales="free_x") +
  labs(title="PM2.5 vs covariables meteorologicas",
       x="Valor", y="PM2.5 (ug/m3)", color="Ciudad") +
  theme_bw()

ggsave(file.path(out_dir, "g_pm25_vs_covariables.png"), p7, width=10, height=5, dpi=150)
print("Guardado: g_pm25_vs_covariables.png")

# ============================================================
# Tabla descriptiva
# ============================================================

desc <- df %>%
  group_by(ciudad) %>%
  summarise(
    n_dias = n(),
    pm25_mean = round(mean(pm25, na.rm=TRUE), 2),
    pm25_sd = round(sd(pm25, na.rm=TRUE), 2),
    pm25_min = round(min(pm25, na.rm=TRUE), 2),
    pm25_max = round(max(pm25, na.rm=TRUE), 2),
    temp_mean = round(mean(temp, na.rm=TRUE), 2),
    hr_mean = round(mean(hr, na.rm=TRUE), 2),
    .groups="drop"
  )

write.csv(desc, file.path(clean_dir, "tabla_descriptiva.csv"), row.names=FALSE)
print("Tabla descriptiva guardada.")
print(desc)

print("EDA completado.")
