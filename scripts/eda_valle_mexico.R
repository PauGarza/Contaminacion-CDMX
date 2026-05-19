options(repos="http://cran.itam.mx/")
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(corrplot)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)

# ============================================================
# EDA — Exploracion del dataset expandido
# ============================================================

df <- read.csv("data/clean/pm25_valle_mexico.csv", stringsAsFactors=FALSE)
df$date <- as.Date(df$date)

cat("=== EDA: Valle de Mexico expandido ===\n")
cat("Observaciones:", nrow(df), "\n")
cat("Estaciones:", length(unique(df$estacion)), "\n")
cat("Periodo:", min(df$date), "a", max(df$date), "\n")

# Resumen por estacion
resumen <- df %>%
  group_by(estacion, ciudad, municipio, lat, lon) %>%
  summarise(
    n = n(),
    pm25_mean = mean(pm25),
    pm25_sd = sd(pm25),
    pm25_min = min(pm25),
    pm25_max = max(pm25),
    temp_mean = mean(temp),
    hr_mean = mean(hr),
    .groups = "drop"
  ) %>%
  arrange(desc(pm25_mean))

cat("\n--- Top 10 estaciones mas contaminadas ---\n")
print(head(resumen[, c("estacion", "municipio", "ciudad", "n", "pm25_mean")], 10))

cat("\n--- Top 10 estaciones menos contaminadas ---\n")
print(tail(resumen[, c("estacion", "municipio", "ciudad", "n", "pm25_mean")], 10))

# Comparacion CDMX vs Edomex vs Hidalgo
comp_ciudad <- df %>%
  group_by(ciudad) %>%
  summarise(
    n = n(),
    estaciones = n_distinct(estacion),
    pm25_mean = mean(pm25),
    pm25_sd = sd(pm25),
    temp_mean = mean(temp),
    hr_mean = mean(hr),
    .groups = "drop"
  )
cat("\n--- Comparacion por ciudad ---\n")
print(comp_ciudad)

# Serie temporal: promedio diario
daily_mean <- df %>%
  group_by(date) %>%
  summarise(pm25 = mean(pm25), temp = mean(temp), hr = mean(hr), .groups="drop")

daily_long <- daily_mean %>%
  tidyr::pivot_longer(cols = c(pm25, temp, hr),
                      names_to  = "variable",
                      values_to = "valor") %>%
  mutate(variable = factor(variable,
                           levels = c("pm25", "temp", "hr"),
                           labels = c("PM2.5 (µg/m³)", "Temperatura (°C)", "Humedad relativa (%)")))
p_series <- ggplot(daily_long, aes(x = date, y = valor, color = variable)) +
  geom_line(linewidth = 0.7, lineend = "round") +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("PM2.5 (µg/m³)" = "#E74C3C",
                                "Temperatura (°C)" = "#3498DB",
                                "Humedad relativa (%)" = "#2ECC71")) +
  labs(title    = "Series temporales diarias — Valle de México 2023",
       subtitle = "Promedio de las 14 estaciones de monitoreo",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position    = "none",
        plot.title         = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle      = element_text(size = 9.5, face = "italic", color = "gray30"),
        panel.grid.minor   = element_blank(),
        strip.text         = element_text(face = "bold", size = 10))
ggsave("output/figures/eda_valle_series.png", plot = p_series,
       width = 12, height = 4, dpi = 120)
cat("\nGuardado: output/figures/eda_valle_series.png\n")

# Boxplot por estacion (ordenadas por PM2.5)
est_ord <- resumen$estacion[order(resumen$pm25_mean)]
df$estacion <- factor(df$estacion, levels=est_ord)

media_global <- mean(df$pm25)
p_box_est <- ggplot(df, aes(x = estacion, y = pm25, fill = ciudad)) +
  geom_boxplot(color = "#2C3E50", alpha = 0.75, outlier.alpha = 0.3, outlier.size = 0.8) +
  geom_hline(yintercept = media_global, color = "#E74C3C", linetype = "dashed", linewidth = 0.8) +
  scale_fill_manual(values = c(cdmx = "#3498DB", edomex = "#E74C3C", hidalgo = "#2ECC71"),
                    labels = c(cdmx = "CDMX", edomex = "Edo. Méx.", hidalgo = "Hidalgo")) +
  annotate("text", x = 14.4, y = media_global + 0.5,
           label = paste0("Global: ", round(media_global, 1), " µg/m³"),
           color = "#E74C3C", size = 3, hjust = 1) +
  coord_flip() +
  labs(title    = "PM2.5 por estación — Valle de México 2023",
       subtitle = "Estaciones ordenadas por PM2.5 promedio anual",
       x = NULL,
       y = expression(paste(PM[2.5], " (", mu, "g/", m^3, ")")),
       fill = "Entidad") +
  theme_minimal() +
  theme(plot.title       = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle    = element_text(size = 9.5, face = "italic", color = "gray30"),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold", size = 9))
ggsave("output/figures/eda_valle_boxplot_estacion.png", plot = p_box_est,
       width = 10, height = 7, dpi = 120)
cat("Guardado: output/figures/eda_valle_boxplot_estacion.png\n")

# Boxplot por mes
meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
df$mes_nombre <- factor(meses_esp[df$mes], levels=meses_esp)
p_box_mes <- ggplot(df, aes(x = mes_nombre, y = pm25)) +
  geom_boxplot(fill = "#3498DB", color = "#2C3E50", alpha = 0.7,
               outlier.alpha = 0.3, outlier.size = 0.8) +
  labs(title    = "PM2.5 por mes — Valle de México 2023",
       subtitle = "Distribución mensual agregada de las 14 estaciones",
       x = NULL,
       y = expression(paste(PM[2.5], " (", mu, "g/", m^3, ")"))) +
  theme_minimal() +
  theme(plot.title       = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle    = element_text(size = 9.5, face = "italic", color = "gray30"),
        panel.grid.minor = element_blank())
ggsave("output/figures/eda_valle_boxplot_mes.png", plot = p_box_mes,
       width = 9, height = 5, dpi = 120)
cat("Guardado: output/figures/eda_valle_boxplot_mes.png\n")

# Histogramas
df_hist <- data.frame(
  pm25     = df$pm25,
  log_pm25 = log(df$pm25),
  temp     = df$temp,
  hr       = df$hr
) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  mutate(variable = factor(variable,
                           levels = c("pm25","log_pm25","temp","hr"),
                           labels = c("PM2.5 (µg/m³)","log(PM2.5)","Temperatura (°C)","Humedad relativa (%)")))
p_hist <- ggplot(df_hist, aes(x = valor)) +
  geom_histogram(fill = "#3498DB", color = "white", bins = 30) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  labs(title    = "Distribución de variables — Valle de México 2023",
       subtitle = "Histogramas con 30 intervalos",
       x = NULL, y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title       = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle    = element_text(size = 9.5, face = "italic", color = "gray30"),
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold", size = 9))
ggsave("output/figures/eda_valle_histogramas.png", plot = p_hist,
       width = 12, height = 4, dpi = 120)
cat("Guardado: output/figures/eda_valle_histogramas.png\n")

# Correlacion — estilo Otho
vars_cor <- c("temp", "hr", "sen_t", "cos_t", "pm25")
labs_cor <- c("Temperatura", "Humedad rel.", "Sen estacional", "Cos estacional", "PM2.5")
cor_mat  <- cor(df[, vars_cor], use = "complete.obs")
rownames(cor_mat) <- colnames(cor_mat) <- labs_cor

cor_long_eda <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
names(cor_long_eda) <- c("var1", "var2", "corr")
cor_long_eda <- cor_long_eda[cor_long_eda$var1 != cor_long_eda$var2, ]
cor_long_eda$var1      <- factor(cor_long_eda$var1, levels = rev(labs_cor))
cor_long_eda$var2      <- factor(cor_long_eda$var2, levels = labs_cor)
cor_long_eda$txt_color <- ifelse(abs(cor_long_eda$corr) >= 0.35, "white", "#2C3E50")

p_cor <- ggplot(cor_long_eda, aes(x = var2, y = var1, fill = corr)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.2f", corr), color = txt_color),
            size = 4, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient2(low = "#E74C3C", mid = "#F7F7F7", high = "#3498DB",
                       midpoint = 0, limits = c(-1, 1), name = "Correlación\nde Pearson") +
  coord_fixed() +
  labs(title    = "Matriz de correlación — Valle de México 2023",
       subtitle = "Correlación de Pearson entre PM2.5 y covariables climáticas",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title   = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle = element_text(size = 9.5, face = "italic", color = "gray30"),
        panel.grid   = element_blank(),
        axis.text.x  = element_text(face = "bold", size = 10, color = "#2C3E50",
                                    angle = 30, hjust = 1),
        axis.text.y  = element_text(face = "bold", size = 10, color = "#2C3E50"),
        legend.title = element_text(face = "bold", size = 9),
        legend.position = "right")
ggsave("output/figures/eda_valle_correlacion.png", plot = p_cor,
       width = 6.5, height = 5.5, dpi = 120)
cat("Guardado: output/figures/eda_valle_correlacion.png\n")

# Scatter con lowess — estilo Otho
df_sc <- data.frame(temp = df$temp, hr = df$hr, dia = df$dia_año, pm25 = df$pm25) %>%
  tidyr::pivot_longer(cols = c(temp, hr, dia), names_to = "cov", values_to = "x") %>%
  mutate(cov = factor(cov,
                      levels = c("temp","hr","dia"),
                      labels = c("Temperatura (°C)","Humedad relativa (%)","Día del año")))
p_scatter <- ggplot(df_sc, aes(x = x, y = pm25)) +
  geom_point(color = "#7F8C8D", alpha = 0.20, size = 0.8) +
  geom_smooth(method = "loess", color = "#E74C3C", fill = "#E74C3C",
              alpha = 0.20, se = TRUE, linewidth = 1.0) +
  facet_wrap(~cov, scales = "free_x", ncol = 3) +
  labs(title    = expression(paste(PM[2.5], " vs covariables — Valle de México 2023")),
       subtitle = "Puntos observados con curva LOESS",
       x = NULL,
       y = expression(paste(PM[2.5], " (", mu, "g/", m^3, ")"))) +
  theme_minimal() +
  theme(plot.title       = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle    = element_text(size = 9.5, face = "italic", color = "gray30"),
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold", size = 10))
ggsave("output/figures/eda_valle_scatter.png", plot = p_scatter,
       width = 12, height = 4.5, dpi = 120)
cat("Guardado: output/figures/eda_valle_scatter.png\n")

# Scatter log(PM2.5) vs covariables — motiva la transformación log
df_scl <- data.frame(temp = df$temp, hr = df$hr, dia = df$dia_año, logpm25 = log(df$pm25)) %>%
  tidyr::pivot_longer(cols = c(temp, hr, dia), names_to = "cov", values_to = "x") %>%
  mutate(cov = factor(cov,
                      levels = c("temp","hr","dia"),
                      labels = c("Temperatura (°C)","Humedad relativa (%)","Día del año")))
p_scl <- ggplot(df_scl, aes(x = x, y = logpm25)) +
  geom_point(color = "#7F8C8D", alpha = 0.20, size = 0.8) +
  geom_smooth(method = "loess", color = "#E74C3C", fill = "#E74C3C",
              alpha = 0.20, se = TRUE, linewidth = 1.0) +
  facet_wrap(~cov, scales = "free_x", ncol = 3) +
  labs(title    = "log(PM2.5) vs covariables — motiva transformación logarítmica",
       subtitle = "La relación es más lineal en escala log — justifica el modelo Normal en log(PM2.5)",
       x = NULL, y = "log(PM2.5)") +
  theme_minimal() +
  theme(plot.title       = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle    = element_text(size = 9.5, face = "italic", color = "gray30"),
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold", size = 10))
ggsave("output/figures/eda_valle_scatter_log.png", plot = p_scl,
       width = 12, height = 4.5, dpi = 120)
cat("Guardado: output/figures/eda_valle_scatter_log.png\n")

# Scatter coloreado por ciudad — heterogeneidad espacial antes del modelo
df_scc <- data.frame(temp = df$temp, hr = df$hr, dia = df$dia_año,
                     pm25 = df$pm25, ciudad = df$ciudad) %>%
  tidyr::pivot_longer(cols = c(temp, hr, dia), names_to = "cov", values_to = "x") %>%
  mutate(cov = factor(cov,
                      levels = c("temp","hr","dia"),
                      labels = c("Temperatura (°C)","Humedad relativa (%)","Día del año")),
         ciudad = factor(ciudad, levels = c("cdmx","edomex","hidalgo"),
                         labels = c("CDMX","Edo. Méx.","Hidalgo")))
p_scc <- ggplot(df_scc, aes(x = x, y = pm25, color = ciudad)) +
  geom_point(alpha = 0.25, size = 0.8) +
  scale_color_manual(values = c("CDMX" = "#3498DB", "Edo. Méx." = "#E74C3C", "Hidalgo" = "#2ECC71")) +
  facet_wrap(~cov, scales = "free_x", ncol = 3) +
  labs(title    = expression(paste(PM[2.5], " vs covariables por entidad — heterogeneidad espacial")),
       subtitle = "Cada punto es una observación diaria; colores por entidad federativa",
       x = NULL,
       y = expression(paste(PM[2.5], " (", mu, "g/", m^3, ")")),
       color = "Entidad") +
  theme_minimal() +
  theme(plot.title       = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle    = element_text(size = 9.5, face = "italic", color = "gray30"),
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold", size = 10),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold", size = 9))
ggsave("output/figures/eda_valle_scatter_ciudad.png", plot = p_scc,
       width = 12, height = 4.5, dpi = 120)
cat("Guardado: output/figures/eda_valle_scatter_ciudad.png\n")

# Guardar resumen
write.csv(resumen, "output/figures/eda_valle_resumen.csv", row.names=FALSE)
cat("\nResumen guardado: output/figures/eda_valle_resumen.csv\n")
