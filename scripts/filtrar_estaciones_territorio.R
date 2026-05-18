options(repos="http://cran.itam.mx/")
library(terra)
library(dplyr)

wdir <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."))
setwd(wdir)

# ============================================================
# Filtrar estaciones SINAICA por territorio (spatial join)
# ============================================================

# 1. Cargar estaciones
st <- read.csv("data/raw/todas_estaciones_sinaica.csv", stringsAsFactors=FALSE)
cat("Total estaciones SINAICA:", nrow(st), "\n")

# 2. Crear SpatVector de puntos (lon/lat)
pts <- vect(st, geom=c("lon", "lat"), crs="EPSG:4326")

# 3. Cargar GADM nivel 2 y filtrar territorios de interes
mex <- vect("data/gadm_mexico/gadm41_MEX_2.shp")

# Territorios de interes
alcaldias_cdmx <- c("Álvaro Obregón", "Azcapotzalco", "Benito Juárez", "Coyoacán",
                    "Cuajimalpa de Morelos", "Cuauhtémoc", "Gustavo A. Madero",
                    "Iztacalco", "Iztapalapa", "La Magdalena Contreras",
                    "Miguel Hidalgo", "Milpa Alta", "Tláhuac", "Tlalpan",
                    "Venustiano Carranza", "Xochimilco")

municipios_edomex <- c("Acolman", "Amecameca", "Apaxco", "Atenco", "Atizapán de Zaragoza",
                       "Atlautla", "Axapusco", "Ayapango", "Chalco", "Chiautla",
                       "Chicoloapan", "Chiconcuac", "Chimalhuacán", "Coacalco de Berriozábal",
                       "Cocotitlán", "Coyotepec", "Cuautitlán", "Cuautitlán Izcalli",
                       "Ecatepec de Morelos", "Ecatzingo", "Huehuetoca", "Hueypoxtla",
                       "Huixquilucan", "Isidro Fabela", "Ixtapaluca", "Jaltenco",
                       "Jilotzingo", "Juchitepec", "La Paz", "Melchor Ocampo",
                       "Naucalpan de Juárez", "Nextlalpan", "Nezahualcóyotl",
                       "Nicolás Romero", "Nopaltepec", "Otumba", "Ozumba",
                       "Papalotla", "San Martín de las Pirámides", "Tecámac",
                       "Temamatla", "Temascalapa", "Tenango del Aire", "Teoloyucan",
                       "Teotihuacán", "Tepetlaoxtoc", "Tepetlixpa", "Tepotzotlán",
                       "Tequixquiac", "Texcoco", "Tezoyuca", "Tlalmanalco",
                       "Tlalnepantla de Baz", "Tonanitla", "Tultepec", "Tultitlán",
                       "Valle de Chalco Solidaridad", "Villa del Carbón", "Zumpango")

municipios_hidalgo <- c("Tizayuca")

# Filtrar GADM
gadm_filtrado <- mex[mex$NAME_1 %in% c("Distrito Federal", "México", "Hidalgo") &
                     mex$NAME_2 %in% c(alcaldias_cdmx, municipios_edomex, municipios_hidalgo), ]

cat("Poligonos en territorio de interes:", nrow(gadm_filtrado), "\n")

# 4. Spatial join con extract (mas robusto que intersect)
# extract devuelve el ID del poligono que contiene cada punto
ext <- terra::extract(gadm_filtrado, pts)

# Filtrar puntos que cayeron en algun poligono
idx_en_poligono <- which(!is.na(ext$NAME_1))
cat("Estaciones dentro del territorio:", length(idx_en_poligono), "\n")

if (length(idx_en_poligono) == 0) {
  cat("ERROR: No se encontraron estaciones dentro del territorio\n")
  cat("Primeras 10 estaciones en area CDMX:\n")
  st_cdmx_area <- st[st$lon > -100 & st$lon < -98.5 & st$lat > 19 & st$lat < 20.5, ]
  print(head(st_cdmx_area[, c("station_id", "station_name", "lat", "lon")], 10))
} else {
  est_filtradas <- st[idx_en_poligono, ]
  est_filtradas$estado <- ext$NAME_1[idx_en_poligono]
  est_filtradas$municipio <- ext$NAME_2[idx_en_poligono]
  
  # Seleccionar columnas relevantes
  est_filtradas <- est_filtradas %>%
    select(station_id, station_name, network_name, estado, municipio, lat, lon) %>%
    arrange(estado, municipio, station_name)
  
  cat("\nEstaciones encontradas:\n")
  print(est_filtradas)
  
  write.csv(est_filtradas, "data/raw/estaciones_territorio_valle.csv", row.names=FALSE)
  cat("\nGuardado: data/raw/estaciones_territorio_valle.csv\n")
  
  cat("\nResumen por estado:\n")
  print(table(est_filtradas$estado))
  
  cat("\nResumen por municipio/alcaldia:\n")
  print(table(est_filtradas$municipio))
}
