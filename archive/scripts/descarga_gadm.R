options(repos="http://cran.itam.mx/")

# Descarga shapefile municipal de Mexico desde GADM
gadm_url <- "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_MEX_shp.zip"
zip_path <- "data/gadm41_MEX_shp.zip"
shp_dir  <- "data/gadm_mexico"

dir.create("data", showWarnings=FALSE)
dir.create(shp_dir, showWarnings=FALSE)

if (!file.exists(zip_path)) {
  cat("Descargando shapefile de GADM (Mexico, nivel 2, municipios)...\n")
  download.file(gadm_url, zip_path, mode="wb", timeout=300)
  cat("Descarga completada.\n")
}

if (!file.exists(file.path(shp_dir, "gadm41_MEX_2.shp"))) {
  cat("Descomprimiendo...\n")
  unzip(zip_path, exdir=shp_dir)
  cat("Descompresion completada.\n")
}

library(terra)
mex_mun <- vect(file.path(shp_dir, "gadm41_MEX_2.shp"))
cat("Shapefile cargado.\n")
cat("Numero de municipios:", nrow(mex_mun), "\n")
cat("Campos:", names(mex_mun), "\n")

# Ver CDMX y Estado de Mexico
cdmx_mun <- mex_mun[mex_mun$NAME_1 == "Ciudad de México", ]
cat("Alcaldias CDMX:", nrow(cdmx_mun), "\n")
print(cdmx_mun$NAME_2)

edomex_mun <- mex_mun[mex_mun$NAME_1 == "México", ]
cat("Municipios Estado de Mexico:", nrow(edomex_mun), "\n")

# Ver si Tlalnepantla esta
print(edomex_mun$NAME_2[grep("Tlalnepantla", edomex_mun$NAME_2, ignore.case=TRUE)])

cat("\nListo para usar gadm41_MEX_2.shp\n")
