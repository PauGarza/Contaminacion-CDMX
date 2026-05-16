library(terra)
m <- vect('data/gadm_mexico/gadm41_MEX_2.shp')

cdmx <- m[m$NAME_1 == "Distrito Federal", ]
cat("Alcaldias CDMX:", nrow(cdmx), "\n")
print(cdmx$NAME_2)

cat("\n--- Estado de Mexico ---\n")
edo <- m[m$NAME_1 == "México", ]
cat("Municipios:", nrow(edo), "\n")
print(edo$NAME_2[grep("Tlalnepantla|Azcapotzalco|Gustavo|Miguel|Cuauhtemoc|Benito|Coyoacan|Iztapalapa", edo$NAME_2, ignore.case=TRUE)])
