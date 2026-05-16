options(repos="http://cran.itam.mx/")
library(terra)

mexico.map <- vect("C:/Users/pauli/Documents/RegresionAvanzada/CodigoRA/Mexico/shapes/")
cat("Campos del shapefile:\n")
print(names(mexico.map))
cat("\n")

# Muestra de registros para CDMX, Jalisco, NL
for (s in c(9,14,19)) {
  idx <- mexico.map$`STL-1` == s
  estado <- mexico.map[idx, ]
  cat("\n=== Estado", s, "===\n")
  cat("Num poligonos:", nrow(estado), "\n")
  
  # Mostrar los primeros 10 registros
  for (campo in names(estado)) {
    cat(campo, ": ")
    vals <- head(estado[[campo]], 10)
    print(as.character(vals))
  }
}
