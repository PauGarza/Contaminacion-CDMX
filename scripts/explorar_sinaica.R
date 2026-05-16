options(repos="http://cran.itam.mx/")
library(rsinaica)

# Buscar estaciones en las redes correctas
print("Valle de Mexico:")
vm <- stations_sinaica[stations_sinaica$network_name == "Valle de Mexico", c('station_id','station_name','station_code','lat','lon')]
print(vm)

print("")
print("Guadalajara:")
gdl <- stations_sinaica[stations_sinaica$network_name == "Guadalajara", c('station_id','station_name','station_code','lat','lon')]
print(gdl)

print("")
print("Monterrey:")
mty <- stations_sinaica[stations_sinaica$network_name == "Monterrey", c('station_id','station_name','station_code','lat','lon')]
print(mty)

# Guardar tablas para referencia
write.csv(vm, "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/raw/estaciones_cdmx.csv", row.names=FALSE)
write.csv(gdl, "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/raw/estaciones_gdl.csv", row.names=FALSE)
write.csv(mty, "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/raw/estaciones_mty.csv", row.names=FALSE)
