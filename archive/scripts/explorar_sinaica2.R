options(repos="http://cran.itam.mx/")
library(rsinaica)

# Buscar nombres que contengan Valle o Mexico
redes <- unique(stations_sinaica[, c('network_id','network_name')])
print("Redes con 'Valle':")
print(redes[grep("Valle", redes$network_name, ignore.case=TRUE),])

print("")
print("Redes con 'Mexico':")
print(redes[grep("Mexico", redes$network_name, ignore.case=TRUE),])

print("")
print("Redes con 'México':")
print(redes[grep("México", redes$network_name, ignore.case=TRUE),])

print("")
print("Todas las estaciones con network_id 119 (Valle de Mexico segun primer output):")
vm <- stations_sinaica[stations_sinaica$network_id == 119, c('station_id','station_name','station_code','lat','lon')]
print(vm)

# Guardar
write.csv(vm, "C:/Users/pauli/Documents/RegresionAvanzada/Contaminacion-CDMX/data/raw/estaciones_cdmx.csv", row.names=FALSE)
