library(rsinaica)
data(stations_sinaica)

ids <- c(244,245,256,258,259,260,268,269,300,302,432)
sub <- stations_sinaica[stations_sinaica$station_id %in% ids, 
                        c("station_id","station_name","lat","lon","state_code")]
print(sub)
write.csv(sub, "data/coords_estaciones_cdmx.csv", row.names=FALSE)
