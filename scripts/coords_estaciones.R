options(repos="http://cran.itam.mx/")
library(rsinaica)
ids <- c(259, 256, 266, 300, 102, 103, 105, 106, 141, 142, 146, 148)
print(stations_sinaica[stations_sinaica$station_id %in% ids, c('station_id','station_name','lat','lon')])
