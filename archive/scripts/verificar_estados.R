library(terra)
m = vect('C:/Users/pauli/Documents/RegresionAvanzada/CodigoRA/Mexico/shapes/')
# Estados de interes
estados_buscar = c('Distrito Federal','Ciudad de Mexico','Jalisco','Nuevo Leon')
for (e in estados_buscar) {
  idx = grep(e, m$ADM1, ignore.case=TRUE)
  if (length(idx) > 0) {
    cat(e, '-> STL-1:', unique(m$'STL-1'[idx]), 'Ejemplos:', head(m$ADM1[idx], 3), '\n')
  }
}
# Mostrar todos los estados unicos
u = unique(data.frame(stl=m$'STL-1', nombre=m$ADM1))
cat('\nEstados unicos:\n')
print(u[order(u$stl),])
