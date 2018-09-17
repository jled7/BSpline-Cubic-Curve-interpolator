#######################################################################
###
### MUESTRA LOS DATOS INTRODUCIDOS (por consola)
### @params: datos (data.frame)
###
#######################################################################
muestraDatos <- function(datos) {
    if (is.null(datos)) {
        print("AVISO: No hay datos")
    } else {
      	n <- dim(datos)[1]
      	print(sprintf("%d datos introducidos", n))
      	s <- sprintf("dato %d: (%.2f, %.2f)", 1:n, datos$x, datos$y)
      	writeLines(s)
    }
}