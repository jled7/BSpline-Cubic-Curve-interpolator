#######################################################################
###
### MUESTRA LOS PUNTOS DE CONTROL (por consola)
### @params: puntosControl (data.frame)
###
#######################################################################
muestraPuntosControl <- function(puntosControl) {
    if (is.null(puntosControl)) {
        print("AVISO: No hay puntos de control")
    } else {
      	n <- dim(puntosControl)[1]
      	print(sprintf("%d puntos de control generados", n))
      	s <- sprintf("Punto de control %d: (%.2f, %.2f)", 1:n, puntosControl$x, puntosControl$y)
      	writeLines(s)
    }
}