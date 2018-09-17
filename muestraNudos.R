#######################################################################
###
### MUESTRA EL VECTOR DE NUDOS (por consola)
### @params: nudos (vector)
###
#######################################################################
muestraNudos <- function(nudos) {
  	if (is.null(nudos)) {
  	    print("AVISO: No hay nudos")
  	} else {  	
  	    m <- length(nudos)
  	    print(sprintf("%d nudos generados", m))
  	    s <- sprintf("nudo %d: %.5f", 1:m, nudos)
  	    writeLines(s)
  	}
}