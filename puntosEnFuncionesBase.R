#######################################################################
###
### GENERA PUNTOS A PARTIR DE LAS FUNCIONES BASE DE LA  BSPLINE 
### (por defecto, 20 PUNTOS)
### @params: bspline (data.frame)
### @params: nPuntos (integer o NULL)
###
### Devolvemos puntos (data.frame) a partir de las funciones base de
### la bspline
### Cada punto de la funcion base N(i, p, u, nudos): 
### coordenadas (x, y) y color (i == indice de la funcion base) 
###
#######################################################################
puntosEnFuncionesBase <- function(bspline, nPuntos=NULL) {
	puntosControl <- bspline$puntosControl
	nudos <- bspline$nudos
	p <- bspline$p
	n <- dim(puntosControl)[1] 

	### CODIGO A REALIZAR:
  ### Generamos los puntos (x, y, color) y los almacenamos en la variable puntos
	puntos <- data.frame(x=double(), y=double(), color=integer())
	temp <- NULL
	
	k <- 1
	suma <- 1 / nPuntos
	
	for (i in 1:n) {
	  u <- 0
  	while (u < 1){
  	    tempX <- u
  	    tempY <- N(i, p, u, nudos)
  	    temp <- c(tempX, tempY, i)
  	    puntos <- rbind(puntos, temp)
  	    u <- u + suma
  	}
	}
    colnames(puntos) <- c("x","y","color")

    puntos$color <- factor (puntos$color)
    print(puntos)
	# rownames(puntos) <- NULL
	# colnames(puntos) <- c("x", "y", "color")
	# puntos <- data.frame(puntos)
	# puntos$color <- factor(puntos$color)
		
	return(puntos)
}