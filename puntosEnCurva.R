#######################################################################
###
### GENERA PUNTOS SOBRE LA BSPLINE
### (por defecto, 20 PUNTOS)
### @params: bspline (data.frame)
### @params: nPuntos (integer o NULL)
###
### Devolvemos puntos (data.frame) sobre la bspline: coordenadas (x, y)
###
#######################################################################
puntosEnCurva <- function(bspline, nPuntos=NULL) {
  	puntosControl <- bspline$puntosControl
  	nudos <- bspline$nudos
  	p <- bspline$p
    n <- dim(puntosControl)[1] 
  	### CODIGO A REALIZAR:
  	### Generamos las coordenadas (x, y) y las almacenamos en la variable puntos
  	puntos <- data.frame(x=double(), y=double())
  	temp <- data.frame(x=double(), y=double())
  	
  	u <- 0
  	suma <- 1 / nPuntos
  	
  	while (u < 1){
  	  tempX <- 0
  	  tempY <- 0
  	  for (i in 1:n) {
  	    tempX <- tempX + (N(i, p, u, nudos) * puntosControl[i,1])
  	    tempY <- tempY + (N(i, p, u, nudos) * puntosControl[i,2])
  	  }
  	  
  	  temp[1,1] <- tempX
  	  temp[1,2] <- tempY
  	  
  	  puntos<- rbind(puntos, temp)
  	  colnames(puntos) <- c("x","y")
  	  
  	  u <- u + suma
  	}
  	
  	
	# rownames(puntos) <- NULL
	# colnames(puntos) <- c("x", "y")
	# puntos <- data.frame(puntos)

	return(puntos)
}