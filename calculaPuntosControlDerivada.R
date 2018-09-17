#######################################################################
###
### CALCULA LOS PUNTOS DE CONTROL DE LA DERIVADA DE LA BSPLINE
### @params: bspline (data.frame)
###
### Devolvemos los puntos de control de la derivada (data.frame)
###
#######################################################################
calculaPuntosControlDerivada <- function(bspline) {
  	puntosControl <- bspline$puntosControl
  	nudos <- bspline$nudos
  	p <- bspline$p
  
    ### CODIGO A REALIZAR:
    ### Calcular los puntos de control de la derivada
  	q <- data.frame(x=double(), y=double())
  	n <- dim(puntosControl)[1]
  	nNudos <- length(nudos);
  	print(p);
  	print(puntosControl);
  	print(nudos);
  	
  	
  	for(i in 1:(n-1)) {
  	  numerador <- (p * (puntosControl[i+1,] - puntosControl[i,]))
  	  denominador <- (nudos[i+p+1] - nudos[i+1])
  	  if(denominador < epsilom) {
  	    q[i,] <- 0
  	  } else {
  	    q[i,] <- numerador / denominador
  	  }
  	}
  	colnames(q) <- c("x", "y")
  	rownames(q) <- NULL  
  	print(q)
  	# q <- data.frame(q)
  
  	return(q)
}