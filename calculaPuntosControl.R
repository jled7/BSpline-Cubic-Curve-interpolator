#######################################################################
###
### CALCULA LOS PUNTOS DE CONTROL DE LA BSPLINE
### @params: datos (data.frame)
### @params: nudos (vector)
###
### Devolvemos los puntos de control de la bspline (data.frame)
###
#######################################################################
calculaPuntosControl <- function(datos, nudos) {
  	nDatos <- dim(datos)[1]
  	### Paso Forward
  	ABG <- dame_AlfaBetaGamma(nDatos, nudos)
  	print("Alfa")
  	print(ABG$alfa)
  	print("Beta")
  	print(ABG$beta)
  	print("Gamma")
  	print(ABG$gamma)
  	
  	LD  <- dame_LambdaDelta(datos, ABG$alfa, ABG$beta, ABG$gamma)
  	print("Lambda")
  	print(LD$lambda)
  	print("Delta")
  	print(LD$delta)
  
  	### Paso backward
  	X <- dame_X(LD$delta, LD$lambda)
  	print("X")
  	print(X)
    
    ### CODIGO A REALIZAR:
    ### Calcular los puntos de control de la bspline
    puntosControl <- data.frame(x=double(), y=double())
    
    for(i in 1:nDatos) {
      puntosControl[i+1, ] <- X[i,]
    }
    
    puntosControl[1,] <- puntosControl[2,]
    puntosControl[nDatos+2,] <- puntosControl[nDatos+1,]
  	# rownames(puntosControl) <- NULL
  	# colnames(puntosControl) <- c("x", "y")
  	# puntosControl <- data.frame(puntosControl)
  
  	return(puntosControl)
}