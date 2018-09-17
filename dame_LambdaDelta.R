#######################################################################
###
### CALCULA LAMBDA-DELTA (PASO FORDWARD 2)
### @params: datos (data.frame)
### @params: alfa, beta, gamma
###
### Devolvemos lambda (vector) y beta (data.frame)
###
#######################################################################
dame_LambdaDelta <- function (datos, alfa, beta, gamma) {
  	n <- length(alfa)
  	lambda <- array(0, dim = c(n,1))
  	delta  <- data.frame(x=double(), y=double())
  	
    ### CODIGO A REALIZAR:
    ### Calcular lambda y delta 
    print("Datos")
    print(datos)
  	
  	## Calcular lambda
  	if(beta[1] < epsilom) {
  	  lambda[1] <- 0
  	} else {
  	  lambda[1] <- gamma[1]/beta[1]
  	}
    
  	for(i in 2:(n-1)) {
  	  numerador <- gamma[i]
  	  denominador <- beta[i]-(alfa[i]*lambda[i-1])
  	  if(denominador < epsilom) {
  	    lambda[i] <- 0
  	  } else {
  	    lambda[i] <- numerador/denominador
  	  }
  	}
  	
  	## Calcular delta
  	if(beta[1] < epsilom) {
  	  delta[1,] <- 0
  	} else {
  	  delta[1,] <- datos[1,]/beta[1]
  	}
  	
  	for(i in 2:(n)) {
  	  numerador <- datos[i,]-(alfa[i]*delta[i-1,])
  	  denominador <- beta[i]-(alfa[i]*lambda[i-1])
  	  if(denominador < epsilom) {
  	    delta[i,] <- 0
  	  } else {
  	    delta[i,] <- numerador/denominador
  	  }
  	}
  	
  	res        <- NULL
  	res$lambda <- lambda
  	res$delta  <- delta
  
  	return(res)
}