#######################################################################
###
### CALCULA ALFA, BETA, GAMMA (PASO FORDWARD 1)
### @params: n (integer)
### @params: nudos (vector)
###
### Devolvemos los vectores alfa, beta, gamma
###
#######################################################################
dame_AlfaBetaGamma <- function (n, nudos) {
    
  	alfa  <- array(0, dim = n)
  	beta  <- array(0, dim = n)
  	gamma <- array(0, dim = n)
  
  	### alfa[1] == alfa[n] == 0
  	### gamma[1] == gamma[n] == 0
    ### beta[1] == beta[n] == 1
  	
    ### CODIGO A REALIZAR:
    ### Calcular los vectores alfa, beta, gamma
    alfa[1] = alfa[n] = 0
    gamma[1] = gamma[n] = 0
    beta[1] = beta[n] = 1
    
    for(i in 2:(n-1)) {
        numerador <- (nudos[i+4] - nudos[i+3])*(nudos[i+4] - nudos[i+3])
        denominador <- (nudos[i+4] - nudos[i+1])*(nudos[i+4] - nudos[i+2])
        if(denominador < epsilom) {
          alfa[i] <- 0
        } else {
          alfa[i] <- numerador/denominador
        }
        numerador1 <- (nudos[i+3] - nudos[i+1])*(nudos[i+4] - nudos[i+3])
        denominador1 <- (nudos[i+4] - nudos[i+1])*(nudos[i+4] - nudos[i+2])
        numerador2 <- (nudos[i+5] - nudos[i+3])*(nudos[i+3] - nudos[i+2])
        denominador2 <- (nudos[i+5] - nudos[i+2])*(nudos[i+4] - nudos[i+2])
        if(denominador1 < epsilom) {
          divison1 <- 0
        } else {
          division1 <- numerador1/denominador1  
        }
        if(denominador2 < epsilom) {
          divison2 <- 0
        } else {
          division2 <- numerador2/denominador2  
        }
        beta[i] = division1 + division2
        numerador <- (nudos[i+3] - nudos[i+2])*(nudos[i+3] - nudos[i+2])
        denominador <- (nudos[i+4] - nudos[i+2])*(nudos[i+5] - nudos[i+2])
        if(denominador < epsilom) {
          gamma[i] <- 0
        } else {
          gamma[i] <- numerador/denominador
        }
        
    }
    
  	res       <- NULL
  	res$alfa  <- alfa
  	res$beta  <- beta
  	res$gamma <- gamma
  	
  	return(res)

}