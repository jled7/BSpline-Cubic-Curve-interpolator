#######################################################################
###
### CALCULA LOS NUDOS NO UNIFORMES DE UNA BSPLINE
### @params: datos (datos introducidos)
### @params: p (grado de la bpsline)
### @params: esCentripeta (FALSE/TRUE)
###
### Devolvemos un vector de nudos
### 
#######################################################################
calculaNudosNoUniforme <- function(datos, p, esCentripeta) {

    ### CODIGO A REALIZAR:
    ### Calcular los nudos no uniformes (Chord-length o Centripeta) de una bspline
    nudos <- NULL
    print("DATOS")
    n <- dim(datos)[1]
    print(datos)
    print("n")
    print(n)
    if(esCentripeta) {
      temp <- data.frame(x=double(), y=double())
      normal <- array()
      L <- 0
      for(i in 2:(n)) {
        temp[i,] <- datos[i, ] - datos[i-1, ]
        normal[i-1] <- sqrt(sqrt(temp[i,1]^2+temp[i,2]^2))
        L <- L + normal[i-1]
      }
    } else {
      temp <- data.frame(x=double(), y=double())
      normal <- array()
      L <- 0
      for(i in 2:(n)) {
         temp[i,] <- datos[i, ] - datos[i-1, ]
         normal[i-1] <- sqrt(temp[i,1]^2+temp[i,2]^2)
         L <- L + normal[i-1]
      }
    }
    
    mid <- array()
    for(i in 1:(n-2)) {
      mid[i] <- 0
      for(j in 1:i) {
        mid[i] <- mid[i] + (normal[j] / L)
      }
    }
    
    pre <- rep(0,p+1)
    mid <- c(mid)
    post <- rep(1,p+1)
    nudos <- c(pre, mid, post)
    print(nudos)
  	return(nudos)
}