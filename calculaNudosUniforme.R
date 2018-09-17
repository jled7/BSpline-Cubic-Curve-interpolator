#######################################################################
###
### CALCULA LOS NUDOS UNIFORMES DE UNA BSPLINE
### @params: nDatos (numero de datos)
### @params: p (grado de la bspline)
###
### Devolvemos un vector de nudos
###
#######################################################################
calculaNudosUniforme <- function(nDatos, p) {

    ### CODIGO A REALIZAR:
    ### Calcular los nudos uniformes de una bspline
    # nDatos <- 4
    # p <- 3
    # 0 0 0 [0 0.33 0.66 1] 1 1 1
    pre <- rep(0,p)
    mid <- seq(0,1,by=1/(nDatos-1))
    post <- rep(1,p)
    nudos <- c(pre, mid, post)
 	
  	return(nudos)
}

