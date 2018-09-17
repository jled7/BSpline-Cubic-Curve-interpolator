#######################################################################
###
### CREA LA DERIVADA DE LA BSPLINE
### @params: bspline (data.frame)
###
### Devolvemos la curva bspline derivada (data.frame)
###
#######################################################################
creaBsplineDerivada <- function(bspline) {
    derivada <- NULL

    if (is.null(bspline)) {
        print("AVISO: No se genera ninguna curva bspline derivada")
    }	else {
        ### CODIGO A REALIZAR:
        ### Calculamos los nudos de la B-spline derivada
        nudos <- NULL

        ### Mostramos los nudos generados por pantalla
        muestraNudos(nudos)

        ### Calculamos los puntos de control de la B-spline derivada
        puntosControl <- calculaPuntosControlDerivada(bspline)
      
        ### Rango de las coordenadas de los puntos de control de la derivada
        xy <- calculaLimites(puntosControl)

        ### Mostramos por pantalla los puntos de control de la B-spline derivada
        muestraPuntosControl(puntosControl)
          
      	derivada$p <- bspline$p - 1
      	derivada$tipo <- bspline$tipo
      	derivada$xy <- xy
      	derivada$nudos <- nudos
      	derivada$puntosControl <- puntosControl
    }
  
  	return(derivada)

}