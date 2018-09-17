##########################################################################################
###
### CALCULA LOS VALORES MAXIMOS Y MINIMOS DE LAS COORDENADAS DE LOS PUNTOS DE CONTROL
### @params: puntosControl (data.frame)
###
### Devolvemos xy (data.frame)
###
##########################################################################################
calculaLimites <- function(puntosControl) {
    ### Valores por defecto
    xy <- data.frame(x=c(-10,10), y=c(-10,10))

    ### CODIGO A REALIZAR:
    ### Si hay puntos de control,
    ###   Calcular minX, maxX, minY, maxY que corresponden a los valores minimos y maximos
    ###   de las coordenadas de los puntos de control
    ###             xy <- data.frame(x=c(minX,maxX), y=c(minY,maxY))
    
    minX <- min (puntosControl$x)
    maxX <- max (puntosControl$x)
    minY <- min (puntosControl$y)
    maxY <- max (puntosControl$y)
    
    xy <- data.frame(x=c(minX,maxX), y=c(minY,maxY))
    		
  	return(xy)
}