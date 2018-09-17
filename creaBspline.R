#######################################################################
###
### CREA LA CURVA BSPLINE
### @params: datos (data.frame)
### @params: p (grado de la curva, cubica)
### @params: tipoNudos (1 - Uniforme, 2 - Chord Length, 3 - Centripeta)
###
### Devolvemos la curva bspline (data.frame)
###
#######################################################################
creaBspline <- function(datos, p, tipoNudos) {
    ### Inicializamos la variable de salida
    bspline <- NULL

    if (is.null(datos)) {
        print("AVISO: No hay datos. No se genera ninguna curva bspline")
    } else {
      	### Calculamos los nudos de la bspline
      	nudos <- calculaNudos(datos, p, tipoNudos)

        if (is.null(nudos )) {
            print("AVISO: No hay nudos. No se genera ninguna curva bspline")
        } else {
         	### Mostramos los nudos generados por pantalla
          	muestraNudos(nudos)
          
          	### Calculamos los puntos de control de la bspline que pasa por los datos
          	### introducidos por el usuario
          	puntosControl <- calculaPuntosControl(datos, nudos)

            ### Rango de las coordenadas de los puntos de control de la bspline 
            xy <- calculaLimites(puntosControl)

          	### Mostramos por pantalla los puntos de control de la bspline
          	muestraPuntosControl(puntosControl)

            bspline$p <- p
            bspline$tipo <- tipoNudos
            bspline$xy <- xy
            bspline$nudos <- nudos
            bspline$puntosControl <- puntosControl
        }
    }
  
  	return(bspline)
}