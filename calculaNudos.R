#######################################################################
###
### CALCULA LOS NUDOS DE UNA BSPLINE
### @params: datos (data.frame)
### @params: p (grado de la bspline)
### @params: tipoNudos (1 - Uniforme, 2 - Chord Length, 3 - Centripeta)
###
### Devolvemos el vector de nudos
###
#######################################################################
calculaNudos <- function(datos, p, tipoNudos) {
  	### Numero de datos introducidos
  	nDatos <- dim(datos)[1]

  	### Calculamos los nudos segun el tipo de B-spline: 
    ### uniforme y no-uniforme(chord_length o centripeta)
  	if (tipoNudos == 1) {
    	### Uniforme
  		nudos <- calculaNudosUniforme(nDatos, p)
  	} else {
    	if (tipoNudos == 2) {
       		### Chord Length
    		nudos <- calculaNudosNoUniforme(datos, p, FALSE)
    	} else {
          	### Centripeta
    		nudos <- calculaNudosNoUniforme(datos, p, TRUE)
    	}
  	}
  	
  	return(nudos)
}