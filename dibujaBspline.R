#######################################################################
###
### DIBUJA BSPLINE
### @params: bspline (data.frame)
### @params: datos (data.frame)
###
#######################################################################
dibujaBspline <- function(bspline, datos) {
    ### Si no hay bspline, dibujamos una pantalla inicial vacia
  	if (is.null(bspline)) {
  		  pantallaEnBlanco()
  	} else {
            ### Calculamos 60 puntos de la curva bspline
    		puntos <- puntosEnCurva(bspline, 60)
    
    		### Dibujamos los puntos sobre la curva
    		pp <- ggplot(puntos, aes(x=x, y=y), color="black") +
          			geom_point(shape = 20, size = 2) +
          			geom_path()
    			
    		### Dibujamos los datos (no se dibujan si es una curva bspline derivada)
    		if (is.null(datos) == FALSE) {
    			pp <- pp + geom_point(data=datos, aes(x=x, y=y), color="red", shape=20, size = 4)
    			xyLimites <- NULL
    			xyLimites$x <- c(-10, 10)
    			xyLimites$y <- c(-10, 10)
    		} else {
    		    xyLimites <- bspline$xy
    		}
    
    		### Dibujamos los puntos de control
    		pp <- pp + 
    		    	geom_point(data = bspline$puntosControl, aes(x=x, y=y), color="blue", shape = 15, size = 4)
    
    		### Ajustamos los limites
    		minimoX <- min(bspline$puntosControl$x, xyLimites$x[1])
    		maximoX <- max(bspline$puntosControl$x, xyLimites$x[2])
    		minimoY <- min(bspline$puntosControl$y, xyLimites$y[1])
    		maximoY <- max(bspline$puntosControl$y, xyLimites$y[2])

    		pp <- pp + 
    				scale_x_continuous(limits = c(minimoX, maximoX)) + 
    			   	scale_y_continuous(limits = c(minimoY, maximoY))
    
    		pp <- pp + 
          			theme_bw() +
          			theme(legend.position="none", 
            				axis.title.x=element_blank(),
            		  		axis.title.y=element_blank(),
            				panel.grid.major = element_blank(), 
            				panel.grid.minor = element_blank(), 
            	    		panel.background = element_blank()
            	    )
    		
    		print(pp)
  	}
}
