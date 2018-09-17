#######################################################################
###
### DIBUJA FUNCIONES BASE
### @params: bspline (data.frame)
###
#######################################################################
dibujaFuncionesBase <- function(bspline) {
    ### Si no hay bspline, dibujamos una pantalla inicial vacia
  	if (is.null(bspline)) {
  		pantallaEnBlanco()
  	} else {
        ### Calculamos 120 puntos a dibujar a partir de las funciones base de la bspline
    	puntos <- puntosEnFuncionesBase(bspline, 120)
    
        ### Creamos una paleta de colores
    	nColores <- max(as.numeric(puntos$color))
    	misColores <- colorRampPalette(c("red","yellow","springgreen","royalblue"))
    	paletaColores <- misColores(nColores)
    
    	### Dibujamos cada funcion base (en colores)
    	pp <- ggplot(puntos, aes(x=x, y=y, color=color)) +
          		geom_point(shape = 20, size = 2) +
    		  	geom_line() +
          		#geom_path() +
          		scale_colour_manual(values=paletaColores) +
          		theme_bw() +
          		theme(legend.position="none", axis.text.x=element_blank(),
              			axis.text.y=element_blank(),
              			axis.title.x=element_blank(),
              		  	axis.title.y=element_blank(),
              			panel.grid.major = element_blank(), 
              			panel.grid.minor = element_blank(), 
          	    		panel.background = element_blank()
          	    )
          			
        print(pp)
  	}
}