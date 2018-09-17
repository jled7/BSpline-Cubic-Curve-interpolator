#######################################################################
###
### DIBUJAMOS LOS DATOS INTRODUCIDOS
### @params: datos (datos introducidos, data.frame)
### 
#######################################################################
dibujaDatos <- function (datos) {
    ggplot(datos, aes(x=x, y=y))+	
        geom_point(color="red", shape=20, size=4) +
        coord_cartesian(xlim = c(-10,10), ylim = c(-10,10))
}