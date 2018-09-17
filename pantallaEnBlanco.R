#######################################################################
###
### DIBUJAMOS PLOT VACIO
### 
#######################################################################
pantallaEnBlanco <- function() {
    df <- data.frame(x=c(-10, -10, 10, 10), y=c(-10, -10, 10, 10))
    ggplot(df, aes(x=x, y=y))+	
        geom_point(color="white") +
        coord_cartesian(xlim = c(-10,10), ylim = c(-10,10)) +
        theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank())
}