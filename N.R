#######################################################################
###
### FUNCION COX-DE-BOOR
### @params: i (indice)
### @params: p (grado de la curva)
### @params: u (parametro)
### @params: nudos (vector)
###
### Devolvemos el valor de la funcion base N(i,p,u)
###
#######################################################################
N <- function (i, p, u, nudos) {
	### CODIGO A REALIZAR
    ### Calcular el valor de la funcion base N(i,p,u)
    nip <- 0
    #print("ANALISIS")
    #print("p")
    #print(p)
    #print("i")
    #print(i)
    #print("u")
    #print(u)
    #print("nudos")
    #print(nudos)
    if(p != 0) {
      numerador = u - nudos[i]
      denominador = nudos[i+p] - nudos[i]
      print("NEW");
      print(numerador);
      print(denominador);
      if(denominador > epsilom) {
        nip <- (numerador/denominador) * N(i, p-1, u, nudos)
      }
      numerador = nudos[i+p+1] - u
      denominador = nudos[i+p+1] - nudos[i+1]
      if(denominador > epsilom) {
        nip <- nip + ((numerador/denominador) * N(i+1, p-1, u, nudos))
      }
    } else {
      if(u >= nudos[i] && u < nudos[i+1]) {
        nip <- 1
      }
    }
  	return(nip)
}