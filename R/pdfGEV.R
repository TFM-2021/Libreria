#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' @param x vector de datos cuyo histograma se va a calcular
#' @param E fg dfg
#' @param media  dgdgdfg
#' @param desv dfg dg
#' @return el histograma con la densidad normal superpuesta
#' @export pdfGEV

require(base)
pdfGEV = function(x, E, media, desv,...){

  eq = function(x){
    t <- (1+E*((x-media)/desv))^(-1/E)
    (1/desv)*(t^(E+1))*exp(1)^(-t)
  }

  curve(eq, from=min(x), to=max(x), xlab="x", ylab="y",
        main = "Distribucion GEV")
    Maximaverosimilitud <- (-length(x)*log(desv)   -(1+1/E)*sum(log(1+E*((x-media)/desv)))     -sum((1+E*((x-media)/desv))^(-1/E)))*-1

  return(print(paste0("Negative Log-Likelihood Value: ", Maximaverosimilitud)))
}


