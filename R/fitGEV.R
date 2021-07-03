
#' @title
#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer tambien un estimador de nucleo de la densidad.
#' @param x vector de datos a ajustar
#' @return el histograma con la densidad normal superpuesta
#' @export fitGEV



fitGEV = function(x){
  eq = function( par){
    media <- par[1]
    desv <- par[2]
    E <- par[3]
    (-length(x)*log(desv)   -(1+1/E)*sum(log(1+E*((x-media)/desv))) -sum((1+E*((x-media)/desv))^(-1/E)))*-1


  }
  optim(par = c(0.1,0.1,0.1), fn = eq)
}
