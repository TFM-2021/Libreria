
#' @title
#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer tambien un estimador de nucleo de la densidad.
#' @param x vector de datos a ajustar
#' @return el histograma con la densidad normal superpuesta
#' @export fitGPD
#'
#'



fitGPD = function(data,threshold){

  n <- length(data)

  eq = function(par){

    alpha <- par[1]
    xm <- min(data)
    -(n*log(alpha)+n*log(xm) - (alpha +1)*sum(log(data))) # https://en.wikipedia.org/wiki/Pareto_distribution
    #-n/(sum(log(data/xm)))
  }
  optim(c(0.1),fn = eq)
}
