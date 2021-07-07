#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' @param data vector de datos cuyo histograma se va a calcular
#' @param year
#' @return el histograma con la densidad normal superpuesta
#' @export return_level
#'





return_level = function(year, data){
  location <- data$Valores_optimos$Valores_optimos[1]
  scale <-  data$Valores_optimos$Valores_optimos[2]
  shape <- data$Valores_optimos$Valores_optimos[3]
  p <- 1/year
  retorno <- location - scale/shape*(1-(-log(1-p))^(-shape))
  print(paste0("Para el año ", year," se espera: ",retorno ))
}







