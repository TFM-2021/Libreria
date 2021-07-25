#' @description Calcula el retorno de los datos para un determinado año
#' @param model_fit modelo ajustado
#' @param year año que quieres calcular
#' @return el return level para el año elegido
#' @export return_level
#'


return_level = function(year, model_fit){
  location <- model_fit$Valores_optimos$Valores_optimos[1]
  scale <-  model_fit$Valores_optimos$Valores_optimos[2]
  shape <- model_fit$Valores_optimos$Valores_optimos[3]
  p <- 1/year
  retorno <- location - scale/shape*(1-(-log(1-p))^(-shape))
  print(paste0("Para el año ", year," se espera: ",retorno ))
}







