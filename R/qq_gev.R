#' @title
#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer tambien un estimador de nucleo de la densidad.
#' @param x vector de datos a ajustar
#' @return el histograma con la densidad normal superpuesta
#' @export qq_gev











qq_gev = function(model_fit, data){


  location_model <- as.numeric(model_fit$Valores_optimos[1,2])
  scale_model <- as.numeric(model_fit$Valores_optimos[2,2])
  shape_model <- as.numeric(model_fit$Valores_optimos[3,2])




  residuals <- (1 + (shape * (data - location))/scale)^(-1/shape)

  qq <- function(p,
                 shape = shape_model,
                 location = location_model,
                 scale = scale_model){

    location + (scale/shape) * (( - logb(p))^( - shape) - 1)
  }

  ggplot()+
    geom_jitter(aes(y=sort(data),
                    qq(ppoints(1:length(data)))))+
    geom_line(aes(sort(data),sort(data)))
}
