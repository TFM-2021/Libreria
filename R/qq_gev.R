#' @description Dibuja un gráfico de cuantiles reales y modelados
#' @param model_fit modelo ajustado
#' @param data datos ajustados
#' @return Gráfico de qq-plot
#' @export qq_gev





qq_gev <- function(model_fit, data){


  sort_data <- sort(data)
  location_model <- as.numeric(model_fit$Valores_optimos[1,2])
  scale_model <- as.numeric(model_fit$Valores_optimos[2,2])
  shape_model <- as.numeric(model_fit$Valores_optimos[3,2])



  qq <- function(p,
                 location = location_model,
                 shape = shape_model,
                 scale = scale_model){

    location + (scale/shape) * (( - logb(p))^( - shape) - 1)
  }

  ggplot()+
    geom_jitter(aes(y = sort_data,
                    qq(ppoints(1:length(data)))))+
    geom_line(aes(sort_data,sort_data))+

    labs(title =  "QQ-plot GEV: "
                         ,
         subtitle = paste0("location: ", round(location_model,2),
         " scale: ", round(scale_model,2),
         " shape: ", round(shape_model,2)),
         x = "Cuantiles modelados",
         y = "Datos reales",
         color = NULL) +
    theme_minimal()
}





















