#' @description Dibuja un gráfico de cuantiles reales y modelados
#' @param model_fit modelo ajustado
#' @param data datos ajustados
#' @return Gráfico de qq-plot
#' @export qq_gpd





qq_gpd =  function(model_fit, data, threshold){

  data_u <- data[data>threshold]
  sort_data <- sort(data_u)
  scale_model <- as.numeric(model_fit$Valores_optimos[1,2])
  shape_model <- as.numeric(model_fit$Valores_optimos[2,2])


  qq = function(p, threshold_ =threshold, scale = scale_model, shape = shape_model)
  {
    threshold_ + (scale * (p^( - shape)- 1))/shape
    #((1 - p)^( - scale) - 1)/scale
  }

  ggplot()+
    geom_jitter(aes(y = sort_data,
                    x = sort(qq(ppoints(1:length(data_u))))))+
    geom_line(aes(sort_data,sort_data))+

    labs(title =  "QQ-plot GPD: "
         ,
         subtitle = paste0("shape: ", round(shape_model,2),
                           " scale: ", round(scale_model,2)),
         x = "Cuantiles modelados",
         y = "Datos reales",
         color = NULL) +
    theme_minimal()
}
