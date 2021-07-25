#' @description Dibuja un gráfico con los datos reales y los estimados por la distribución para los parámetros óptimos
#' @param fit modelo ajustado
#' @param data datos ajustados
#' @param limite_inferior  limite inferior del grafico
#' @param limite_superior limite superior del grafico
#' @return comparación entre los datos reales y la distribución teórica
#' @export pdfGEV

require(base)
pdfGEV = function(fit, data, limite_inferior, limite_superior){

  location <- fit$Valores_optimos$Valores_optimos[1]
  scale <- fit$Valores_optimos$Valores_optimos[2]
  shape <- fit$Valores_optimos$Valores_optimos[3]

  df <-tibble("Empirico"= data,
              "Teorica" = GEVcdn::rgev(length(data), loc = location, scale = scale, shape = shape))

  print(paste0("Negative Log-Likelihood Value: ", fit$Negative_Log_Likelihood))

  ggplot(df)+
    geom_density(aes(Empirico))+
    geom_density(aes(Teorica),color="red")+
    xlim(c(limite_inferior,limite_superior))+
    labs(title = "PDF datos vs modelo",
         subtitle = paste0("location: ", round(location,2),
                           " scale: ", round(scale,2),
                           " shape: ", round(shape,2)),
         x = "Datos",
         y = "Densidad",
         color = NULL) +
    theme_minimal()


}



