#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' @param x vector de datos cuyo histograma se va a calcular
#' @param E fg dfg
#' @param media  dgdgdfg
#' @param desv dfg dg
#' @return el histograma con la densidad normal superpuesta
#' @export pdfGEV

require(base)
pdfGEV = function(fit, data, limite_inferior, limite_superior){
  location <- fit$Valores_optimos$Valores_optimos[1]
  scale <- fit$Valores_optimos$Valores_optimos[2]
  shape <- fit$Valores_optimos$Valores_optimos[3]

  df <-tibble("Empirico"= data,
              "Teorica" = rgev(length(data), loc = location, scale = scale, shape = shape))

  print(paste0("Negative Log-Likelihood Value: ", fit$Negative_Log_Likelihood))

  ggplot(df)+
    geom_density(aes(Empirico))+
    geom_density(aes(Teorica),color="red")+
    xlim(c(limite_inferior,limite_superior))+
    labs(title = "PDF datos vs modelo",
         x = "Datos",
         y = "Densidad",
         color = NULL)


}



