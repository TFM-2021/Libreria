#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' @param data vector de datos cuyo histograma se va a calcular
#' @return el histograma con la densidad normal superpuesta
#' @export mean_excess_GEV


mean_excess_GEV = function(data){

  rango_datos <- seq(min(data),max(data),1)

  vector <- as.vector(NULL)
  vector_sd <- as.vector(NULL)

  for (u in rango_datos) {
    filtro_datos <- data[data>u]

    media_valores <- mean(filtro_datos-u)
    sd_valores <- sqrt(var(filtro_datos,)/length(filtro_datos))

    vector <- append(vector, media_valores)
    vector_sd <- append(vector_sd, sd_valores)

  }
  ggplot()+
    geom_line(aes(rango_datos, vector))+
    geom_line(aes(rango_datos, vector-2*vector_sd))+
    geom_line(aes(rango_datos, vector+2*vector_sd)) +

    labs(title = "Mean excess",
         x = "Threshold",
         y = "Mean excess",
         color = NULL)

}
