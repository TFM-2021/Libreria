#' @title FITGEV
#' @description Ajusta por MLE los datos a una distribución GEV
#' @param x vector de datos a ajustar
#' @param parametros_iniciales Valores con los que empezar a optimizar
#' @param metodo_optimizacion Sí "SANN" = optimizacion por recocido simulado
#' @return parámetros GEV óptimos y gráficos comparándolos con la verosimilitud
#' @export
#'


fitGumbel =  function(x, metodo_optimizacion=NULL){

  eq = function(par){
    media <- par[1]
    desv <- par[2]

    -(-length(x)*log(desv)-sum((x-media)/desv)-sum(exp(-(x-media)/desv)))

  }
  optimizacion <- optim(par=c(1,1), fn = eq,hessian = T)
  resultados_fit <- tibble("Parametro"= c("location", "scale"),
                           "Valores_optimos"= optimizacion$par)

  verosimilitud <- c(optimizacion$value)

  valor_location <- as.double(resultados_fit[1,2])
  valor_scale <-  as.double(resultados_fit[2,2])

  # PLOT LOCATION -------------------------------------------------------

  # secuencia son los numeros de la variable location para calcular y graficar
  secuencia <-seq(valor_location-0.1,
                  valor_location+0.1,
                  0.001)

  #  aplicaicon de la funcion de versimilutd con las otras dos variables fijas

  verosimilitud_funcion_media <- sapply(secuencia, function(media){
    desv <- as.double(resultados_fit[2,2])

    -(-length(x)*log(desv)-sum((x-media)/desv)-sum(exp(-(x-media)/desv)))
  })

  # agrupamos para plotear
  df <- data.frame(secuencia, verosimilitud_funcion_media)

  plot_location <<- ggplot(df,aes(secuencia, verosimilitud_funcion_media))+
    geom_line() +
    xlim(as.double(valor_location-0.1),
         as.double(valor_location+0.1)) +

    labs(title = "Relación Verosimilitud / Location",
         x = "Location",
         y = "Verosimilitud",
         color = NULL) +

    geom_hline(yintercept=min(verosimilitud_funcion_media), color="red") +

    geom_point(data = df[which.min(df$verosimilitud_funcion_media), ],
               color="red",
               size=3) +

    geom_text_repel(data = df[which.min(df$verosimilitud_funcion_media), ],
                    aes(secuencia, verosimilitud_funcion_media,
                        label = round(secuencia,2))) +
    theme_minimal()


  # PLOT SCALE -------------------------------------------------------

  # secuencia son los numeros de la variable SCALE para calcular y graficar
  secuencia <-seq(valor_scale-0.1,
                  valor_scale+0.1,
                  0.001)

  #  aplicaicon de la funcion de versimilutd con las otras dos variables fijas

  verosimilitud_funcion_media <- sapply(secuencia, function(desv){
    media <- valor_location

    -(-length(x)*log(desv)-sum((x-media)/desv)-sum(exp(-(x-media)/desv)))
  })

  # agrupamos para plotear
  df <- data.frame(secuencia, verosimilitud_funcion_media)

  plot_scale <<- ggplot(df,aes(secuencia, verosimilitud_funcion_media))+
    geom_line() +
    xlim(as.double(valor_scale-0.1),
         as.double(valor_scale+0.1)) +

    labs(title = "Relación Verosimilitud / Scale",
         x = "Scale",
         y = "Verosimilitud",
         color = NULL) +

    geom_hline(yintercept=min(verosimilitud_funcion_media), color="red") +

    geom_point(data = df[which.min(df$verosimilitud_funcion_media), ],
               color="red",
               size=3) +

    geom_text_repel(data = df[which.min(df$verosimilitud_funcion_media), ],
                    aes(secuencia, verosimilitud_funcion_media,
                        label = round(secuencia,2))) +
    theme_minimal()






  list("Valores_optimos"=resultados_fit,
       "Negative_Log_Likelihood"=verosimilitud,
       "AIC" =2*2+2*verosimilitud,
       "BIC" = 2*verosimilitud+2*log(length(x)),
       "Desviaciones_tipicas_estimadas" = tibble("location"=sqrt(solve(optimizacion$hessian)[1,1]),
                                                 "scale"=sqrt(solve(optimizacion$hessian)[2,2])),
       "Matriz_covarianzas" = tibble("location"=solve(optimizacion$hessian)[1,],
                                     "scale"=solve(optimizacion$hessian)[2,]))

}



