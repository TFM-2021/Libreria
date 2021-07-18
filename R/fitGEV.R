#' @title
#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer tambien un estimador de nucleo de la densidad.
#' @param x vector de datos a ajustar
#' @return el histograma con la densidad normal superpuesta
#' @export fitGEV


fitGEV =   function(x, parametros_iniciales){

  eq = function(par){
    media <- par[1]
    desv <- par[2]
    E <- par[3]
    (-length(x)*log(desv)   -(1+1/E)*sum(log(1+E*((x-media)/desv))) -sum((1+E*((x-media)/desv))^(-1/E)))*-1


  }

  optimizacion <- optim(parametros_iniciales,fn = eq,hessian = TRUE)

  resultados_fit <- tibble("Parametro"= c("location", "scale", "shape"),
                           "Valores_optimos"= optimizacion$par)

  verosimilitud <- c(optimizacion$value)


  valor_location <- as.double(resultados_fit[1,2])
  valor_scale <-  as.double(resultados_fit[2,2])
  valor_shape <-  as.double(resultados_fit[3,2])

  # PLOT LOCATION -------------------------------------------------------

  # secuencia son los numeros de la variable location para calcular y graficar
  secuencia <-seq(valor_location-0.1,
                  valor_location+0.1,
                  0.001)

  #  aplicaicon de la funcion de versimilutd con las otras dos variables fijas

  verosimilitud_funcion_media <- sapply(secuencia, function(media){
    E <- as.double(resultados_fit[3,2])
    desv <- as.double(resultados_fit[2,2])

    (-length(x)*log(desv)-(1+1/E)*sum(log(1+E*((x-media)/desv)))-sum((1+E*((x-media)/desv))^(-1/E)))*-1
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
    E <- valor_shape
    media <- valor_location

    (-length(x)*log(desv)-(1+1/E)*sum(log(1+E*((x-media)/desv)))-sum((1+E*((x-media)/desv))^(-1/E)))*-1
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

  # PLOT SHAPE -------------------------------------------------------

  # secuencia son los numeros de la variable SHAPE para calcular y graficar
  secuencia <-seq(valor_shape-0.1,
                  valor_shape+0.1,
                  0.001)

  #  aplicaicon de la funcion de versimilutd con las otras dos variables fijas

  verosimilitud_funcion_media <- sapply(secuencia, function(E){
    media <- valor_location
    desv <- valor_scale

    (-length(x)*log(desv)-(1+1/E)*sum(log(1+E*((x-media)/desv)))-sum((1+E*((x-media)/desv))^(-1/E)))*-1
  })

  # agrupamos para plotear
  df <- data.frame(secuencia, verosimilitud_funcion_media)

  plot_shape <<- ggplot(df,aes(secuencia, verosimilitud_funcion_media))+
    geom_line() +
    xlim(as.double(valor_shape-0.1),
         as.double(valor_shape+0.1)) +

    labs(title = "Relación Verosimilitud / Shape",
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
  # RESUMEN------------------------------------------------------------------
  list("Valores_optimos"=resultados_fit,
       "Negative_Log_Likelihood"=verosimilitud,
       "Desviaciones_tipicas_estimadas" = tibble("location"=sqrt(solve(optimizacion$hessian)[1,1]),
                                                 "scale"=sqrt(solve(optimizacion$hessian)[2,2]),
                                                 "shape"=sqrt(solve(optimizacion$hessian)[3,3])),
       "Matriz_covarianzas" = tibble("location"=solve(optimizacion$hessian)[1,],
                                     "scale"=solve(optimizacion$hessian)[2,],
                                     "shape"=solve(optimizacion$hessian)[3,]))

}

