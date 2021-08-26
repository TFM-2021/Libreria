
#' @title
#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer tambien un estimador de nucleo de la densidad.
#' @param x vector de datos a ajustar
#' @return el histograma con la densidad normal superpuesta
#' @export fitGPD
#'
#'



fitGPD = function(data,threshold){

  data_u <- data[data > threshold]
  exceso <- data_u - threshold

  eq = function(par){
    shape <- par[2]

    scale <- par[1]
    #-(n*log(alpha)+n*log(xm) - (alpha +1)*sum(log(data))) # https://en.wikipedia.org/wiki/Pareto_distribution
    # -n/(sum(log(data/xm)))
    #-(n*log(alpha) + n*log(xm)-(alpha +1)*sum(log(data)))# https://math.stackexchange.com/questions/1399209/how-to-find-the-maximum-likelihood-estimators-of-parameters-in-the-pareto-distri/1399292


    -(-length(data_u) * logb(scale) - (1 + 1/shape) * sum(logb(1 + (shape * exceso)/scale)))
  }
  optimizacion <- optim(c(0.1,0.1),fn = eq,hessian = T)

  resultados_fit <- tibble("Parametro"= c("scale", "shape"),
                           "Valores_optimos"= optimizacion$par)

  verosimilitud <- c(optimizacion$value)

  # RESUMEN------------------------------------------------------------------
  list("Valores_optimos"= tibble("Parametro"= c("scale", "shape"),
                                 "Valores_optimos"= optimizacion$par),

       "Negative_Log_Likelihood"=tibble(verosimilitud),
       "AIC" =2*2+2*verosimilitud,
       "BIC" = 2*verosimilitud+2*log(length(data)),

       "Desviaciones_tipicas_estimadas" = tibble("scale"=sqrt(solve(optimizacion$hessian)[1,1]),
                                                 "shape"=sqrt(solve(optimizacion$hessian)[2,2])),

       "Matriz_covarianzas" = tibble("scale"=solve(optimizacion$hessian)[1,],
                                     "shape"=solve(optimizacion$hessian)[2,]))
}



