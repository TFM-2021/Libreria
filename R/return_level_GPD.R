#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' @param data vector de datos cuyo histograma se va a calcular
#' @param year
#' @return el histograma con la densidad normal superpuesta
#' @export return_level_GPD
#'



return_level_GPD = function(model_fit, data, threshold, year){
  scale <-model_fit$Valores_optimos[1,2]
  shape <- model_fit$Valores_optimos[2,2]

  scale_se <- model_fit$Desviaciones_tipicas_estimadas[1]
  shape_se <- model_fit$Desviaciones_tipicas_estimadas[2]



  V <- matrix(NA,3,3)

  V[2,] <- c(0,
             as.numeric(fit_gpd_yo$Matriz_covarianzas[1,1]),
             as.numeric(fit_gpd_yo$Matriz_covarianzas[2,1]))

  V[3,] <- c(0,
             as.numeric(fit_gpd_yo$Matriz_covarianzas[1,2]),
             as.numeric(fit_gpd_yo$Matriz_covarianzas[2,2]))

  rate <- length(data[data>threshold])/length(data)


  V[1,] <- c(rate*(1-rate)/length(data),0,0)


  m <- 365* year

  x_m <- as.numeric(threshold+scale/shape*(((m*rate)^shape)-1))

  d_1 <- as.numeric((scale*m^shape)*rate^(shape - 1))
  d_2 <- as.numeric(shape^-1*((m*rate)^shape-1))
  d_3 <- as.numeric(-scale*shape^-2*((m*rate)^shape-1)+scale*shape^-1*((m*rate)^shape)*log(m*rate))


  x_m_delta <- c(d_1,d_2,d_3)

  Var <- t(matrix(x_m_delta)) %*% V %*% matrix(x_m_delta)

  c(x_m + 1.96*sqrt(Var),
    x_m - 1.96*sqrt(Var))

}
