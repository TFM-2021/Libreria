#' @title
#' @description Cálculo de la estacionariedad con Mann-Kendall. Si return > 0 = no estacionario
#' @param data serie
#' @return el histograma con la densidad normal superpuesta
#' @export stationary_test



Mann_kendall = function(data){ # the Mann-Kendall  test allows for non-linear trends in the location parameter and any form of distribution,

  n <- length(data())
  resultado <- n*(n - 1) / 2
  return(resultado)
}

# habría que añadir aqui el Likelihood ratio test
# while the likelihood ratio test assumes GEV distribution and
# only allows for a linear trend
# in the location parameter.
