#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' @param data vector de datos cuyo histograma se va a calcular
#' @param year
#' @return el histograma con la densidad normal superpuesta
#' @export return_level_GEV
#'
#'
#'


return_level_GEV = function(model_fit,
                             data,
                             year,
                             mode="calculation"){


  location <- model_fit$Valores_optimos[1,2]
  scale <- model_fit$Valores_optimos[2,2]
  shape <- model_fit$Valores_optimos[3,2]

  year <- year*365

  V <- as.matrix(model_fit$Matriz_covarianzas)


  if(mode == "calculation"){

    p <- 1/year


    y_p <- -log(1-p)


    Z <- location - scale/shape*(1-(-log(1-p))^(-shape))

    d_2 <- as.numeric( -shape^(-1)*(1-y_p^(-shape)))
    d_3 <- as.numeric(scale*shape^(-2)*(1-y_p^(-shape)) -scale*shape^(-1)*y_p^(-shape)*log(y_p))

    z_T_p <- c(1, d_2 , d_3)

    Var <- t(matrix(z_T_p)) %*% V %*% matrix(z_T_p)

    return(c(
      Z +1.96 * as.numeric(sqrt(Var)),
      Z,
      Z -1.96 * as.numeric(sqrt(Var))))


  }

  if (mode == "plot"){

    upper_band<- as.vector(NULL)
    return_level <- as.vector(NULL)
    lower_band <- as.vector(NULL)

    for (year_ in 1:year) {


      p <- 1/year_



      y_p <- -log(1-p)


      Z <- as.numeric(location - scale/shape*(1-(-log(1-p))^(-shape)))

      d_2 <- as.numeric( -shape^(-1)*(1-y_p^(-shape)))
      d_3 <- as.numeric(scale*shape^(-2)*(1-y_p^(-shape)) -scale*shape^(-1)*y_p^(-shape)*log(y_p))

      z_T_p <- c(1, d_2 , d_3)

      Var <- t(matrix(z_T_p)) %*% V %*% matrix(z_T_p)


      upper_band[year_] <- Z +1.96 * as.numeric(sqrt(Var))
      return_level[year_] <- Z
      lower_band[year_] <- Z -1.96 * as.numeric(sqrt(Var))

    }



    as.tibble(upper_band,
              return_level,
              lower_band) %>%
      ggplot()+
      geom_line(aes(1:year, return_level), color="tomato")+
      geom_line(aes(1:year, upper_band))+
      geom_line(aes(1:year, lower_band))+
      scale_x_log10()+
      labs(title =  "Return levels GEV: ",
           subtitle = paste0("location: ", round(location,2),
                             " scale: ", round(scale,2),
                             " shape: ", round(shape,2)),
           x = "Return day",
           y = "Return level",
           color = NULL) +

      theme_minimal()

  }





}
