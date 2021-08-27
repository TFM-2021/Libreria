#' @description Funcion que dibuja el histograma de una variable x, superponiendo la densidad normal
#' @param data vector de datos cuyo histograma se va a calcular
#' @param year
#' @return el histograma con la densidad normal superpuesta
#' @export return_level_Gumbel
#'
#'
#'


return_level_Gumbel = function(model_fit,
                            data,
                            year,
                            mode="calculation"){


  location <- model_fit$Valores_optimos[1,2]
  scale <- model_fit$Valores_optimos[2,2]
  year <- 365 *year

  if(mode == "calculation"){

    p <- 1/year


    Z <- as.numeric(location-scale*log(-log(1-p)))



    return(Z)


  }

  if (mode == "plot"){

    return_level <- as.vector(NULL)
    for (year_ in 1:year) {


      p <- 1/year_


      Z <- as.numeric(location-scale*log(-log(1-p)))


      return_level[year_] <- Z
    }



    as.tibble(
              return_level
              ) %>%
      ggplot()+
      geom_line(aes(1:year, return_level), color="tomato")+
      scale_x_log10()+
      labs(title =  "Return levels Gumbel: ",
           subtitle = paste0("location: ", round(location,2),
                             " scale: ", round(scale,2)),
           x = "Return day",
           y = "Return level",
           color = NULL) +

      theme_minimal()

  }



}
