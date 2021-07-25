





pp_base <- function(data, shape = 1, location = 0, scale = 1){

  quantiles <- location + (scale/shape) * (( - logb(ppoints(data)))^( - shape) - 1)

  exp( - (1  + (shape * (quantiles - location))/scale)^(-1 /shape))
  plot(seq(0,1,1/(length(data)-1)),pp_base(data))

}
