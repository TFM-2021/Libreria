
library(fitdistrplus)
library(ismev)
library(extRemes)
library(actuar)
data("danishmulti")
plot(danishmulti$Contents)
plot(wind$Albany)
y <- venice$r8
glm(Fort$Prec ~ Fort$obs)
tseries::adf.test(dowjones$Index)
TFMevt::fitGEV(dowjones$Index, c(1,1,1))

library(readr)
data <-read_csv("R/2644206.csv", col_types = cols(DATE = col_date(format = "%Y-%m-%d")))

plot(data$DATE,data$PRCP)
y <- data$PRCP

plot(density(y))
library(tidyverse)
data <- data %>%
  filter(PRCP != is.na(PRCP))
fit_fev <- fevd(y)
return.level(fit_fev)
fit <-TFMevt::fitGEV(y,c(1,1,1))
TFMevt::return_level(year = 5,fit)

plot(data$DATE,data$PRCP)

abline(h=10.079)



fevd(danishuni$Loss)



library(GEVcdn)
a <- rgev(10000,0.5,scale = 0.5928882,shape = 0.1165839)
plot(density(a))
b <- rgev(10000,50,scale = 0.5928882,shape = 0.6165839)
plot(density(b))
c <- append(a,b)
plot(c)
tseries::adf.test(c)

length(c)
data <- tibble("fecha"=1:20000,
               "serie"=c)
data
fit_glm <- glm(serie ~ fecha, data = data)
fit_glm$coefficients[2]



