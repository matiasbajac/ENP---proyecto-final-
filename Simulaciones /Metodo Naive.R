library(tidyverse)

set.seed(12345)

n <- 5000

W <- rnorm(n)
X <- rnorm(n)


Y <- X + W

datos <- data.frame(X, Y)

plot(X, Y)


##Ajustamos un modelo lineal simple 


fitlm <- lm(Y ~ X, data = datos)
## Sacamos los resiudos 

eVec_lm <- abs(fitlm$residuals)


### Regresión no paramétrica

fitnw <- ksmooth(
  X,
  Y,
  kernel = "normal",
  bandwidth = 0.2
)

## vemos los resiudos de la regresion no parametrica 
eVec_nw <- abs(fitnw$y - Y)


### Vemos los residuos de los dos 

par(mfrow = c(1, 2))
hist(eVec_nw)
hist(eVec_lm)

## Nos creamos una grilla de valores para y 

## intervalo de prediccion conformable Naive

Xnew <- seq(-4, 4, 0.50)

# regeresion paramétrica

muHat_lm <- predict(
  fitlm,
  newdata = data.frame(X = Xnew)
)

# Intervalos al 5% de confianza para lm
C.X_lm_lwr <- c(
  muHat_lm - quantile(eVec_lm, .975)
)

C.X_lm_uppr <- c(
  muHat_lm + quantile(eVec_lm, .975)
)

# regeresion no paramétrica
muHat_nw <- ksmooth(
  X,
  Y,
  kernel = "normal",
  bandwidth = 0.2,
  x.points = Xnew
)

# Intervalos al 5% de confianza para nw
C.X_nw_lwr <- c(
  muHat_nw$y - quantile(eVec_nw, .975)
)

C.X_nw_uppr <- c(
  muHat_nw$y + quantile(eVec_nw, .975)
)

resultados_conf_naive <- data.frame(
  Xnew,
  muHat_lm,
  muHat_nw$y,
  C.X_lm_lwr,
  C.X_lm_uppr,
  C.X_nw_lwr,
  C.X_nw_uppr
)

par(mfrow = c(1, 1))

plot(
  X,
  Y
)

# Plots Modelo lineal
lines(
  resultados_conf_naive$Xnew,
  resultados_conf_naive$muHat_lm,
  col = "red",
  cex = 1.5
)

lines(
  resultados_conf_naive$Xnew,
  resultados_conf_naive$C.X_lm_lwr,
  col = "red",
  cex = 1.5,
  lty = "dashed"
)

lines(
  resultados_conf_naive$Xnew,
  resultados_conf_naive$C.X_lm_uppr,
  col = "red",
  cex = 1.5,
  lty = "dashed"
)

# Plots Naradaya-Watson
lines(
  resultados_conf_naive$Xnew,
  resultados_conf_naive$muHat_nw,
  col = "blue",
  cex = 1.5
)

lines(
  resultados_conf_naive$Xnew,
  resultados_conf_naive$C.X_nw_lwr,
  col = "blue",
  cex = 1.5,
  lty = "dashed"
)

lines(
  Xnew,
  C.X_nw_uppr,
  col = "blue",
  cex = 1.5,
  lty = "dashed"
)