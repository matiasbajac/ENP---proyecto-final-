n <- 5000

U <- rnorm(n)
X <- rnorm(n)


Y <- X + U

regData <- data.frame(X, Y)


plot(X, Y)



splitConfPredict <- function(Xin) {
  

  nData <- nrow(regData)
  regData$index <- 1:nData
  regData$split <- 1
  regData$split[sample(regData$index, floor(nrow(regData) / 2), replace = F)] <- 2
  fitlm.spl <- lm(Y ~ X, data = subset(regData, split == 1))
  resOut <- abs(
    subset(regData, split == 2)$Y -
      predict(fitlm.spl, newdata = subset(regData, split == 2))
  )
  kOut <- ceiling(((nData / 2) + 1) * (.975))
  resUse <- resOut[order(resOut)][kOut]
  
  Y.hat <- predict(fitlm.spl, newdata = data.frame(X = Xin))
  C.split <- c(Y.hat - resUse, Y.hat, Y.hat + resUse)
  return(C.split)
}

# Intervalo split

Csplit <- t(sapply(Xnew, FUN = splitConfPredict))


# Resultados

resultados_conf_split <- data.frame(
  Xnew,
  muHat_vector, # La estimacion puntual es la misma
  Csplit_lwr = Csplit[, 1],
  Csplit_uppr = Csplit[, 3],
  Cxa_lm_lwr = C.X_lm_lwr,
  Cxa_lm_uppr = C.X_lm_uppr
)

ggplot(resultados_conf_split , aes(x = Xnew)) +
  geom_point(data = datos, aes(x = X, y = Y), color = "black") +  # puntos originales
  
  # Modelo Lineal intervalo naive 
  geom_line(aes(y = muHat_vector), color = "red", size = 1.2) + 
  geom_line(aes(y = C.X_lm_lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = C.X_lm_uppr), color = "red", linetype = "dashed") +
  
  # intervalo conformal 
  geom_line(aes(y = Csplit_lwr), color = "blue", size = 1.2,linetype = "dashed") + 
  geom_line(aes(y = Csplit_uppr ), color = "blue", linetype = "dashed") +

  
  labs(title = "Comparación: Regresión Lineal, intervalo conformal naive vs intervalo split",
       x = "X",
       y = "Y") +
  theme_minimal()
