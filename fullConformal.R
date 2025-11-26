## Full Comformal 


set.seed(2025)

n <- 5000

W <- rnorm(n)
X <- rnorm(n)

Y <- X + W

datos <- data.frame(X, Y)



## Modelo Lineal  usando tidymodels 



lm_wf =workflow() %>% 
  add_model(parsnip::linear_reg() %>% set_engine("lm")) %>% 
  add_formula(Y ~ X)

lm_fit <- fit(lm_wf, data = datos)

residuos= augment(lm_fit,new_data=datos) %>% 
  select(.resid)


Xnew = seq(-4,4,.25) 

muHat_lm <- predict(
  lm_fit,
  new_data = data.frame(X = Xnew)
)


muHat_vector <- muHat_lm$.pred
residuosVec = residuos$.resid


## Creamos una grilla exhaustiva 

yCand <- seq(
  from = min(Y) - 1,
  to = max(Y) + 1,
  by = 0.1
)


confPredict <- function(y, Xin) {
  nData <- nrow(regData)
  regData.a <- rbind(regData, c(Xin, y))
  
  # Se ajusta el modelo de regresión
  fitlm.a <- lm(Y ~ X, data = regData.a)
  
  # Se calculan los residuos
  
  resOut <- abs(fitlm.a$residuals)
  
  
  
  # Se deja el último afuera
  resOut_new <- resOut[length(resOut)]
  
  # Se calcula el rango
  
  pi.y <- mean(
    apply(
      as.matrix(resOut),
      1,
      function(x) {
        x <= resOut_new
      }
    )
  )
  # Creamos el intervalo
  
  testResult <- pi.y * (nData + 1) <= ceiling(.975 * (nData + 1))
  
  return(testResult)
}

# Creamos una matriz para guardar los resultados de n_{new}x2

Cxa <- matrix(
  0,
  nrow = length(Xnew),
  ncol = 2
)

# Construimos los rangos

for (i in 1:length(Xnew)) {
  Cxa[i, ] <- range(
    yCand[sapply(yCand, confPredict, Xin = Xnew[i])]
  )
}

# Guardamos los resultados para X_{new}

resultados_full_conformal <- data.frame(
  Xnew,
  muHat_lm, # La estimacion puntual es la misma
  Cxa_lm_lwr_full = Cxa[, 1],
  Cxa_lm_uppr_full = Cxa[, 2],
  C.X_lm_lwr,
  C.X_lm_uppr
)

### Resultados (Comparamos con el intervalo simple)


ggplot(resultados_full_conformal , aes(x = Xnew)) +
  geom_point(data = datos, aes(x = X, y = Y), color = "black") +  # puntos originales
  
  # Modelo Lineal intervalo naive 
  geom_line(aes(y = muHat_vector), color = "red", size = 1.2) + 
  geom_line(aes(y = C.X_lm_lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = C.X_lm_uppr), color = "red", linetype = "dashed") +
  
  # intervalo full conformal
  geom_line(aes(y = Cxa_lm_lwr_full), color = "blue", size = 1.2,linetype = "dashed") + 
  geom_line(aes(y = Cxa_lm_uppr_full ), color = "blue", linetype = "dashed") +
  
  
  labs(title = "Comparación: Regresión Lineal, intervalo full vs conformal naive ",
       x = "X",
       y = "Y") +
  theme_minimal()
