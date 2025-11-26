library(tidyverse)
library(tidymodels)

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

C.X_lm_lwr <- c(
  muHat_vector - quantile(residuosVec, .975)
)

C.X_lm_uppr <- c(
  muHat_vector + quantile(residuosVec, .975)
)


## Random forest usando tidymodels 

rf_spec <- rand_forest(mode = "regression") %>%
  set_engine("ranger")

rf_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_formula(Y ~ X)

rf_fit <- fit(rf_wf, data = datos)

pred_rf <- predict(rf_fit, new_data = datos) %>%
  pull(.pred)

residuos_rf <- datos$Y - pred_rf

muHat_rf <- predict(
  rf_fit,
  new_data = data.frame(X = Xnew)
)


muHat_vector_rf <- muHat_rf$.pred


C.X_rf_lwr<- c(
  muHat_rf - quantile(residuos_rf, .975)
)
C.X_rf_lwr = C.X_rf_lwr$.pred

C.X_rf_uppr <- c(
  muHat_rf + quantile(residuos_rf, .975)
)

C.X_rf_uppr = C.X_rf_uppr$.pred


resultados_conf_naive <- data.frame(
  Xnew,
  muHat_vector,
  muHat_vector_rf,
  C.X_lm_lwr,
  C.X_lm_uppr,
 C.X_rf_lwr,
  C.X_rf_uppr
)




ggplot(resultados_conf_naive, aes(x = Xnew)) +
  geom_point(data = datos, aes(x = X, y = Y), color = "black") +  # puntos originales
  
  # Modelo Lineal
  geom_line(aes(y = muHat_vector), color = "red", size = 1.2) + 
  geom_line(aes(y = C.X_lm_lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = C.X_lm_uppr), color = "red", linetype = "dashed") +
  
  # Modelo Random Forest
  geom_line(aes(y = muHat_vector_rf), color = "blue", size = 1.2) + 
  geom_line(aes(y = C.X_rf_lwr), color = "blue", linetype = "dashed") +
  geom_line(aes(y = C.X_rf_uppr), color = "blue", linetype = "dashed") +
  
  labs(title = "Comparación: Regresión Lineal vs Random Forest (Naive Prediction Bands)",
       x = "X",
       y = "Y") +
  theme_minimal()

