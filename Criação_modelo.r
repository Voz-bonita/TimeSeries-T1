pacman::p_load("Mcomp", "tseries", "dplyr", "glue", "forecast", "ggpubr", "purrr", "ggplot2", "stringr")
source("./auxiliar.r", encoding = "UTF-8")

# modelo

current_data <- M3[[1277]]
serie_t <- current_data$x

PERIOD_TO_VALUE <- c("QUARTERLY" = 4, "MONTHLY" = 1)

# seleção melhor modelo

plot(current_data)

# extraindo a tendencia da série

# tempo = as.numeric(time(current_data))
# out = lm(current_data$xx ~ poly(tempo,9))
# out2 = lm(X ~ poly(tempo,6))

# rejeita hipótese de estacionariedade, conforme é observado pelo plot que a série possui uma forte tendência, mostrando-se não estacionária.
kpss.test(serie_t, null = "Trend")

# Para tranformar em uma série estacionária: (raízes unitarias):
serie_t %>% ndiffs() # uma diferenciação necessária
serie_est <- diff(serie_t, differences = 1)
serie_est %>%
  autoplot() +
  geom_line(linewidth = 1.5) +
  theme_bw() +
ggsave("assets/Serie1277_diferenciada.png")

serie_est %>% nsdiffs() # 0
# Obtemos a série estacionária após as diferenciações


png("assets/Serie1277-acf_pacf.png")
## Gráficos ACF e PACF
par(mfrow = c(1, 2))
acf(serie_est, lag.max = length(serie_est))
pacf(serie_est, lag.max = length(serie_est))
dev.off()


## Gráficos ACF e PACF

acf(serie_est,lag.max = 12*7) 

#primeiro elemento sempre 1, com uma forte convergência para 0.

pacf(serie_est, lag.max = 12*7) 

# A quebra é logo no lag 1 com uma fraca convergência para 0.

# Pelo gráfico ACF de autocorrelações parciais, visualizamos que a série não apresenta correlações significativas, o que sugere que não há dependência de valores atuais em relação aos anteriores.

#SELEÇÃO MODELO SARIMA

# Dado que pelos lags sazonais não temos evidência de sazonalidade, temos que investigar qual é o melhor modelo ARIMA(p,d,q), lembrando que d = 1 dado 1 raiz unitaria.

melhor_AICc = 1e308

melhor_p = c()

melhor_q = c()

AICc = c()

for(p in 0:3){
  
  for(q in 0:3){
    
    fit = Arima(serie_t, order = c(p,1,q))
    
    if(fit$aicc < melhor_AICc){
      
      melhor_AICc = fit$aicc
      
      melhor_p <- c(melhor_p,p)
      
      melhor_q <- c(melhor_q,q)
      
      AICc <- c(AICc,melhor_AICc)
    }
  }
}

dt <- data.frame( "p" = melhor_p, "q" = melhor_q, "Valor de AICc" = AICc)

dt

#Pelos critério de menor AICc, o modelo escolhido é o p = 3 e q = 3. ARIMA(3,1,3)

modelo = Arima(serie_est, order=c(3,1,3))

modelo

#resíduos

plot(modelo$residuals)
