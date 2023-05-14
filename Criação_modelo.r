pacman::p_load("Mcomp",'tseries', "dplyr", "glue", "forecast", "ggpubr", "purrr",'ggplot2','stringr')

#modelo

current_data = M3[[1227]]

serie_t = current_data$x

PERIOD_TO_VALUE <- c("QUARTERLY" = 4, "MONTHLY" = 1)

#seleção melhor modelo

plot(current_data)

# extraindo a tendencia da série

#tempo = as.numeric(time(current_data))
#out = lm(current_data$xx ~ poly(tempo,9))
#out2 = lm(X ~ poly(tempo,6))

# rejeita hipótese de estacionariedade, conforme é observado pelo plot que a série possui uma forte tendência, mostrando-se não estacionária.

kpss.test(current_data$x) 

# Para tranformar em uma série estacionária: (raízes unitarias):

serie_est <- diff(serie_t) %>% diff(lag=12) #verificar porque 12

plot(serie_est, main= 'Série Estacionária')

#Obtermos a série estacionária após as diferenciações


## Gráficos ACF e PACF

acf(serie_est,lag.max = 12*7) 
#primeiro elemento sempre 1, com uma leve convergência para 0.

pacf(serie_est, lag.max = 12*7) 

# A quebra é logo no lag 3. (?)

# Pelos gráfico PACF não existe 

# Pelo gráfico ACF de autocorrelações parciais, visualizamos que a série não apresenta correlações significativas, o que sugere que não há dependencia de valores atuais em relação aos anteriores.

#SELEÇÃO MODELO SARIMA

# Dado que notamos uma quebra no p = 3, se faz necessária uma correção pela média móvel (q = 3), assim temos que investigar qual é o melhor modelo P=0 e Q=3. 

melhor_AICc = Inf

melhor_p = c()

melhor_q = c()

AICc = c()

for(p in 0:3){
  
  for(q in 0:3){
    
    fit = Arima(serie_t, order = c(p,3,q), seasonal = c(0,1,1))
    
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

#Pelos critério de menor AICc, o modelo escolhido é o p = 0 e q = 2. SARIMA(0,1,2)x(0,1,1)

modelo = Arima(serie_est, order=c(0,1,2), seasonal=c(0,1,1))

modelo

#resíduos

plot(modelo$residuals)
