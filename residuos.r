pacman::p_load("Mcomp", "tseries", "dplyr", "glue", "forecast", "ggpubr", "purrr", "ggplot2", "stringr")

current_data <- M3[[1277]]

serie_t <- current_data$x

PERIOD_TO_VALUE <- c("QUARTERLY" = 4, "MONTHLY" = 1)

serie_est <- diff(serie_t, differences = 1)

#

# Seleção modelo

#


modelo = Arima(serie_est, order=c(3,1,3))

modelo

plot(modelo$residuals)

#resíduos

residuo = modelo$residuals %>% window(start = c(1978,1))

plot(residuo, main = "Resíduos sem 0's da inicialização")



# Visualização 

par(mfrow = c(1,3))

plot(residuo);
qqnorm(residuo);
qqline(residuo)
acf(residuo, lag.max = length(modelo))

dev.off()

# Teste de estacionaridade

kpss.test(residuo, null = 'Trend')

# Teste de independência

Box.test(residuo, lag = 20, type = "Ljung-Box")

# Teste de normalidade

shapiro.test(residuo)
