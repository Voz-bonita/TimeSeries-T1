pacman::p_load("Mcomp", "tseries", "dplyr", "glue", "forecast", "ggpubr", "purrr", "ggplot2", "stringr")

current_data <- M3[[1277]]

serie_t <- current_data$x

PERIOD_TO_VALUE <- c("QUARTERLY" = 4, "MONTHLY" = 1)

#

# Seleção modelo

#


(modelo <- Arima(serie_t, order = c(3, 1, 3)))
residuo <- modelo$residuals

# Visualização

png("assets/residuos1277.png")
par(mfrow = c(2, 2))
plot(residuo)
qqnorm(residuo)
qqline(residuo)
acf(residuo, lag.max = length(modelo))
pacf(residuo, lag.max = length(modelo))

dev.off()

# Teste de estacionaridade

kpss.test(residuo, null = "Trend")

# Teste de independência

Box.test(residuo, lag = 20, type = "Ljung-Box")

# Teste de normalidade

shapiro.test(residuo)