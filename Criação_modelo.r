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
    theme_bw()
ggsave("assets/Serie1277_diferenciada.png")

serie_est %>% nsdiffs() # 0
# Obtemos a série estacionária após as diferenciações


png("assets/Serie1277-acf_pacf.png")
## Gráficos ACF e PACF
par(mfrow = c(1, 2))
acf(serie_est)
pacf(serie_est)
dev.off()

# SELEÇÃO MODELO SARIMA

# Dado que pelos lags sazonais não temos evidência de sazonalidade, temos que investigar qual é o melhor modelo ARIMA(p,d,q), lembrando que d = 1 dado 1 raiz unitaria.

p_search <- c()
q_search <- c()
AICc_search <- c()

for (p in 0:3) {
    for (q in 0:3) {
        fit <- Arima(serie_t, order = c(p, 1, q))
        AICc_search <- c(AICc_search, fit$aicc)
        p_search <- c(p_search, p)
        q_search <- c(q_search, q)
    }
}

data.frame(p = p_search, q = q_search, AICc = AICc_search) %>%
    arrange(AICc) %>%
    format_tab("\\label{tab:params_search}AICc calculados para tamanhos pequenos dos parâmetros p e q", "latex", digits = 2)