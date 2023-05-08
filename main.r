pacman::p_load("Mcomp", "dplyr", "ggplot2")

PERIOD_TO_VALUE <- c("QUARTERLY" = 4, "MONTHLY" = 1)
ids <- c(1277, 1903)

current_data <- M3[[ids[1]]]
current_data %>% plot()
current_data$n
xy <- c(as.vector(current_data$x), as.vector(current_data$xx))
xy_time <- c(as.vector(time(current_data$x)), as.vector(time(current_data$xx)))

plot_df <- data.frame(
    "Ano" = xy_time,
    "Y" = xy,
    "Tipo" = c(
        rep("Treino", current_data$n),
        rep("Teste", current_data$h)
    )
)

ggplot(data = plot_df) +
    geom_line(aes(x = `Ano`, y = `Y`, color = `Tipo`), size = 1) +
    scale_color_manual(values = c("red", "black")) +
    theme_bw()


period_value <- PERIOD_TO_VALUE[current_data$period]
seasonal_obs_date <- plot_df[["Ano"]][
    as.numeric(rownames(plot_df)) %% period_value == 0
]

ggplot(data = plot_df) +
    geom_line(aes(x = `Ano`, y = `Y`, color = `Tipo`), size = 1) +
    geom_vline(xintercept = seasonal_obs_date, linetype = "dotted") +
    scale_color_manual(values = c("red", "black")) +
    theme_void()

xy_train <- current_data$x
stl(xy_train, s.window = "periodic") %>% plot()