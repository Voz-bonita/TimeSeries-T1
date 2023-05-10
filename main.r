pacman::p_load("Mcomp", "dplyr", "glue", "forecast", "ggpubr", "purrr")
source("./auxiliar.r", encoding = "UTF-8")

PERIOD_TO_VALUE <- c("QUARTERLY" = 4, "MONTHLY" = 1)
ids <- c(1277, 1903)
ID <- ids[1]
current_data <- M3[[ID]]

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

period_value <- PERIOD_TO_VALUE[current_data$period]

# Serie seguida de facilitador da analise de sazonalidade
ggplot_series(plot_df) %>%
    ggsave(glue("assets/Serie{ID}_0.png"), .)
ggplot_series(plot_df, seasonal_lines = TRUE, period_value) %>%
    ggsave(glue("assets/Serie{ID}_s.png"), .)

xy_train <- current_data$x

windows <- seq(3, 9, by = 2)
decompositions <- map(windows, ~ stl(xy_train, s.window = .) %>%
    autoplot() + theme_bw())
args_ <- decompositions
args_[["ncol"]] <- 4
do.call(ggarrange, args_) %>%
    ggsave(glue("assets/Serie{ID}_stl.png"), .)