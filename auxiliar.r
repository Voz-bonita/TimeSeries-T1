pacman::p_load("ggplot2")

ggplot_series <- function(plot_df, seasonal_lines = FALSE, period_value = 0) {
    plot <- ggplot(data = plot_df) +
        geom_line(aes(x = `Ano`, y = `Y`, color = `Tipo`), linewidth = 1) +
        scale_color_manual(values = c("red", "black")) +
        theme_bw()

    if (seasonal_lines) {
        seasonal_obs_date <- plot_df[["Ano"]][
            as.numeric(rownames(plot_df)) %% period_value == 0
        ]

        plot <- plot +
            geom_vline(xintercept = seasonal_obs_date, linetype = "dotted") +
            theme_void()
    }
    return(plot)
}