pacman::p_load("Mcomp", "dplyr")

ids <- c(1277, 1903)

current_data <- M3[[ids[1]]]
current_data %>% plot()

xy_train <- current_data$x
stl(xy_train, s.window = "periodic") %>% plot()