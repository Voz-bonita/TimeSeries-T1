pacman::p_load("Mcomp", "dplyr")

ids <- c(1277, 1903)

M3[[ids[1]]] %>% plot()
M3[[ids[2]]] %>% plot()