library(ggspectra)
library(dplyr)
library(rlang)
p <-
ggplot(sun.spct) +
geom_line() +
geom_line(data = . %>% smooth_spct(method = "supsmu"), colour = "red", size = 1.2)

svg("~/ggspectra-0-3-5.svg", width = 7, height = 4)
print(p)
dev.off()
