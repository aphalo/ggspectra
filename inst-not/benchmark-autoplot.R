library(ggspectra)
# library(profvis)

p1 <- autoplot(sun.spct)

ggpg <- ggplotGrob(p1)

# profvis(
#   ggpg <- ggplotGrob(p1),
#   interval = 0.005,
#   rerun = TRUE
#)

p2 <- autoplot(sun.spct, annotations = "")

ggpg <- ggplotGrob(p2)

# profvis(
#   ggpg <- ggplotGrob(p2),
#   interval = 0.005,
#   rerun = TRUE
# )

p3 <- autoplot(sun.spct, annotations = "peaks")

ggpg <- ggplotGrob(p3)

p4 <- autoplot(sun.spct, annotations = "colour.guide")

ggpg <- ggplotGrob(p4)

p5 <- autoplot(sun.spct, annotations = "summaries")

ggpg <- ggplotGrob(p5)


gc()
bench::mark(ggplotGrob(p1),
            ggplotGrob(p2),
            ggplotGrob(p3),
            ggplotGrob(p4),
            ggplotGrob(p5),
            check = FALSE,
            min_iterations = 10)


p10 <- autoplot(sun_evening.spct)
p11 <- autoplot(sun_evening.mspct)
p12 <- autoplot(sun_evening.spct, annotations = "")
p13 <- autoplot(sun_evening.mspct, annotations = "")
p14 <- autoplot(sun_evening.mspct, annotations = "", norm = "skip")
p15 <- autoplot(sun_evening.spct, facets = 2)
p16 <- autoplot(sun_evening.mspct, facets = 2)

gc()
bench::mark(ggplotGrob(p10),
            ggplotGrob(p11),
            ggplotGrob(p12),
            ggplotGrob(p13),
            ggplotGrob(p14),
            ggplotGrob(p15),
            ggplotGrob(p16),
            check = FALSE,
            min_iterations = 10)

