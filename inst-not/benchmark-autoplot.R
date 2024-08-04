library(lubridate)
library(ggspectra)
library(bench)
library(dplyr)
library(forcats)
# library(profvis)

rm(list = ls(pattern = "*"))


# one spectrum: sun.spct --------------------------------------------------

p00 <- ggplot(sun.spct) + geom_line()

p01 <- autoplot(sun.spct)

# ggpg <- ggplotGrob(p1)

# profvis(
#   ggpg <- ggplotGrob(p1),
#   interval = 0.005,
#   rerun = TRUE
#)

p02 <- autoplot(sun.spct, annotations = "")

# ggpg <- ggplotGrob(p2)

# profvis(
#   ggpg <- ggplotGrob(p2),
#   interval = 0.005,
#   rerun = TRUE
# )

p03 <- autoplot(sun.spct, annotations = "peaks")

# ggpg <- ggplotGrob(p3)

p04 <- autoplot(sun.spct, annotations = "colour.guide")

# ggpg <- ggplotGrob(p4)

p05 <- autoplot(sun.spct, annotations = "summaries")

p06 <- autoplot(sun.spct, annotations = c("+", "title:objt:when:what"))

p07 <- autoplot(sun.spct, annotations = "labels")

p08 <- p01 + theme_minimal()

p09 <- p02 + theme_minimal()


# five spectra: sun_evening.spct and sun_evening.mspct --------------------

length(sun_evening.mspct)
getMultipleWl(sun_evening.spct)

p10 <- autoplot(sun_evening.spct)
p11 <- autoplot(sun_evening.mspct)
p12 <- autoplot(sun_evening.spct, annotations = "")
p13 <- autoplot(sun_evening.mspct, annotations = "")
p14 <- autoplot(sun_evening.mspct, annotations = "", norm = "skip")
p15 <- autoplot(sun_evening.spct, facets = 2)
p16 <- autoplot(sun_evening.mspct, facets = 2)
p17 <- autoplot(sun_evening.spct, plot.data = "mean")
p18 <- autoplot(sun_evening.mspct, plot.data = "mean")
p19 <- autoplot(sun_evening.spct, plot.data = "mean", annotations = "")
p20 <- autoplot(sun_evening.mspct, plot.data = "mean", annotations = "")
p21 <- autoplot(sun_evening.spct, facets = 1)
p22 <- autoplot(sun_evening.mspct, facets = 1)

gc()
bench::mark(ggplotGrob(p00),
            ggplotGrob(p01),
            ggplotGrob(p02),
            ggplotGrob(p03),
            ggplotGrob(p04),
            ggplotGrob(p05),
            ggplotGrob(p06),
            ggplotGrob(p07),
            ggplotGrob(p08),
            ggplotGrob(p09),

            ggplotGrob(p10),
            ggplotGrob(p11),
            ggplotGrob(p12),
            ggplotGrob(p13),
            ggplotGrob(p14),
            ggplotGrob(p15),
            ggplotGrob(p16),
            ggplotGrob(p17),
            ggplotGrob(p18),
            ggplotGrob(p19),
            ggplotGrob(p20),
            ggplotGrob(p21),
            ggplotGrob(p22),
            check = FALSE,
            min_iterations = 25) -> z

nrow(z)

z$num.spectra <- factor(c(rep("1", 10), rep("5", 13)), levels = c("1", "5"))
class(z)
print(z)

z |> mutate(expression = fct_reorder(as.character(expression),
                                     median, .desc = TRUE)) |>
  as_bench_mark() |>
  autoplot("boxplot") -> fig

base.name <- paste("bench-autoplot-",
                   as.character(today()),
                   "--ggspectra-",
                   packageVersion("ggspectra"),
                   "--ggplot2-",
                   packageVersion("ggplot2"), "--",
                   paste("R-", R.version$major, ".", R.version$minor, sep = ""),
                   "--nodename-",
                   Sys.info()["nodename"],
                   sep = "")
comment(z) <- base.name


fig <- fig + labs(title = paste("Benchmarks of", as.character(today())),
                  subtitle = base.name)
print(fig)

object.name <- paste(base.name, "-tb", sep = "")
assign(object.name, z)

figure.name <- paste(base.name, "-fig", sep = "")
assign(figure.name, fig)

plots.name <- paste(base.name, "-gg-list", sep = "")
assign(plots.name, mget(x = ls(pattern = "^p[0-9]?")))

info.name <- paste(base.name, "-session", sep = "")

assign(info.name, sessionInfo())

file.path <-  paste("./inst-not/", base.name, ".rda", sep = "")

save(list = c(object.name, figure.name, plots.name, info.name), file = file.path)

pdf.file.name <- paste("./inst-not/", base.name, ".pdf", sep = "")
pdf(pdf.file.name, width = 7, height = 7)
print(fig)
dev.off()
