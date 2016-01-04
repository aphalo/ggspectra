## ------------------------------------------------------------------------
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)
library(ggspectra)

## ----setup, include = FALSE, eval = FALSE--------------------------------
#  library(svglite)
#  knitr::opts_chunk$set(
#    dev = "svglite",
#    fig.ext = ".svg"
#  )

## ---- include=FALSE, echo=FALSE------------------------------------------
library(knitr)
opts_chunk$set(fig.path = 'figure/pos-', fig.align = 'center', fig.show = 'hold',
               fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE)

## ------------------------------------------------------------------------
two_suns.spct <- rbindspct(list(sun1 = sun.spct, sun2 = sun.spct * 2))

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line()

## ------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(color = spct.idx) + geom_line()

## ------------------------------------------------------------------------
ggplot(two_suns.spct, aes(w.length, s.e.irrad, color = spct.idx)) + geom_line()

## ------------------------------------------------------------------------
ggplot(sun.spct, unit.out = "photon") + geom_line()

## ------------------------------------------------------------------------
ggplot(yellow_gel.spct) + geom_line()

## ------------------------------------------------------------------------
ggplot(yellow_gel.spct, plot.qty = "absorbance") + geom_line()

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + stat_peaks()

## ------------------------------------------------------------------------
ggplot(sun.spct, unit.out = "photon") + geom_line() + stat_peaks()

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + stat_valleys()

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(shape = 21) + scale_fill_identity()

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = 21, shape = 4, color = "red", size = 2)

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = 21, geom = "text")

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(shape = 21, span = 25, size = 2) + scale_fill_identity() +
  stat_peaks(geom = "label", span = 25, vjust = "bottom", size = 3, 
             color = "white", label.fmt = "%3.0f")

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = NULL, geom = "vline", linetype = "dotted") +
  stat_peaks(span = NULL, geom = "hline", linetype = "dotted")

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = 21, geom = "text", label.fmt = "%3.2f", aes(label = ..y.label..))

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = NULL) +
  stat_peaks(span = NULL, geom = "text", vjust = -0.5, size = 2.5,
             y.label.fmt = "%.3f",
             aes(label = paste(..y.label.., "at", ..x.label.. , "nm")))

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = 21, geom = "point", colour = "red") +
  stat_valleys(span = 21, geom = "point", colour = "blue") +
  stat_peaks(span = 51, geom = "text", colour = "red", 
             vjust = -0.3, label.fmt = "%3.0f nm") +
  stat_valleys(span = 51, geom = "text", colour = "blue", 
               vjust = 1.2, label.fmt = "%3.0f nm")

## ------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(color = spct.idx) +
  geom_line() + ylim(NA, 1.8) +
  stat_peaks(span = NULL, color = "black") +
  stat_peaks(span = NULL, geom = "text", vjust = -0.5, size = 3,
             y.label.fmt = "%.3f", color = "black", 
             aes(label = paste(..y.label.., "at", ..x.label.. , "nm"))) +
  facet_grid(spct.idx~.)

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  stat_color() + scale_color_identity()

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() +
  stat_color(shape = 21, color = "black") + 
  scale_fill_identity()

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  stat_color(geom = "bar") + 
  geom_point(shape = 21, color = "black", stroke = 1.2, fill = "white") +
  scale_fill_identity() + 
  scale_color_identity() + 
  theme_bw()

## ------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(shape = spct.idx) +
  stat_color() + scale_color_identity() +
  geom_line() + 
  facet_grid(spct.idx~., scales = "free_y")

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + stat_wl_summary()

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() +
  stat_wl_summary(label.fmt = "%.3f", color = "red")

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  stat_wl_summary(range = c(300,350), geom = "rect") +
  geom_line()

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() + 
  stat_wl_summary(geom = "hline", color = "red") +
  stat_wl_summary(label.fmt = "Mean = %.3f", color = "red", vjust = -0.3)

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wl_summary(range = c(400,500), geom = "rect", alpha = 0.2, fill = color(450)) +
  stat_wl_summary(range = c(400,500), label.fmt = "Mean = %.3f", vjust = -0.3, geom = "text") + 
  geom_line()

## ------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(color = spct.idx) +
  geom_line() + 
  stat_wl_summary(geom = "hline") +
  stat_wl_summary(label.fmt = "Mean = %.3f", vjust = 1.2, show.legend = FALSE) +
  facet_grid(spct.idx~.)

## ------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(color = spct.idx) +
  geom_line() + 
  stat_wl_summary(geom = "hline") +
  stat_wl_summary(label.fmt = "Mean = %.3f", vjust = 1.2, show.legend = FALSE) +
  facet_grid(spct.idx~., scales = "free_y")

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_summary(w.band = PAR(), geom = "rect", alpha = 0.5) +
  stat_wb_summary(w.band = PAR(), vjust = 1.2, geom = "text") +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity() +  
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_summary(w.band = c(400,500), geom = "rect", alpha = 0.5) +
  stat_wb_summary(w.band = c(400,500), geom = "text", y.multiplier = 0.5) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity() +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wb_summary(w.band = VIS_bands(), geom = "rect", alpha = 0.7) +
  stat_wb_summary(w.band = VIS_bands(), angle = 90, hjust = -0.1, geom = "text", 
                 aes(label = paste(..wb.name.., " = ", ..y.label.., sep = ""),
                     color = ..wb.color..)) +
  geom_line() + 
  scale_color_identity() + 
  scale_fill_identity() +
  ylim(NA, 1.1) + 
  theme_bw()

## ------------------------------------------------------------------------
my.data <- data.frame(x = 300:800)
ggplot(my.data, aes(x)) + stat_wl_strip(ymin = -1, ymax = 1) +
    scale_fill_identity()

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() +
  stat_wl_strip(ymin = -Inf, ymax = -0.025) + 
  scale_fill_identity() +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() + 
  stat_wl_strip(w.band = VIS_bands(), ymin = -Inf, ymax = -0.025) + 
  scale_fill_identity() +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wl_strip(w.band = VIS_bands(), ymin = -Inf, ymax = Inf, alpha = 0.4) + 
  scale_fill_identity() +
  geom_line() +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wl_strip(alpha = 0.4, ymin = -Inf, ymax = Inf) + 
  scale_fill_identity() +
  geom_line() +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wl_strip(alpha = 0.4, ymin = -Inf, ymax = Inf) + 
  stat_wb_summary(w.band = PAR(), ymin = -0.01, ymax = 0, color = "black", geom = "rect") +
  stat_wb_summary(w.band = PAR(), integral.fun = "total",
                vjust = -0.5, geom = "text", color = "black",
                y.position = 0,
                aes(label = paste(..wb.name.., " = ", ..y.label.., sep = ""))) +  
  geom_line() + 
  scale_fill_identity() +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_spct()

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_spct(fill = color(sun.spct))

## ------------------------------------------------------------------------
ggplot(sun.spct * yellow_gel.spct) + 
  geom_spct(fill = color(sun.spct * yellow_gel.spct))

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  wl_guide(alpha = 0.4) +
  geom_line() +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  wl_guide(ymax = -0.025) +
  geom_line() 

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  wl_guide() +
  geom_spct(alpha = 0.75, colour = "white", size = 1) 

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  wl_guide() +
  geom_line(size = 2, colour = "white") +
  geom_line(size = 1, colour = "black") +
  geom_hline(yintercept = 0, colour = "grey92") +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_spct() + 
  wb_guide(w.band = Plant_bands())

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_spct() + 
  wb_guide(w.band = Plant_bands(), guide.position = "top") +
  lims(y = c(NA, 1.1))

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_spct() + 
  wb_guide(w.band = Plant_bands(), guide.position = "top",
                 aes(label = sprintf("%s", ..wb.name..))) +
  ylim(NA, 1.1) + theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_spct() + 
  wb_guide(w.band = Plant_bands(), guide.position = "top",
                 aes(label = paste(..wb.name.., ..y.label.., sep = ": "))) +
  ylim(NA, 1.1)

## ------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_spct() + 
  wb_guide(w.band = Plant_bands(), guide.position = "top2",
                 aes(label = paste(..wb.name.., ..y.label.., sep = "\n"))) +
  ylim(NA, 1.1)

## ------------------------------------------------------------------------
plot(sun.spct)

## ------------------------------------------------------------------------
plot(sun.spct, label.qty = "mean")

## ------------------------------------------------------------------------
plot(sun.spct, label.qty = "contribution")

## ------------------------------------------------------------------------
plot(sun.spct, unit.out = "photon")

## ------------------------------------------------------------------------
plot(sun.spct, annotations = c("segments", "labels", "color.guide"))

## ------------------------------------------------------------------------
plot(sun.spct, 
     annotations = c("segments", "labels", "summaries", "color.guide"))

## ------------------------------------------------------------------------
plot(sun.spct, range = VIS())

## ------------------------------------------------------------------------
plot(sun.spct, w.band = PAR())

## ------------------------------------------------------------------------
plot(sun.spct, w.band = CIE())

## ------------------------------------------------------------------------
plot(sun.daily.spct)

## ------------------------------------------------------------------------
plot(two_suns.spct) + facet_wrap(~spct.idx)

## ------------------------------------------------------------------------
plot(two_suns.spct) + aes(linetype = spct.idx)

## ------------------------------------------------------------------------
plot(yellow_gel.spct, annotations = "colour.guide")

## ------------------------------------------------------------------------
plot(sun.spct) + geom_spct(fill = color(sun.spct)) + 
  geom_spct(data = yellow_gel.spct * sun.spct, color = "black", 
            fill = color(yellow_gel.spct * sun.spct))

