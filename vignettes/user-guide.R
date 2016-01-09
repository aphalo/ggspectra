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
             color = "white")

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = NULL, geom = "vline", linetype = "dotted") +
  stat_peaks(span = NULL, geom = "hline", linetype = "dotted")

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = 21, geom = "text", aes(label = ..y.label..))

## ------------------------------------------------------------------------
ggplot(sun.spct) + geom_line() + 
  stat_peaks(span = NULL) +
  stat_peaks(span = NULL, geom = "text", vjust = -0.5, size = 2.5,
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
             color = "black", 
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
  stat_wl_summary(range = c(300,350), geom = "rect") +
  geom_line()

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  geom_line() + 
  stat_wl_summary(geom = "hline", color = "red") +
  stat_wl_summary(label.fmt = "Mean = %.3g", color = "red", vjust = -0.3)

## ------------------------------------------------------------------------
ggplot(sun.spct) +
  stat_wl_summary(range = c(400,500), geom = "rect", alpha = 0.2, fill = color(450)) +
  stat_wl_summary(range = c(400,500), label.fmt = "Mean = %.3g", vjust = -0.3, geom = "text") + 
  geom_line()

## ------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(color = spct.idx) +
  geom_line() + 
  stat_wl_summary(geom = "hline") +
  stat_wl_summary(label.fmt = "Mean = %.3g", vjust = 1.2, show.legend = FALSE) +
  facet_grid(spct.idx~.)

## ------------------------------------------------------------------------
ggplot(two_suns.spct) + aes(color = spct.idx) +
  geom_line() + 
  stat_wl_summary(geom = "hline") +
  stat_wl_summary(label.fmt = "Mean = %.3g", vjust = 1.2, show.legend = FALSE) +
  facet_grid(spct.idx~., scales = "free_y")

