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

## ------------------------------------------------------------------------
library(knitr)
opts_chunk$set(fig.path = 'figure/pos-', fig.align = 'center', fig.show = 'hold',
               fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE)

## ------------------------------------------------------------------------
two_suns.spct <- rbindspct(list(sun1 = sun.spct, sun2 = sun.spct * 2))

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_peaks()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_valleys()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_peaks(shape = 21) + scale_fill_identity()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_peaks(shape = 21, span = 25, size = 2) + scale_fill_identity() +
  stat_peaks(geom = "label", span = 25, vjust = "bottom", size = 3, 
             color = "white", label.fmt = "%3.0f")

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_peaks(span = NULL, geom = "vline", linetype = "dotted") +
  stat_peaks(span = NULL, geom = "hline", linetype = "dotted")

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_peaks(span = 21, shape = 4, color = "red", size = 2)

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_peaks(span = 21, geom = "text")

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_peaks(span = 21, geom = "text", aes(label = ..y.label..))

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_peaks(span = NULL) +
  stat_peaks(span = NULL, geom = "text", vjust = -0.5, size = 2.5,
             y.label.fmt = "%.3f",
             aes(label = paste(..y.label.., "at", ..x.label.. , "nm")))

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_peaks(span = 21, geom = "point", colour = "red") +
  stat_valleys(span = 21, geom = "point", colour = "blue") +
  stat_peaks(span = 51, geom = "text", colour = "red", 
             vjust = -0.3, label.fmt = "%3.0f nm") +
  stat_valleys(span = 51, geom = "text", colour = "blue", 
               vjust = 1.2, label.fmt = "%3.0f nm")

## ------------------------------------------------------------------------
ggplot(two_suns.spct, aes(w.length, s.e.irrad, color = spct.idx)) +
  geom_line() + ylim(NA, 1.8) +
  stat_peaks(span = NULL, color = "black") +
  stat_peaks(span = NULL, geom = "text", vjust = -0.5, size = 3,
             y.label.fmt = "%.3f", color = "black", 
             aes(label = paste(..y.label.., "at", ..x.label.. , "nm"))) +
  facet_grid(spct.idx~.)

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_color() + scale_color_identity()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_color(shape = 21, color = "black") + scale_fill_identity()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + 
  stat_color(geom = "bar") + stat_color(shape = 21, color = "black", stroke = 1.2, fill = "white") +
  scale_fill_identity() + scale_color_identity() + theme_bw()

## ------------------------------------------------------------------------
ggplot(two_suns.spct, aes(w.length, s.e.irrad, shape = spct.idx)) +
  stat_color() + scale_color_identity() +
  geom_line() + 
  facet_grid(spct.idx~., scales = "free_y")

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_average()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() +
  stat_average(range = c(300,500), label.fmt = "%.3f", color = "red")

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) +
  stat_average(geom = "hline", color = "red") +
  geom_line() + stat_average(label.fmt = "Mean = %.3f", color = "red", vjust = -0.3)

## ------------------------------------------------------------------------
ggplot(two_suns.spct, aes(w.length, s.e.irrad, color = spct.idx)) +
  stat_average(geom = "hline") +
  geom_line() + stat_average(label.fmt = "Mean = %.3f", vjust = -0.3, show.legend = FALSE)

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) +
  stat_average(range = c(400,500), geom = "rect", alpha = 0.2, fill = color(450)) +
  geom_line() + 
  stat_average(range = c(400,500), label.fmt = "Mean = %.3f", vjust = -0.3, geom = "text")

## ------------------------------------------------------------------------
ggplot(two_suns.spct, aes(w.length, s.e.irrad, color = spct.idx)) +
  stat_average(geom = "hline") +
  geom_line() + 
  stat_average(label.fmt = "Mean = %.3f", vjust = 1.2, show.legend = FALSE) +
  facet_grid(spct.idx~.)

## ------------------------------------------------------------------------
ggplot(two_suns.spct, aes(w.length, s.e.irrad, color = spct.idx)) +
  stat_average(geom = "hline") +
  geom_line() + 
  stat_average(label.fmt = "Mean = %.3f", vjust = 1.2, show.legend = FALSE) +
  facet_grid(spct.idx~., scales = "free_y")

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) +
  geom_line() + scale_color_identity() + scale_fill_identity() +
  stat_waveband(w.band = PAR(), 
                 geom = "rect", alpha = 0.5) +
  stat_waveband(w.band = PAR(), vjust = -0.3, geom = "text") +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) +
  geom_line() + scale_color_identity() + scale_fill_identity() +
  stat_waveband(w.band = c(400,500), 
                 geom = "rect", alpha = 0.5) +
  stat_waveband(w.band = c(400,500), vjust = -0.3, geom = "text") +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) +
  geom_line() + scale_color_identity() + scale_fill_identity() +
  stat_waveband(w.band = VIS_bands(), 
                 geom = "rect", alpha = 0.7) +
  stat_waveband(w.band = VIS_bands(), angle = 90, hjust = -0.1, geom = "text", 
                 aes(label = paste(..wb.name.., " = ", ..y.label.., sep = ""))) +
  ylim(NA, 0.9) + theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) +
  geom_line() + scale_fill_identity() +
  stat_color_guide(w.band = VIS_bands(), ymin = -0.03, ymax = 0) + 
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) +
  geom_line() + scale_fill_identity() +
  stat_color_guide(ymin = -0.03, ymax = 0) + 
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) +
  stat_color_guide(ymin = -0.3, ymax = 1, alpha = 0.4) + 
  geom_line() + scale_fill_identity() +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) +
  stat_color_guide(ymin = -0.3, ymax = 1, alpha = 0.4) + 
  stat_waveband(w.band = PAR(), ymin = -0.01, ymax = 0, color = "black", geom = "rect") +
  stat_waveband(w.band = PAR(), vjust = 0, geom = "text", color = "black",
                aes(label = paste(..wb.name.., " = ", ..y.label.., sep = ""))) +  
  geom_line() + 
  scale_fill_identity() +
  theme_bw()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_spct()

## ------------------------------------------------------------------------
ggplot(sun.spct, aes(w.length, s.e.irrad)) + 
  geom_spct(fill = color(sun.spct)[1])

## ------------------------------------------------------------------------
plot(sun.spct)

## ------------------------------------------------------------------------
plot(sun.spct, unit.out = "photon")

## ------------------------------------------------------------------------
plot(sun.spct, range = VIS())

## ------------------------------------------------------------------------
plot(sun.daily.spct)

## ------------------------------------------------------------------------
plot(two_suns.spct) + facet_wrap(~spct.idx)

## ------------------------------------------------------------------------
plot(two_suns.spct) + aes(linetype = spct.idx)

## ------------------------------------------------------------------------
plot(yellow_gel.spct, annotations = "colour.guide")

## ------------------------------------------------------------------------
plot(sun.spct, annotations = "colour.guide") %+% geom_spct(fill = color(sun.spct)[1]) + 
  geom_spct(data = yellow_gel.spct * sun.spct, color = "black", 
            fill = color(yellow_gel.spct * sun.spct)[1])

