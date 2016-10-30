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
opts_chunk$set(fig.path = 'figure/plot-pos-', fig.align = 'center', fig.show = 'hold',
               fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE)

## ------------------------------------------------------------------------
two_suns.spct <- rbindspct(list(sun1 = sun.spct, sun2 = sun.spct * 2))

## ------------------------------------------------------------------------
theme_set(theme_bw(10))

## ------------------------------------------------------------------------
plot(sun.spct)

## ------------------------------------------------------------------------
plot(sun.spct, label.qty = "mean")

## ------------------------------------------------------------------------
plot(sun.spct, label.qty = "contribution")

## ------------------------------------------------------------------------
plot(sun.spct, label.qty = "relative")

## ------------------------------------------------------------------------
plot(sun.spct, label.qty = "mean", unit.out = "photon")

## ------------------------------------------------------------------------
plot(sun.spct, label.qty = "relative.pc")

## ------------------------------------------------------------------------
plot(sun.spct, unit.out = "photon")

## ------------------------------------------------------------------------
plot(sun.spct, 
     annotations = c("segments", "labels", "summaries", "color.guide"))

## ------------------------------------------------------------------------
plot(sun.spct, 
     annotations = NULL)

## ------------------------------------------------------------------------
plot(sun.spct, annotations = c("segments", "labels", "color.guide"))

## ------------------------------------------------------------------------
plot(sun.spct, 
     annotations = c("segments", "labels", "color.guide"), 
     text.size = 3.5)

## ------------------------------------------------------------------------
plot(sun.spct, range = VIS())

## ------------------------------------------------------------------------
plot(sun.spct, w.band = PAR())

## ------------------------------------------------------------------------
plot(sun.spct, w.band = CIE())

## ------------------------------------------------------------------------
plot(sun.spct, w.band = NULL)

## ------------------------------------------------------------------------
plot(sun.spct, w.band = NULL, range = c(400,700))

## ------------------------------------------------------------------------
plot(sun.spct, w.band = NULL, range = PAR())

## ------------------------------------------------------------------------
plot(sun.spct, w.band = PAR(), range = PAR())

## ------------------------------------------------------------------------
plot(sun.spct, w.band = VIS_bands(), range = VIS())

## ------------------------------------------------------------------------
plot(sun.daily.spct)

## ------------------------------------------------------------------------
plot(two_suns.spct, label.qty = "mean") + facet_wrap(~spct.idx)

## ------------------------------------------------------------------------
plot(two_suns.spct) + aes(linetype = spct.idx)

## ------------------------------------------------------------------------
plot(sun.spct) + geom_spct(fill = color(sun.spct)) + 
  geom_spct(data = yellow_gel.spct * sun.spct, color = "black", 
            fill = color(yellow_gel.spct * sun.spct))

## ------------------------------------------------------------------------
plot(yellow_gel.spct)

## ------------------------------------------------------------------------
plot(yellow_gel.spct, pc.out = TRUE)

## ------------------------------------------------------------------------
plot(yellow_gel.spct, plot.qty = "absorbance")

## ------------------------------------------------------------------------
yellow_gel.spct$Rfr <- 1 - max(yellow_gel.spct$Tfr)
plot(yellow_gel.spct, plot.qty = "absorptance")

## ------------------------------------------------------------------------
plot(sun.spct) + geom_spct(fill = color(sun.spct)) + 
  geom_spct(data = yellow_gel.spct * sun.spct, color = "black", 
            fill = color(yellow_gel.spct * sun.spct)) +
  stat_peaks(data = yellow_gel.spct * sun.spct, color = "yellow", 
             ignore_threshold = 0.1, span = 21)

## ------------------------------------------------------------------------
two_suns.mspct <- source_mspct(list(sun1 = sun.spct, sun2 = sun.spct * 2))

## ---- fig.width = 7, fig.height = 8--------------------------------------
multiplot(plotlist = mslply(two_suns.mspct, plot))

## ---- fig.width = 7, fig.height = 8--------------------------------------
plot(rbindspct(two_suns.mspct)) + facet_wrap(~spct.idx, ncol = 1)

## ------------------------------------------------------------------------
plot(rbindspct(two_suns.mspct), 
     annotations = c("color.guide", "labels", "boxes", "peaks")) + 
  aes(linetype = spct.idx)

## ------------------------------------------------------------------------
ggplot(rbindspct(two_suns.mspct)) + 
  aes(linetype = spct.idx) +
  wl_guide(ymax = -0.05) +
  geom_line()

## ------------------------------------------------------------------------
plot(VIS())

## ------------------------------------------------------------------------
plot(CIE(), range = CIE())

## ------------------------------------------------------------------------
plot(DNA_N(), range = c(270, 420))

