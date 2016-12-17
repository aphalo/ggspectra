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
     annotations = c("=", "segments", "labels", "summaries", "color.guide", "peaks"))

## ------------------------------------------------------------------------
plot(sun.spct, 
     annotations = c("+", "segments"))

## ------------------------------------------------------------------------
plot(sun.spct, annotations = c("-", "summaries", "peaks"))

## ------------------------------------------------------------------------
plot(sun.spct, annotations = c("+", "valleys"), span = 41)

## ------------------------------------------------------------------------
plot(sun.spct, annotations = c("+", "peak.labels", "valley.labels"), span = 41)

## ------------------------------------------------------------------------
plot(sun.spct, annotations = "")

## ------------------------------------------------------------------------
plot(sun.spct, annotations = "reserve.space")

## ------------------------------------------------------------------------
plot(sun.spct, annotations = c("=", "segments", "labels", "color.guide"), 
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
getTimeUnit(sun.daily.spct)
plot(sun.daily.spct)

## ------------------------------------------------------------------------
plot(two_suns.spct, label.qty = "mean") + facet_wrap(~spct.idx)

## ------------------------------------------------------------------------
plot(two_suns.spct, annotations = c("-", "summaries")) + 
  aes(linetype = spct.idx)

## ------------------------------------------------------------------------
plot(rbindspct(list(sun = sun.spct, filtered = yellow_gel.spct * sun.spct)),
     annotations = c("-", "summaries")) + 
  aes(linetype = spct.idx)

## ------------------------------------------------------------------------
plot(yellow_gel.spct, annotations = c("-", "peaks"))

## ------------------------------------------------------------------------
plot(yellow_gel.spct, pc.out = TRUE, annotations = c("-", "peaks"))

## ------------------------------------------------------------------------
plot(yellow_gel.spct, plot.qty = "absorbance", annotations = c("-", "peaks"))

## ------------------------------------------------------------------------
yellow_gel.spct$Rfr <- 1 - max(yellow_gel.spct$Tfr)
plot(yellow_gel.spct, plot.qty = "absorptance", annotations = c("-", "peaks"))

## ------------------------------------------------------------------------
plot(sun.spct) + geom_spct(fill = color(sun.spct)) + 
  geom_spct(data = yellow_gel.spct * sun.spct, color = "black", 
            fill = color(yellow_gel.spct * sun.spct))

## ------------------------------------------------------------------------
two_suns.mspct <- source_mspct(list(sun1 = sun.spct, sun2 = sun.spct * 2))

## ---- fig.width = 7, fig.height = 8--------------------------------------
multiplot(plotlist = mslply(two_suns.mspct, plot))

## ---- fig.width = 7, fig.height = 8--------------------------------------
plot(rbindspct(two_suns.mspct)) + facet_wrap(~spct.idx, ncol = 1)

## ------------------------------------------------------------------------
plot(rbindspct(two_suns.mspct), 
     annotations = c("-", "summaries")) + 
  aes(linetype = spct.idx)

## ------------------------------------------------------------------------
plot(rbindspct(two_suns.mspct), 
     annotations = c("-", "summaries"), span = 301) + 
  aes(color = ifelse(spct.idx == "sun1", "darkgreen", "darkred"))

## ------------------------------------------------------------------------
plot(rbindspct(two_suns.mspct), annotations = "") + 
  aes(linetype = spct.idx) +
  wl_guide(ymax = -0.05)

## ------------------------------------------------------------------------
plot(sun.spct, annotations = c("-", "peaks")) +
  stat_peaks(span = NULL, color = "red") +
  stat_peaks(span = NULL, geom = "text", 
             label.fmt = "max at %3.1f nm",
             vjust = -0.4, color = "red")

## ------------------------------------------------------------------------
ggplot(rbindspct(two_suns.mspct)) + 
  aes(linetype = spct.idx) +
  wl_guide(ymax = -0.05) +
  geom_line()

## ------------------------------------------------------------------------
plot(VIS())

## ------------------------------------------------------------------------
plot(CIE(), range = CIE(), annotations = c("-", "color.guide"))

## ------------------------------------------------------------------------
plot(DNA_N(), range = c(270, 420), annotations = c("-", "color.guide"))

