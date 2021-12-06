## -----------------------------------------------------------------------------
library(dplyr)
library(photobiologyWavebands)
library(ggspectra)
library(ggrepel)

good_label_repel <- packageVersion('ggrepel') != "0.9.0" # >= 0.9.0 required
options(ggrepel.max.overlaps = Inf) # needed for 'ggrepel' (>= 0.9.0)

## ---- include=FALSE, echo=FALSE-----------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', fig.show = 'hold',
               fig.width = 7, fig.height = 4, cache = FALSE)
options(warnPartialMatchArgs = FALSE)

## -----------------------------------------------------------------------------
two_suns.spct <- rbindspct(list(sun1 = sun.spct, sun2 = sun.spct * 2))

## -----------------------------------------------------------------------------
theme_set(theme_bw(10))

## -----------------------------------------------------------------------------
autoplot(sun.spct)

## -----------------------------------------------------------------------------
autoplot(sun.spct, unit.out = "photon")

## -----------------------------------------------------------------------------
autoplot(two_suns.spct)

## -----------------------------------------------------------------------------
autoplot(sun.spct, label.qty = "mean")

## -----------------------------------------------------------------------------
autoplot(sun.spct, label.qty = "contribution")

## -----------------------------------------------------------------------------
autoplot(sun.spct, label.qty = "relative")

## -----------------------------------------------------------------------------
autoplot(sun.spct, 
     annotations = c("+", "title:when:where"))

## -----------------------------------------------------------------------------
autoplot(sun.spct, 
     annotations = c("+", "title:none:what:where"))

## -----------------------------------------------------------------------------
autoplot(sun.spct, 
     annotations = c("+", "boundaries"))

## -----------------------------------------------------------------------------
autoplot(sun.spct, 
     annotations = c("=", "labels", "summaries", "color.guide", "peaks", "boundaries"))

## -----------------------------------------------------------------------------
autoplot(sun.spct, 
     annotations = c("+", "segments"))

## -----------------------------------------------------------------------------
autoplot(sun.spct, annotations = c("-", "summaries", "peaks"))

## -----------------------------------------------------------------------------
autoplot(sun.spct, annotations = c("+", "valleys"), span = 41)

## ---- eval=good_label_repel---------------------------------------------------
autoplot(sun.spct, 
         annotations = list(c("+", "peak.labels"), 
                            c("-", "boxes", "summaries", "labels")), 
         span = 51)

## ---- eval=good_label_repel---------------------------------------------------
autoplot(sun.spct, 
         annotations = list(c("+", "valley.labels"), 
                            c("-", "peaks")), 
         span = 51)

## ---- eval=good_label_repel---------------------------------------------------
autoplot(sun.spct, annotations = c("+", "peak.labels", "valley.labels"), span = 51)

## -----------------------------------------------------------------------------
autoplot(sun.spct, annotations = "")

## -----------------------------------------------------------------------------
autoplot(sun.spct, annotations = "reserve.space")

## -----------------------------------------------------------------------------
autoplot(sun.spct, annotations = c("=", "segments", "labels", "color.guide"), 
     text.size = 3.5)

## -----------------------------------------------------------------------------
autoplot(sun.spct, ylim = c(NA, 1))

## -----------------------------------------------------------------------------
autoplot(sun.spct, range = VIS())

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = PAR())

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = CIE())

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = NULL)

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = NULL, range = c(400,700))

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = NULL, range = PAR())

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = PAR(), range = PAR())

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = VIS_bands(), range = VIS())

## -----------------------------------------------------------------------------
getTimeUnit(sun.daily.spct)
autoplot(sun.daily.spct)

## -----------------------------------------------------------------------------
autoplot(two_suns.spct, idfactor = NA) + facet_wrap(~spct.idx)

## -----------------------------------------------------------------------------
filter_no_yes.spct <- 
  rbindspct(list(sun = sun.spct, filtered = yellow_gel.spct * sun.spct), 
            idfactor = "Source")
autoplot(filter_no_yes.spct)

## -----------------------------------------------------------------------------
autoplot(yellow_gel.spct, 
         annotations = list(c("-", "peaks"), c("+", "wls")))

## -----------------------------------------------------------------------------
autoplot(yellow_gel.spct, pc.out = TRUE, 
         annotations = list(c("-", "peaks"), c("+", "wls")))

## -----------------------------------------------------------------------------
autoplot(yellow_gel.spct, plot.qty = "absorbance", wls.target = 2,
         annotations = list(c("-", "peaks"), c("+", "wls")))

## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  yellow_gel.spct$Rfr <- 1 - max(yellow_gel.spct$Tfr)
#  autoplot(yellow_gel.spct, plot.qty = "absorptance", annotations = c("-", "peaks"))

## -----------------------------------------------------------------------------
autoplot(sun.spct) + geom_spct(fill = color_of(sun.spct)) + 
  geom_spct(data = yellow_gel.spct * sun.spct, color = "black", 
            fill = color_of(yellow_gel.spct * sun.spct))

## -----------------------------------------------------------------------------
autoplot(yellow_gel.spct, annotations = c("+", "boundaries"))

## -----------------------------------------------------------------------------
autoplot(white_led.raw_spct, annotations = c("+", "boundaries"))

## ---- eval=FALSE--------------------------------------------------------------
#  autoplot(dplyr::select(white_led.raw_spct, w.length, counts = counts_1),
#       annotations = c("+", "boundaries"))

## ---- eval=FALSE--------------------------------------------------------------
#  autoplot(dplyr::select(white_led.raw_spct, w.length, counts_1, counts_3),
#       annotations = c("+", "boundaries"))

## -----------------------------------------------------------------------------
autoplot(sun.spct, 
         chroma.type = beesxyzCMF.spct,
          annotations = c("=", "color.guide"))

## ---- eval=FALSE--------------------------------------------------------------
#  # Not run so as to pass CRAN checks!!
#  autoplot(yellow_gel.spct - 0.01, annotations = c("+", "boundaries"))

## -----------------------------------------------------------------------------
two_suns.mspct <- source_mspct(list(sun1 = sun.spct, sun2 = sun.spct / 2))
mixed.mspct <- generic_mspct(list(sun = sun.spct, filter = polyester.spct))

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct)

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, idfactor = "Spectra")

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, facets = 1)

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, facets = 2)

## ---- eval = FALSE------------------------------------------------------------
#  autoplot(two_suns.mspct, idfactor = NA) +
#    facet_wrap(~spct.idx, ncol = 2)

## ---- fig.width = 7, fig.height = 8-------------------------------------------
multiplot(plotlist = mslply(mixed.mspct, autoplot))

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct) + 
  aes(color = ifelse(spct.idx == "sun1", "darkgreen", "darkred"))

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, annotations = c("color.guide", "valleys", "peaks"), span = 51)

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, annotations = c("-", "peaks")) +
  stat_peaks(span = NULL, color = "red") +
  stat_peaks(span = NULL, geom = "text", 
             label.fmt = "max at %3.1f nm",
             vjust = -0.5, hjust = 0,
             color = "red", size = 3)

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, plot.data = "mean")

## -----------------------------------------------------------------------------
autoplot(VIS())

## -----------------------------------------------------------------------------
autoplot(CIE(), range = CIE(), annotations = c("-", "color.guide"))

## -----------------------------------------------------------------------------
autoplot(DNA_N(), range = c(270, 420), annotations = c("-", "color.guide"))

