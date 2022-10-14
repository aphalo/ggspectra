## -----------------------------------------------------------------------------
library(ggplot2)
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
two_suns.mspct <- source_mspct(list(sun1 = sun.spct, sun2 = sun.spct * 2))
two_suns.spct <- rbindspct(list(sun1 = sun.spct, sun2 = sun.spct * 2))

## -----------------------------------------------------------------------------
theme_set(theme_bw(10))

## -----------------------------------------------------------------------------
spct_classes()

## -----------------------------------------------------------------------------
autoplot(sun.spct)

## -----------------------------------------------------------------------------
autoplot(sun.spct, unit.out = "photon")

## -----------------------------------------------------------------------------
autoplot(sun.spct, geom = "spct")

## -----------------------------------------------------------------------------
autoplot(two_suns.spct)

## -----------------------------------------------------------------------------
mspct_classes()

## -----------------------------------------------------------------------------
two_suns.mspct <- source_mspct(list(sun1 = sun.spct, sun2 = sun.spct / 2))

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct)

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, idfactor = "Spectra")

## -----------------------------------------------------------------------------
autoplot(two_suns.spct, facets = TRUE)

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, facets = 1)

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, facets = 2)

## -----------------------------------------------------------------------------
autoplot(sun.spct, range = c(400, 700))

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = photobiologyWavebands::VIS_bands("ISO"))

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = waveband(c(380, 760)))

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = NULL)

## ---- eval=FALSE--------------------------------------------------------------
#  set_w.band_default(w.band = Plant_bands())

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, plot.data = "mean")

## -----------------------------------------------------------------------------
autoplot(sun.spct, label.qty = "mean")

## -----------------------------------------------------------------------------
autoplot(sun.spct, label.qty = "contribution.pc")

## -----------------------------------------------------------------------------
autoplot(sun.spct, label.qty = "relative.pc")

## -----------------------------------------------------------------------------
autoplot(sun.spct, 
     annotations = c("+", "title:objt:when:where"))

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
         span = 21)

## ---- eval=good_label_repel---------------------------------------------------
autoplot(sun.spct, 
         annotations = list(c("+", "valley.labels"), 
                            c("-", "peaks")), 
         span = 31)

## ---- eval=good_label_repel---------------------------------------------------
autoplot(sun.spct, annotations = c("+", "peak.labels", "valley.labels"), span = 31)

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
#  autoplot(white_led.raw_spct[ , c("w.length", "counts_1")],
#           annotations = c("+", "boundaries"))

## ---- eval=FALSE--------------------------------------------------------------
#  autoplot(white_led.raw_spct[ , c("w.length", "counts_1", "counts_3")],
#       annotations = c("+", "boundaries"))

## ---- eval=FALSE--------------------------------------------------------------
#  # Not run so as to pass CRAN checks!!
#  autoplot(yellow_gel.spct - 0.01, annotations = c("+", "boundaries"))

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, annotations = c("-", "peaks")) +
  aes(color = ifelse(spct.idx == "sun1", "darkgreen", "darkred"), linetype = "solid") +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
autoplot(two_suns.mspct, annotations = c("-", "peaks")) +
  stat_peaks(span = NULL, color = "red") +
  stat_peaks(span = NULL, geom = "text", 
             label.fmt = "max at %3.1f nm",
             vjust = -0.5, hjust = 0,
             color = "red", size = 3)

## -----------------------------------------------------------------------------
autoplot(VIS())

## -----------------------------------------------------------------------------
autoplot(PAR(), range = c(200, 1000), geom = "spct", 
         unit.in = "photon", unit.out = "energy")

## -----------------------------------------------------------------------------
autoplot(CIE(), range = CIE(), annotations = c("-", "color.guide"))

## -----------------------------------------------------------------------------
autoplot(DNA_N(), range = c(270, 420), annotations = c("-", "color.guide"))

