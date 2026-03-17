## -----------------------------------------------------------------------------
library(ggplot2)
library(photobiologyWavebands)
library(ggspectra)
library(ggrepel)

# ensure all labels are plotted
options(ggrepel.max.overlaps = Inf)

## ----include=FALSE, echo=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', fig.show = 'hold',
               fig.width = 7, fig.height = 4, cache = FALSE)
options(warnPartialMatchArgs = FALSE)

## -----------------------------------------------------------------------------
summary(sun.spct)

## -----------------------------------------------------------------------------
summary(sun_evening.spct)

## -----------------------------------------------------------------------------
summary(sun_evening.mspct)

## -----------------------------------------------------------------------------
theme_set(theme_bw(10))

## -----------------------------------------------------------------------------
# the classes
spct_classes()

## -----------------------------------------------------------------------------
# autoplot methods for spectra
grep("_spct*$", methods("autoplot"), value = TRUE)

## -----------------------------------------------------------------------------
autoplot(sun.spct)

## -----------------------------------------------------------------------------
autoplot(sun.spct, unit.out = "photon")

## -----------------------------------------------------------------------------
autoplot(sun.spct, geom = "spct")

## -----------------------------------------------------------------------------
autoplot(sun.spct, geom = c("spct", "line"))

## -----------------------------------------------------------------------------
autoplot(sun_evening.spct)

## -----------------------------------------------------------------------------
autoplot(sun_evening.spct, idfactor = "Time")

## -----------------------------------------------------------------------------
# the classes
mspct_classes()

## -----------------------------------------------------------------------------
# autoplot methods for collections of spectra
grep("_mspct*$", methods("autoplot"), value = TRUE)

## -----------------------------------------------------------------------------
autoplot(sun_evening.mspct)

## -----------------------------------------------------------------------------
autoplot(sun_evening.mspct, idfactor = "Time")

## -----------------------------------------------------------------------------
autoplot(sun_evening.spct, facets = TRUE)

## -----------------------------------------------------------------------------
autoplot(sun_evening.mspct, facets = 2)

## -----------------------------------------------------------------------------
autoplot(sun_evening.mspct, geom = "spct", facets = 2)

## -----------------------------------------------------------------------------
p1 <- autoplot(sun.spct)
summary(p1$data)

## -----------------------------------------------------------------------------
autoplot(sun.spct, range = c(400, 700))

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = photobiologyWavebands::VIS_bands("ISO"))

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = waveband(c(380, 760)))

## -----------------------------------------------------------------------------
autoplot(sun.spct, w.band = NULL)

## ----eval=FALSE---------------------------------------------------------------
# set_w.band_default(w.band = Plant_bands())

## -----------------------------------------------------------------------------
autoplot(sun.spct, geom = "spct", norm = "max")

## -----------------------------------------------------------------------------
autoplot(sun_evening.mspct, plot.data = "mean")

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

## -----------------------------------------------------------------------------
autoplot(sun.spct, 
         annotations = list(c("+", "peak.labels"), 
                            c("-", "boxes", "summaries", "labels")), 
         span = 21)

## -----------------------------------------------------------------------------
autoplot(sun.spct, 
         annotations = list(c("+", "valley.labels"), 
                            c("-", "peaks")), 
         span = 31)

## -----------------------------------------------------------------------------
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
autoplot(sun_evening.spct, facets = 2) +
  geom_vline(xintercept = c(400, 700), linetype = "dashed")

## -----------------------------------------------------------------------------
filter_no_yes.spct <- 
  rbindspct(list(sun = sun.spct, filtered = yellow_gel.spct * sun.spct), 
            idfactor = "Light source")
autoplot(filter_no_yes.spct)

## -----------------------------------------------------------------------------
autoplot(yellow_gel.spct, 
         annotations = list(c("-", "peaks"), c("+", "wls")))

## -----------------------------------------------------------------------------
autoplot(yellow_gel.spct, plot.qty = "absorbance", wls.target = 2,
         annotations = list(c("-", "peaks"), c("+", "wls")))

## ----eval=FALSE, echo=FALSE---------------------------------------------------
# yellow_gel.spct$Rfr <- 1 - max(yellow_gel.spct$Tfr)
# autoplot(yellow_gel.spct, plot.qty = "absorptance", annotations = c("-", "peaks"))

## -----------------------------------------------------------------------------
autoplot(sun.spct, 
         w.band = UV_bands("CIE"), 
         geom = c("spct", "line"), 
         range = c(280, 400),
         unit.out = "photon") +
  geom_vline(xintercept = c(280, 315, 340, 400), linetype = "dotted")

## -----------------------------------------------------------------------------
autoplot(yellow_gel.spct, annotations = c("+", "boundaries"))

## -----------------------------------------------------------------------------
autoplot(white_led.raw_spct, 
         annotations = list(c("+", "boundaries"), c("-", "peaks")))

## ----eval=FALSE, message = FALSE----------------------------------------------
# autoplot(white_led.raw_spct[ , c("w.length", "counts_1")],
#          annotations = c("+", "boundaries"))

## ----eval=FALSE---------------------------------------------------------------
# autoplot(white_led.raw_spct[ , c("w.length", "counts_1", "counts_3")],
#      annotations = list(c("+", "boundaries"), c("-", "peaks")))

## ----eval=FALSE---------------------------------------------------------------
# # Not run so as to pass CRAN checks!!
# autoplot(yellow_gel.spct - 0.01, annotations = c("+", "boundaries"))

## -----------------------------------------------------------------------------
autoplot(sun_evening.mspct, annotations = c("-", "peaks")) +
  aes(color = ifelse(spct.idx == "time.05", "black", "darkred")) +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
autoplot(sun_evening.mspct, annotations = c("-", "peaks"), facets = TRUE) +
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
autoplot(CIE(), geom = "spct", annotations = c("-", "color.guide"))

## -----------------------------------------------------------------------------
autoplot(DNA_N(),  geom = "spct", annotations = c("-", "color.guide"))

