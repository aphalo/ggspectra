## ------------------------------------------------------------------------
library(ggplot2)
library(photobiology)
library(photobiologyFilters)
library(photobiologyReflectors)
library(photobiologyLEDs)
library(photobiologyPlants)
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
plot(LUMITRONIX_white.spct)

## ------------------------------------------------------------------------
plot(rosco.mspct$Moss_Green_EColour89)

## ------------------------------------------------------------------------
plot(rosco.mspct$Moss_Green_EColour89, plot.qty = "absorbance")

## ------------------------------------------------------------------------
plot(rosco.mspct$Moss_Green_EColour89, plot.qty = "transmittance", pc.out = TRUE)

## ------------------------------------------------------------------------
plot(gold.spct)

## ------------------------------------------------------------------------
plot(Solidago_upper_adax.spct, w.band = Plant_bands())

## ------------------------------------------------------------------------
plot(as.filter_spct(Solidago_upper_adax.spct), w.band = Plant_bands())

## ------------------------------------------------------------------------
plot(as.reflector_spct(Solidago_upper_adax.spct), w.band = Plant_bands())

## ------------------------------------------------------------------------
plot(sun.daily.spct * CIE(), w.band = Plant_bands())

## ------------------------------------------------------------------------
plot(McCree_Oat.spct, w.band = PAR())

## ------------------------------------------------------------------------
plot(McCree_Oat.spct, w.band = PAR(), unit.out = "photon", label.qty = "mean")

## ------------------------------------------------------------------------
my.spct <- cps_spct(w.length = 250:800, cps = 1e3)
plot(my.spct)

## ---- eval=FALSE---------------------------------------------------------
#  my.spct <- raw_spct(w.length = 250:800, counts = 32000)
#  plot(my.spct)
#  

