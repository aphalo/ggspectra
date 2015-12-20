## ------------------------------------------------------------------------
library(ggplot2)
library(photobiology)
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
ggplot(sun.spct, aes(w.length, s.e.irrad)) + geom_line() + 
  stat_peaks()

