## -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(rlang)
library(photobiology)
library(photobiologyWavebands)
library(ggspectra)

## ---- include=FALSE, echo=FALSE-----------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4,
               cache = FALSE)
options(warnPartialMatchArgs = FALSE)

## -----------------------------------------------------------------------------
two_suns.mspct <- source_mspct(list(sun1 = sun.spct, sun2 = sun.spct / 2))

## -----------------------------------------------------------------------------
two_suns.spct <- rbindspct(two_suns.mspct)

## -----------------------------------------------------------------------------
theme_set(theme_bw())

## ---- eval=FALSE--------------------------------------------------------------
#  ggplot() +
#    geom_line(data = sun.spct, mapping = aes(w.length, s.e.irrad)) +
#    geom_line(data = sun.spct %>% smooth_spct(method = "supsmu"),
#              mapping = aes(w.length, s.e.irrad),
#              colour = "red", size = 1.2)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() + 
  geom_line(data = . %>% smooth_spct(method = "supsmu"), 
            colour = "red", size = 1.2)

## -----------------------------------------------------------------------------
photon_as_default()
ggplot(sun.spct) + 
  geom_line() + 
  geom_line(data = . %>% smooth_spct(method = "supsmu"), 
            colour = "red", size = 1.2)
unset_radiation_unit_default()

## ---- eval=FALSE--------------------------------------------------------------
#  ggplot(sun.spct) +
#    geom_line(data = . %>% smooth_spct(method = "supsmu"),
#              colour = "red", size = 1.2)

## ---- eval=FALSE--------------------------------------------------------------
#  sun.spct %>%
#    smooth_spct(method = "supsmu") %>%
#    ggplot() +
#    geom_line(colour = "red", size = 1.2)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() + 
  geom_line(data = . %>% smooth_spct(method = "custom"), 
            colour = "blue", size = 1) +
  geom_line(data = . %>% smooth_spct(method = "lowess"), 
            colour = "green", size = 1) +
  geom_line(data = . %>% smooth_spct(method = "supsmu"), 
            colour = "red", size = 1)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() + 
  stat_peaks(size = 3, span = NULL) +
  stat_peaks(geom = "vline", linetype = "dotted", span = NULL) +
  geom_line(data = . %>% smooth_spct(method = "supsmu"), 
            colour = "red", size = 1.2) +
  stat_peaks(data = . %>% smooth_spct(method = "supsmu"),
             size = 3, span = NULL) +
  stat_peaks(data = . %>% smooth_spct(method = "supsmu"),
             geom = "vline", linetype = "dotted", span = NULL)

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() + 
  geom_line(data = . %>% trim_wl(range = PAR()), colour = "red")

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_line() + 
  geom_point(data = . %>% trim_wl(range = VIS()) %>% tag(),
            mapping = aes(color = wl.color),
            shape = "circle", size = 1.3) +
  scale_color_identity()

## -----------------------------------------------------------------------------
ggplot(sun.spct) + 
  geom_area(data = . %>% trim_wl(range = VIS()) %>% tag(w.band = VIS_bands()),
            mapping = aes(fill = wb.color)) +
  geom_line() + 
  scale_fill_identity()

