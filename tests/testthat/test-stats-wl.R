context("stats-wl")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 200L
white_led.raw_spct <- interpolate_spct(photobiology::white_led.raw_spct, length.out = length.out.spct)
white_led.cps_spct <- interpolate_spct(photobiology::white_led.cps_spct, length.out = length.out.spct)
white_led.source_spct <- interpolate_spct(photobiology::white_led.source_spct, length.out = length.out.spct)

test_that("stat_wl_strip", {
  vdiffr::expect_doppelganger("stat-wl-strip-source",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_wl_strip(ymin = 0.9, ymax = 1.1) +
                                scale_fill_identity() +
                                expand_limits(y = 1.2))

  vdiffr::expect_doppelganger("stat-wl-strip-cps",
                              ggplot(white_led.cps_spct)+
                                geom_line() +
                                stat_wl_strip(ymin = 2.7e5, ymax = 2.9e5) +
                                scale_fill_identity() +
                                expand_limits(y = 3e5))

  vdiffr::expect_doppelganger("stat-wl-strip-raw",
                              ggplot(white_led.raw_spct)+
                                geom_line() +
                                stat_wl_strip(ymin = 62e3, ymax = 66e3) +
                                scale_fill_identity() +
                                expand_limits(y = 70e3))

  })
