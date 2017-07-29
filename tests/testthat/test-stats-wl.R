context("stats-wl")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

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
