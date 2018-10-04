context("stats-label-peaks")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

test_that("stat_label_peaks", {
  vdiffr::expect_doppelganger("stat-label-peaks-sun",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_peaks())

  vdiffr::expect_doppelganger("stat-label-peaks",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_peaks())

  vdiffr::expect_doppelganger("stat-label-peaks-span-11",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_peaks(span = 11))

  vdiffr::expect_doppelganger("stat-label-peaks-span-101",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_peaks(span = 101))

  vdiffr::expect_doppelganger("stat-label-peaks-span-1001",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_peaks(span = 1001))

  vdiffr::expect_doppelganger("stat-label-peaks-span-null",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_peaks(span = NULL))

})

test_that("stat_label_valleys", {
  vdiffr::expect_doppelganger("stat-label-valleys",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_valleys())

  vdiffr::expect_doppelganger("stat-label-valleys-span-11",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_valleys(span = 11))

  vdiffr::expect_doppelganger("stat-label-valleys-span-101",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_valleys(span = 101))

  vdiffr::expect_doppelganger("stat-label-valleys-span-1001",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_valleys(span = 1001))

  vdiffr::expect_doppelganger("stat-label-valleys-span-null",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_label_valleys(span = NULL))

})

