context("stats-label-peaks")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 100L
sun.spct <- interpolate_spct(photobiology::sun.spct, length.out = length.out.spct)
ccd.spct <- interpolate_spct(photobiology::ccd.spct, length.out = length.out.spct)

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

