context("stats-peaks")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 300L
sun.spct <- interpolate_spct(photobiology::sun.spct, length.out = length.out.spct)

test_that("stat_peaks", {
  vdiffr::expect_doppelganger("stat-peaks-sun",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks())

  vdiffr::expect_doppelganger("stat-peaks",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks())

  vdiffr::expect_doppelganger("stat-peaks-span-11",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks(span = 11))

  vdiffr::expect_doppelganger("stat-peaks-span-101",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks(span = 101))

  vdiffr::expect_doppelganger("stat-peaks-span-1001",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks(span = 1001))

  vdiffr::expect_doppelganger("stat-peaks-span-null",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks(span = NULL))

})

test_that("stat_valleys", {
  vdiffr::expect_doppelganger("stat-valleys",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys())

  vdiffr::expect_doppelganger("stat-valleys-span-11",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys(span = 11))

  vdiffr::expect_doppelganger("stat-valleys-span-101",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys(span = 101))

  vdiffr::expect_doppelganger("stat-valleys-span-1001",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys(span = 1001))

  vdiffr::expect_doppelganger("stat-valleys-span-null",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys(span = NULL))

})

