context("stats-peaks")

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 300L
sun.spct <- interpolate_spct(photobiology::sun.spct, length.out = length.out.spct)

test_that("stat_peaks", {
  vdiffr::expect_doppelganger("stat-peaks",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks())

  vdiffr::expect_doppelganger("stat-peaks-global-thr",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks(global.threshold = 0.5))

  vdiffr::expect_doppelganger("stat-peaks-global-thr-neg",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks(global.threshold = -0.5))

  vdiffr::expect_doppelganger("stat-peaks-global-thr-I",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks(global.threshold = I(0.2)))

  vdiffr::expect_doppelganger("stat-peaks-local-thr",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks(local.threshold = 0.05)
                              )

  vdiffr::expect_doppelganger("stat-peaks-local-thr-far",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_peaks(local.threshold = 0.1,
                                           local.reference = "farthest")
  )

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

  vdiffr::expect_doppelganger("stat-valleys-global-thr",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys(global.threshold = 0.5)
                              )

  vdiffr::expect_doppelganger("stat-valleys-global-thr-neg",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys(global.threshold = -0.5)
                              )

  vdiffr::expect_doppelganger("stat-valleys-global-thr-I",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys(global.threshold = I(0.3))
                              )

  vdiffr::expect_doppelganger("stat-valleys-local-thr",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys(local.threshold = 0.05)
                              )

  vdiffr::expect_doppelganger("stat-valleys-local-thr-far",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys(local.threshold = 0.1,
                                             local.reference = "farthest")
  )

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

  vdiffr::expect_doppelganger("stat-valleys-span-null-strict",
                              ggplot(sun.spct)+
                                geom_line() +
                                stat_valleys(span = NULL,
                                             strict = TRUE))

})

