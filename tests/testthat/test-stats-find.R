context("stats-find")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 100L
sun.spct <- interpolate_spct(photobiology::sun.spct, length.out = length.out.spct)
white_led.raw_spct <- interpolate_spct(photobiology::white_led.raw_spct, length.out = length.out.spct)
white_led.cps_spct <- interpolate_spct(photobiology::white_led.cps_spct, length.out = length.out.spct)
white_led.source_spct <- interpolate_spct(photobiology::white_led.source_spct, length.out = length.out.spct)
Ler_leaf_trns.spct <- interpolate_spct(photobiology::Ler_leaf_trns.spct, length.out = length.out.spct)
Ler_leaf_trns.spct <- clean(Ler_leaf_trns.spct, range.s.data = c(1e-5, 1 - 1e-5))
Ler_leaf_trns_i.spct <- interpolate_spct(photobiology::Ler_leaf_trns_i.spct, length.out = length.out.spct)
Ler_leaf_trns_i.spct <- clean(Ler_leaf_trns_i.spct, range.s.data = c(1e-5, 1 - 1e-5))
Ler_leaf_rflt.spct <- interpolate_spct(photobiology::Ler_leaf_rflt.spct, length.out = length.out.spct)
Ler_leaf_rflt.spct <- clean(Ler_leaf_rflt.spct, range.s.data = c(1e-5, 1 - 1e-5))
Ler_leaf.spct <- clean(Ler_leaf.spct, min.Afr = 1e-5, range.s.data = c(1e-5, 1 - 1e-5))
Ler_leaf.spct <- interpolate_spct(photobiology::Ler_leaf.spct, length.out = length.out.spct)
Ler_leaf.spct <- clean(Ler_leaf.spct, min.Afr = 1e-5, range.s.data = c(1e-5, 1 - 1e-5))
ccd.spct <- interpolate_spct(photobiology::ccd.spct, length.out = length.out.spct)

test_that("stat_find_wls", {
  vdiffr::expect_doppelganger("stat-find-wls",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls())

  vdiffr::expect_doppelganger("stat-find-wls-HM",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = "half.maximum"))

  vdiffr::expect_doppelganger("stat-find-wls-HR",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = "half.range"))

  vdiffr::expect_doppelganger("stat-find-wls-zero",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = 0))

  vdiffr::expect_doppelganger("stat-find-wls-100",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = 100))

  vdiffr::expect_doppelganger("stat-find-wls-vector",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = (1:5) / 10))

  vdiffr::expect_doppelganger("stat-find-wls-vector-long",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_wls(target = (-1:9) / 10))

  vdiffr::expect_doppelganger("stat-find-qtys",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_qtys())

  vdiffr::expect_doppelganger("stat-find-qtys-HR",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_qtys(target = "half.range"))

  vdiffr::expect_doppelganger("stat-find-qtys-vector",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_qtys(target = (3:8) * 100))

  vdiffr::expect_doppelganger("stat-find-qtys-vector-long",
                              ggplot(white_led.source_spct)+
                                geom_line() +
                                stat_find_qtys(target = (1:10) * 100))

})


test_that("stat_find_wls-group", {

  white_led.source_mspct <-
    source_mspct(list(one = white_led.source_spct,
                 half = white_led.source_spct / 2))

  vdiffr::expect_doppelganger("stat-find-wls-group",
                              ggplot(white_led.source_mspct) +
                                aes(linetype = spct.idx) +
                                geom_line() +
                                stat_find_wls(aes(colour = spct.idx)))

  vdiffr::expect_doppelganger("stat-find-wls-vector-group",
                              ggplot(white_led.source_mspct)+
                                aes(linetype = spct.idx) +
                                geom_line() +
                                stat_find_wls(target = (1:5) / 10,
                                              aes(colour = spct.idx)))

  vdiffr::expect_doppelganger("stat-find-qtys-vector-group",
                              ggplot(white_led.source_mspct)+
                                aes(linetype = spct.idx) +
                                geom_line() +
                                stat_find_qtys(target = (3:8) * 100,
                                               aes(colour = spct.idx)))

  })

test_that("stat_find_wls-panel", {

  white_led.source_mspct <-
    source_mspct(list(one = white_led.source_spct,
                      half = white_led.source_spct / 2))

  vdiffr::expect_doppelganger("stat-find-wls-panel",
                              ggplot(white_led.source_mspct) +
                                facet_wrap(~spct.idx) +
                                geom_line() +
                                stat_find_wls(aes(colour = spct.idx)))

  vdiffr::expect_doppelganger("stat-find-wls-vector-panel",
                              ggplot(white_led.source_mspct)+
                                facet_wrap(~spct.idx) +
                                aes(linetype = spct.idx) +
                                geom_line() +
                                stat_find_wls(target = (1:5) / 10,
                                              aes(colour = spct.idx)))

  vdiffr::expect_doppelganger("stat-find-qtys-vector-panel",
                              ggplot(white_led.source_mspct)+
                                facet_wrap(~spct.idx) +
                                aes(linetype = spct.idx) +
                                geom_line() +
                                stat_find_qtys(target = (3:8) * 100,
                                               aes(colour = spct.idx)))
})

