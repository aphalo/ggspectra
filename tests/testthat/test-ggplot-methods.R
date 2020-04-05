context("ggplot")
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

test_that("raw_spct", {
  vdiffr::expect_doppelganger("raw-ggplot",
                              ggplot(white_led.raw_spct) + geom_line())
})

test_that("raw_spct", {
  vdiffr::expect_doppelganger("cps-ggplot",
                              ggplot(white_led.cps_spct) + geom_line())
})

test_that("source_spct", {
  vdiffr::expect_doppelganger("source-ggplot",
                              ggplot(white_led.source_spct) + geom_line())
  vdiffr::expect_doppelganger("source-ggplot-p",
                              ggplot(white_led.source_spct, unit.out = "photon") +
                                geom_line())
  expect_error(ggplot(white_led.source_spct, unit.out = "zzz") + geom_line())
})

test_that("source_mspct", {
  two_leds.mspct <- source_mspct(list(one = white_led.source_spct,
                                      half = white_led.source_spct / 2))
  vdiffr::expect_doppelganger("source-mspct-ggplot",
                              ggplot(two_leds.mspct) +
                                aes(linetype = spct.idx) +
                                geom_line())
  vdiffr::expect_doppelganger("source-mspct-ggplot-p",
                              ggplot(two_leds.mspct, unit.out = "photon") +
                                aes(linetype = spct.idx) +
                                geom_line())
  expect_error(ggplot(two_leds.mspct, unit.out = "zzz") +
                 aes(linetype = spct.idx) +
                 geom_line())
})

test_that("response_spct", {
  vdiffr::expect_doppelganger("response-ggplot",
                              ggplot(ccd.spct) + geom_line())
  vdiffr::expect_doppelganger("response-ggplot-e",
                              ggplot(ccd.spct, unit.out = "energy") +
                                geom_line())
  vdiffr::expect_doppelganger("response-ggplot-p",
                              ggplot(ccd.spct, unit.out = "photon") +
                                geom_line())
  expect_error(ggplot(ccd.spct, unit.out = "zzz") + geom_line())
})

test_that("response_mspct", {
  two_ccd.mspct <- response_mspct(list(one = ccd.spct,
                                        half = ccd.spct / 2))
  vdiffr::expect_doppelganger("response-mspect-ggplot",
                              ggplot(two_ccd.mspct) +
                                aes(linetype = spct.idx) +
                                geom_line())
  vdiffr::expect_doppelganger("response-mspect-ggplot-e",
                              ggplot(two_ccd.mspct, unit.out = "energy") +
                                aes(linetype = spct.idx) +
                                geom_line())
  vdiffr::expect_doppelganger("response-mspect-ggplot-p",
                              ggplot(two_ccd.mspct, unit.out = "photon") +
                                aes(linetype = spct.idx) +
                                geom_line())
  expect_error(ggplot(two_ccd.mspct, unit.out = "zzz") +
                 aes(linetype = spct.idx) +
                 geom_line())
})

test_that("filter_spct", {
  vdiffr::expect_doppelganger("filter-ggplot",
                              ggplot(polyester.spct) + geom_line())
  vdiffr::expect_doppelganger("filter-ggplot-A",
                              ggplot(polyester.spct,
                                     plot.qty = "absorbance") + geom_line())
  vdiffr::expect_doppelganger("filter-ggplot-Afr",
                              ggplot(polyester.spct,
                                     plot.qty = "absorptance") + geom_line())
  vdiffr::expect_doppelganger("filter-ggplot-Tfr",
                              ggplot(polyester.spct,
                                     plot.qty = "transmittance") + geom_line())
  expect_error(ggplot(polyester.spct, plot.qty = "zzz") + geom_line())
})

test_that("filter_mspct", {
  two_polyester.mspct <- filter_mspct(list(one = polyester.spct,
                                           half = polyester.spct / 2))
  vdiffr::expect_doppelganger("filter-mspct-ggplot",
                              ggplot(two_polyester.mspct) +
                                aes(linetype = spct.idx) +
                                geom_line())
  vdiffr::expect_doppelganger("filter-mspct-ggplot-A",
                              ggplot(two_polyester.mspct,
                                     plot.qty = "absorbance") +
                                aes(linetype = spct.idx) +
                                geom_line())
  vdiffr::expect_doppelganger("filter-mspct-ggplot-Afr",
                              ggplot(two_polyester.mspct,
                                     plot.qty = "absorptance") +
                                aes(linetype = spct.idx) +
                                geom_line())
  vdiffr::expect_doppelganger("filter-mspct-ggplot-Tfr",
                              ggplot(two_polyester.mspct,
                                     plot.qty = "transmittance") +
                                aes(linetype = spct.idx) +
                                geom_line())
  expect_error(ggplot(two_polyester.mspct, plot.qty = "zzz") +
                 aes(linetype = spct.idx) +
                 geom_line())
})

test_that("object_spct", {
  vdiffr::expect_doppelganger("object-ggplot",
                              ggplot(Ler_leaf.spct) +
                                aes(linetype = variable) + geom_line())
  vdiffr::expect_doppelganger("object-ggplot-all",
                              ggplot(Ler_leaf.spct, plot.qty = "all") +
                                aes(linetype = variable) + geom_line())
  vdiffr::expect_doppelganger("object-ggplot-Tfr",
                              ggplot(Ler_leaf.spct, plot.qty = "transmittance") +
                                geom_line())
  vdiffr::expect_doppelganger("object-ggplot-Rfr",
                              ggplot(Ler_leaf.spct, plot.qty = "reflectance") +
                                geom_line())
  expect_error(ggplot(Ler_leaf.spct, plot.qty = "zzz") +
                 geom_line())
})
