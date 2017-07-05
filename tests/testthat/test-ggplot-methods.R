context("ggplot")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

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

test_that("filter_spct", {
  vdiffr::expect_doppelganger("filter-ggplot",
                              ggplot(polyester.spct) + geom_line())
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
