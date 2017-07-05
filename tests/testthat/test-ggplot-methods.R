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
})

test_that("response_spct", {
  vdiffr::expect_doppelganger("response-ggplot",
                              ggplot(ccd.spct) + geom_line())
  vdiffr::expect_doppelganger("response-ggplot-p",
                              ggplot(ccd.spct, unit.out = "photon") +
                                geom_line())
})

test_that("filter_spct", {
  vdiffr::expect_doppelganger("filter-ggplot",
                              ggplot(polyester.spct) + geom_line())
})

# Not implemented yet
# test_that("object_spct", {
#   vdiffr::expect_doppelganger("object-default",
#                               ggplot(Ler_leaf.spct) + geom_line())
# })
