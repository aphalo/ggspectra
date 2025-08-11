context("autoplot-waveband")
library(ggplot2)
library(photobiology)

test_that("waveband", {
  set_annotations_default()
  vdiffr::expect_doppelganger("wb-default-cie",
                              autoplot(CIE(), range = c(230, 430)))
  vdiffr::expect_doppelganger("wb-default-cie-wl-range-shrink",
                              autoplot(CIE(),
                                       w.length = 200:400,
                                       range = c(230, 380)))
  vdiffr::expect_doppelganger("wb-cie-range-shrink-long",
                              autoplot(CIE(),
                                       w.length = 200:400,
                                       range = c(230, 300, 380)))
  # vdiffr::expect_doppelganger("wb-default-cie-wl-range-expand",
  #                             autoplot(CIE(),
  #                                      w.length = 250:350,
  #                                      range = c(200, 400)))
  # vdiffr::expect_doppelganger("wb-default-cie-range-r",
  #                             autoplot(CIE(), range = c(NA, 340)))
  # vdiffr::expect_doppelganger("wb-default-cie",
  #                             autoplot(CIE(), range = c(250, NA)))
  vdiffr::expect_doppelganger("wb-default-red",
                              autoplot(Red()))
  vdiffr::expect_doppelganger("wb-default-ylim",
                              autoplot(Red(), ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("wb-text-size",
                              autoplot(Red(), text.size = 3.5))
  vdiffr::expect_doppelganger("wb-plus-segments",
                              autoplot(Red(), annotations = c("+", "segments")))
  vdiffr::expect_doppelganger("wb-minus-boxes",
                              autoplot(Red(), annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("wb-no-annotations",
                              autoplot(Red(), annotations = ""))
  vdiffr::expect_doppelganger("wb-reserve-space",
                              autoplot(Red(), annotations = "reserve.space"))
})
