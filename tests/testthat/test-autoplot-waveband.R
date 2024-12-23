context("autoplot-waveband")
library(ggplot2)
library(photobiology)

test_that("waveband", {
  set_annotations_default()
  vdiffr::expect_doppelganger("waveband-default-cie",
                              autoplot(CIE(), range = c(230, 430)))
  vdiffr::expect_doppelganger("waveband-default-red",
                              autoplot(Red()))
  vdiffr::expect_doppelganger("waveband-default-ylim",
                              autoplot(Red(), ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("waveband-text-size",
                              autoplot(Red(), text.size = 3.5))
  vdiffr::expect_doppelganger("waveband-plus-segments",
                              autoplot(Red(), annotations = c("+", "segments")))
  vdiffr::expect_doppelganger("waveband-minus-boxes",
                              autoplot(Red(), annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("waveband-no-annotations",
                              autoplot(Red(), annotations = ""))
  vdiffr::expect_doppelganger("waveband-reserve-space",
                              autoplot(Red(), annotations = "reserve.space"))
})
