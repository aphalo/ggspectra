context("plot")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

test_that("raw_spct", {
  set_annotations_default()
    # skip_on_cran()
  vdiffr::expect_doppelganger("raw-default",
                      autoplot(white_led.raw_spct))
  vdiffr::expect_doppelganger("raw-ylim1",
                              autoplot(white_led.raw_spct, ylim = c(NA, 9e4)))
  vdiffr::expect_doppelganger("raw-ylim2",
                              autoplot(white_led.raw_spct, ylim = c(-5e3, 9e4)))
  vdiffr::expect_doppelganger("raw-range-num",
                              autoplot(white_led.raw_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("raw-range-wb",
                              autoplot(white_led.raw_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("raw-no-annotations",
                      autoplot(white_led.raw_spct, annotations = ""))
  vdiffr::expect_doppelganger("raw-reserve-space",
                              autoplot(white_led.raw_spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("raw-minus-annotations",
                      autoplot(white_led.raw_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("raw-plus-annotations",
                      autoplot(white_led.raw_spct, annotations = c("+", "boundaries")))
})

test_that("cps_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("cps-default",
                              autoplot(white_led.cps_spct))
  vdiffr::expect_doppelganger("cps-ylim1",
                              autoplot(white_led.cps_spct, ylim = c(NA, 5e5)))
  vdiffr::expect_doppelganger("cps-ylim2",
                              autoplot(white_led.cps_spct, ylim = c(-1e5, 5e5)))
  vdiffr::expect_doppelganger("cps-range-num",
                              autoplot(white_led.cps_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("cps-range-wb",
                              autoplot(white_led.cps_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("cps-no-annotations",
                              autoplot(white_led.cps_spct, annotations = ""))
  vdiffr::expect_doppelganger("cps-reserve-space",
                              autoplot(white_led.cps_spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("cps-minus-annotations",
                              autoplot(white_led.cps_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("cps-plus-annotations",
                              autoplot(white_led.cps_spct, annotations = c("+", "boundaries")))
})

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



test_that("set_annotations_default", {
  set_annotations_default()
  expect_null(getOption("photobiology.plot.annotations"))
  set_annotations_default("")
  expect_setequal(getOption("photobiology.plot.annotations"), "")
  set_annotations_default("reserve.space")
  expect_setequal(getOption("photobiology.plot.annotations"), "reserve.space")
  set_annotations_default()
  set_annotations_default(c("+", "title:what:where"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                 c("boxes", "labels", "summaries", "colour.guide", "peaks", "title:what:where"))
  set_annotations_default("+")
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks", "title:what:where"))
  set_annotations_default("-")
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks", "title:what:where"))
  set_annotations_default(c("-", "title"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks", "title:what:where"))
  set_annotations_default(c("-", "title*"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("-", "peaks*"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide"))
  set_annotations_default(c("+", "peaks"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("+", "peak.labels"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peak.labels"))
  set_annotations_default(c("+", "peaks"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("+", "peaks"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("+", "segments"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("segments", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("+", "boxes"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("boxes", "labels", "summaries", "colour.guide", "peaks"))
  set_annotations_default(c("=", "segments", "summaries"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("segments", "summaries"))
  set_annotations_default(c("=", ""))
  expect_setequal(getOption("photobiology.plot.annotations"), "")
  set_annotations_default(c("segments", "summaries"))
  expect_setequal(getOption("photobiology.plot.annotations"),
                  c("segments", "summaries"))
  set_annotations_default(c("color.guide", "colour.guide"))
  expect_setequal(getOption("photobiology.plot.annotations"), "colour.guide")
  set_annotations_default() # clean up
})

test_that("set_w.band_default", {
  set_w.band_default()
  expect_null(getOption("photobiology.plot.bands"))
  set_w.band_default(UVB())
  expect_setequal(getOption("photobiology.plot.bands"), list(UVB()))
  set_w.band_default(UV_bands())
  expect_setequal(getOption("photobiology.plot.bands"), UV_bands())
  expect_warning(set_w.band_default("UVB"))
  expect_warning(set_w.band_default(c(400,500)))
  expect_warning(set_w.band_default(list(a = 400, b = "a")))
  set_w.band_default() # clean up
})
