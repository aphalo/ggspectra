context("plot")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 100L
ccd.spct <- interpolate_spct(photobiology::ccd.spct, length.out = length.out.spct)

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

test_that("response_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("response-default-scaled",
                              autoplot(fscale(ccd.spct, f = "mean", target = 1)))
  vdiffr::expect_doppelganger("response-default-normalized",
                              autoplot(ccd.spct, norm = 600))

  vdiffr::expect_doppelganger("response-default",
                              autoplot(ccd.spct))
  vdiffr::expect_doppelganger("response-default-ylim",
                              autoplot(ccd.spct, ylim = c(-0.1, 1.2)))
  vdiffr::expect_doppelganger("response-text-size",
                              autoplot(ccd.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("response-span-31",
                              autoplot(ccd.spct, span = 31))
  vdiffr::expect_doppelganger("response-wb-vis",
                              autoplot(ccd.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("response-label-average",
                              autoplot(ccd.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("response-label-mean",
                              autoplot(ccd.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("response-label-total",
                              autoplot(ccd.spct, label.qty = "total"))
  vdiffr::expect_doppelganger("response-label-contrib",
                              autoplot(ccd.spct, label.qty = "contribution"))
  vdiffr::expect_doppelganger("response-label-relative",
                              autoplot(ccd.spct, label.qty = "relative"))
  vdiffr::expect_doppelganger("response-range-num",
                              autoplot(ccd.spct, range = c(300, 700)))
  vdiffr::expect_doppelganger("response-range-wb",
                              autoplot(ccd.spct, range = waveband(c(300, 700))))
  vdiffr::expect_doppelganger("response-no-annotations",
                              autoplot(ccd.spct, annotations = ""))
  vdiffr::expect_doppelganger("response-reserve-space",
                              autoplot(ccd.spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("response-minus-annotations",
                              autoplot(ccd.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("response-plus-annotations",
                              autoplot(ccd.spct, annotations = c("+", "boundaries")))

  vdiffr::expect_doppelganger("response-default-q",
                              autoplot(ccd.spct, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-default-ylim-q",
                              autoplot(ccd.spct, unit.out = "photon", ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("response-text-size-q",
                              autoplot(ccd.spct, unit.out = "photon", text.size = 3.5))
  vdiffr::expect_doppelganger("response-span-31-q",
                              autoplot(ccd.spct, unit.out = "photon", span = 31))
  vdiffr::expect_doppelganger("response-wb-vis-q",
                              autoplot(ccd.spct, unit.out = "photon", w.band = VIS_bands()))
  vdiffr::expect_doppelganger("response-label-average-q",
                              autoplot(ccd.spct, unit.out = "photon", label.qty = "average"))
  vdiffr::expect_doppelganger("response-label-mean-q",
                              autoplot(ccd.spct, unit.out = "photon", label.qty = "mean"))
  vdiffr::expect_doppelganger("response-label-total-q",
                              autoplot(ccd.spct, unit.out = "photon", label.qty = "total"))
  vdiffr::expect_doppelganger("response-label-contrib-q",
                              autoplot(ccd.spct, unit.out = "photon", label.qty = "contribution"))
  vdiffr::expect_doppelganger("response-label-relative-q",
                              autoplot(ccd.spct, unit.out = "photon", label.qty = "relative"))
  vdiffr::expect_doppelganger("response-range-num-q",
                              autoplot(ccd.spct, unit.out = "photon", range = c(500, 700)))
  vdiffr::expect_doppelganger("response-range-wb-q",
                              autoplot(ccd.spct, unit.out = "photon", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("response-no-annotations-q",
                              autoplot(ccd.spct, unit.out = "photon", annotations = ""))
  vdiffr::expect_doppelganger("response-minus-summaries-q",
                              autoplot(ccd.spct, unit.out = "photon", annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("response-minus-boxes-q",
                              autoplot(ccd.spct, unit.out = "photon", annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("response-plus-boundaries-q",
                              autoplot(ccd.spct, unit.out = "photon", annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("response-plus-segments-q",
                              autoplot(ccd.spct, unit.out = "photon", annotations = c("+", "segments")))

  # time units
  vdiffr::expect_doppelganger("response-second",
                              autoplot(setTimeUnit(ccd.spct, "second", TRUE), norm = NULL))
  vdiffr::expect_doppelganger("response-hour",
                              autoplot(setTimeUnit(ccd.spct, "hour", TRUE), norm = NULL))
  vdiffr::expect_doppelganger("response-day",
                              autoplot(setTimeUnit(ccd.spct, "day", TRUE), norm = NULL))
  vdiffr::expect_doppelganger("response-exposure",
                              autoplot(setTimeUnit(ccd.spct, "exposure", TRUE), norm = NULL))
  vdiffr::expect_doppelganger("response-duration",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(4.5, "minutes"), TRUE), norm = NULL))
  vdiffr::expect_doppelganger("response-none",
                              autoplot(setTimeUnit(ccd.spct, "unknown", TRUE), norm = NULL))
  vdiffr::expect_doppelganger("response-seconds",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "seconds"), TRUE), norm = NULL))
  vdiffr::expect_doppelganger("response-hours",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "hours"), TRUE), norm = NULL))
  vdiffr::expect_doppelganger("response-days",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "days"), TRUE), norm = NULL))

  # vdiffr::expect_doppelganger("response-second-q",
  #                             autoplot(setTimeUnit(ccd.spct, "second", TRUE), norm = NULL, unit.out = "photon"))
  # vdiffr::expect_doppelganger("response-hour-q",
  #                             autoplot(setTimeUnit(ccd.spct, "hour", TRUE), norm = NULL, unit.out = "photon"))
  # vdiffr::expect_doppelganger("response-day-q",
  #                             autoplot(setTimeUnit(ccd.spct, "day", TRUE), norm = NULL, unit.out = "photon"))
  # vdiffr::expect_doppelganger("response-exposure-q",
  #                             autoplot(setTimeUnit(ccd.spct, "exposure", TRUE), norm = NULL, unit.out = "photon"))
  # vdiffr::expect_doppelganger("response-duration-q",
  #                             autoplot(setTimeUnit(ccd.spct, lubridate::duration(4.5, "minutes"), TRUE), norm = NULL, unit.out = "photon"))
  # vdiffr::expect_doppelganger("response-none-q",
  #                             autoplot(setTimeUnit(ccd.spct, "unknown", TRUE), norm = NULL, unit.out = "photon"))
  # vdiffr::expect_doppelganger("response-seconds-q",
  #                             autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "seconds"), TRUE), norm = NULL, unit.out = "photon"))
  # vdiffr::expect_doppelganger("response-hours-q",
  #                             autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "hours"), TRUE), norm = NULL, unit.out = "photon"))
  # vdiffr::expect_doppelganger("response-days-q",
  #                             autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "days"), TRUE), norm = NULL, unit.out = "photon"))

  })

test_that("response_mspct", {
  set_annotations_default()
  two_ccds.mspct <- response_mspct(list(one = ccd.spct,
                                      half = ccd.spct / 2))

  vdiffr::expect_doppelganger("response-mspct-default",
                              autoplot(two_ccds.mspct))
  vdiffr::expect_doppelganger("response-mspct-default-range",
                              autoplot(two_ccds.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("response-mspct-default-e",
                              autoplot(two_ccds.mspct, unit.out = "energy"))
  vdiffr::expect_doppelganger("response-mspct-default-p",
                              autoplot(two_ccds.mspct, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-mspct-no-annotations",
                              autoplot(two_ccds.mspct, annotations = ""))
  vdiffr::expect_doppelganger("response-mspct-reserve-space",
                              autoplot(two_ccds.mspct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("response-mspct-mean",
                              autoplot(two_ccds.mspct, plot.data = "mean"))
  vdiffr::expect_doppelganger("response-mspct-median",
                              autoplot(two_ccds.mspct, plot.data = "median"))
  vdiffr::expect_doppelganger("response-mspct-sum",
                              autoplot(two_ccds.mspct, plot.data = "sum"))
  # vdiffr::expect_doppelganger("response-mspct-sum",
  #                             autoplot(two_ccds.mspct, plot.data = "prod"))
  testthat::expect_warning(autoplot(two_ccds.mspct, plot.data = "prod"))
  vdiffr::expect_doppelganger("response-mspct-se",
                              autoplot(two_ccds.mspct, plot.data = "se"))
  vdiffr::expect_doppelganger("response-mspct-se-ylab",
                              autoplot(two_ccds.mspct, plot.data = "se", ylab = "Standard error of the mean"))
  vdiffr::expect_doppelganger("response-mspct-var",
                              autoplot(two_ccds.mspct, plot.data = "var"))
  vdiffr::expect_doppelganger("response-mspct-var-ylab",
                              autoplot(two_ccds.mspct, plot.data = "var", ylab = "Variance"))
  # vdiffr::expect_doppelganger("response-mspct-mean-e",
  #                             autoplot(two_ccds.mspct, plot.data = "mean", unit.out = "energy"))
  # vdiffr::expect_doppelganger("response-mspct-mean-p",
  #                             autoplot(two_ccds.mspct, plot.data = "mean", unit.out = "photon"))
  # vdiffr::expect_doppelganger("response-mspct-median-e",
  #                             autoplot(two_ccds.mspct, plot.data = "median", unit.out = "energy"))
  # vdiffr::expect_doppelganger("response-mspct-median-p",
  #                             autoplot(two_ccds.mspct, plot.data = "median", unit.out = "photon"))
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
