context("autoplot-response")
library(photobiology)
library(photobiologyWavebands)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 100L
ccd.spct <- interpolate_spct(photobiology::ccd.spct, length.out = length.out.spct)

test_that("response_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("response-default-scaled",
                              autoplot(fscale(ccd.spct, f = "mean", target = 1)))

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
  vdiffr::expect_doppelganger("response-range-num-shrink",
                              autoplot(ccd.spct, range = c(300, 700)))
  vdiffr::expect_doppelganger("response-range-num-shrink-long",
                              autoplot(ccd.spct, range = c(300, 500, 700)))
  vdiffr::expect_doppelganger("response-range-num-shrink-r",
                              autoplot(ccd.spct, range = c(NA, 700)))
  vdiffr::expect_doppelganger("response-range-num-shrink-l",
                              autoplot(ccd.spct, range = c(300, NA)))
  vdiffr::expect_doppelganger("response-range-num-expand",
                              autoplot(ccd.spct, range = c(150, 1200)))
  vdiffr::expect_doppelganger("response-range-num-expand-long",
                              autoplot(ccd.spct, range = c(150, 300, 1200)))
  vdiffr::expect_doppelganger("response-range-num-expand-r",
                              autoplot(ccd.spct, range = c(NA, 1200)))
  vdiffr::expect_doppelganger("response-range-num-expand-l",
                              autoplot(ccd.spct, range = c(150, NA)))
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
  vdiffr::expect_doppelganger("response-range-num-shrink-q",
                              autoplot(ccd.spct, unit.out = "photon", range = c(500, 700)))
  vdiffr::expect_doppelganger("response-range-num-shrink-r-q",
                              autoplot(ccd.spct, unit.out = "photon", range = c(NA, 700)))
  vdiffr::expect_doppelganger("response-range-num-shrink-l-q",
                              autoplot(ccd.spct, unit.out = "photon", range = c(500, NA)))
  vdiffr::expect_doppelganger("response-range-num-expand-q",
                              autoplot(ccd.spct, unit.out = "photon", range = c(150, 1200)))
  vdiffr::expect_doppelganger("response-range-num-expand-r-q",
                              autoplot(ccd.spct, unit.out = "photon", range = c(NA, 1200)))
  vdiffr::expect_doppelganger("response-range-num-expand-l-q",
                              autoplot(ccd.spct, unit.out = "photon", range = c(150, NA)))
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
                              autoplot(setTimeUnit(ccd.spct, "second", TRUE)))
  vdiffr::expect_doppelganger("response-hour",
                              autoplot(setTimeUnit(ccd.spct, "hour", TRUE)))
  vdiffr::expect_doppelganger("response-day",
                              autoplot(setTimeUnit(ccd.spct, "day", TRUE)))
  vdiffr::expect_doppelganger("response-exposure",
                              autoplot(setTimeUnit(ccd.spct, "exposure", TRUE)))
  vdiffr::expect_doppelganger("response-duration",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(4.5, "minutes"), TRUE)))
  vdiffr::expect_doppelganger("response-none",
                              autoplot(setTimeUnit(ccd.spct, "unknown", TRUE)))
  vdiffr::expect_doppelganger("response-seconds",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "seconds"), TRUE)))
  vdiffr::expect_doppelganger("response-hours",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "hours"), TRUE)))
  vdiffr::expect_doppelganger("response-days",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "days"), TRUE)))

  vdiffr::expect_doppelganger("response-second-q",
                              autoplot(setTimeUnit(ccd.spct, "second", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("response-hour-q",
                              autoplot(setTimeUnit(ccd.spct, "hour", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("response-day-q",
                              autoplot(setTimeUnit(ccd.spct, "day", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("response-exposure-q",
                              autoplot(setTimeUnit(ccd.spct, "exposure", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("response-duration-q",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(4.5, "minutes"), TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("response-none-q",
                              autoplot(setTimeUnit(ccd.spct, "unknown", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("response-seconds-q",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "seconds"), TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("response-hours-q",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "hours"), TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("response-days-q",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "days"), TRUE), unit.out = "photon"))

  })

test_that("response_mspct", {
  set_annotations_default()
  ccd.spct <- interpolate_spct(photobiology::ccd.spct, length.out = length.out.spct)
  two_ccds.mspct <- response_mspct(list(one = ccd.spct,
                                      half = ccd.spct / 2))

  vdiffr::expect_doppelganger("response-mspct-default",
                              autoplot(two_ccds.mspct))
  vdiffr::expect_doppelganger("response-mspct-default-range-shrink",
                              autoplot(two_ccds.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("response-mspct-default-range-expand",
                              autoplot(two_ccds.mspct, range = c(100, 1200)))
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
  # vdiffr::expect_warning("response-mspct-prod",
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
  vdiffr::expect_doppelganger("response-mspct-mean-e",
                              autoplot(two_ccds.mspct, plot.data = "mean", unit.out = "energy"))
  vdiffr::expect_doppelganger("response-mspct-mean-p",
                              autoplot(two_ccds.mspct, plot.data = "mean", unit.out = "photon"))
  vdiffr::expect_doppelganger("response-mspct-median-e",
                              autoplot(two_ccds.mspct, plot.data = "median", unit.out = "energy"))
  vdiffr::expect_doppelganger("response-mspct-median-p",
                              autoplot(two_ccds.mspct, plot.data = "median", unit.out = "photon"))
})
