context("autoplot-source")
library(ggplot2)
library(photobiology)
library(photobiologyWavebands)

# We prepare shorter data objects for spectra to improve tests' runtime.
# We also make sure no data are exactly at the boundary of the possible range to
# avoid warnings. Such values are physically possible but not measurable.
length.out.spct <- 100L
sun.spct <- interpolate_spct(photobiology::sun.spct, length.out = length.out.spct)
white_led.source_spct <- interpolate_spct(photobiology::white_led.source_spct, length.out = length.out.spct)

test_that("source_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("src-default-scaled",
                              autoplot(fscale(white_led.source_spct)))
  vdiffr::expect_doppelganger("src-default-normalized",
                              autoplot(normalize(white_led.source_spct)))
  vdiffr::expect_doppelganger("src-default-norm-550",
                              autoplot(normalize(white_led.source_spct, norm = 550)))

  vdiffr::expect_doppelganger("src-default",
                              autoplot(white_led.source_spct))
  vdiffr::expect_doppelganger("src-default-geom-spct",
                              autoplot(white_led.source_spct, geom = "spct"))
  vdiffr::expect_doppelganger("src-ylim1",
                              autoplot(white_led.source_spct, ylim = c(NA, 1)))
  vdiffr::expect_doppelganger("src-ylim2",
                              autoplot(white_led.source_spct, ylim = c(-0.1, 1)))
  vdiffr::expect_doppelganger("src-text-size",
                              autoplot(white_led.source_spct, text.size = 3.5))
  vdiffr::expect_doppelganger("src-span-11",
                              autoplot(white_led.source_spct, span = 11))
  vdiffr::expect_doppelganger("src-wb-vis-1",
                              autoplot(white_led.source_spct, w.band = VIS()))
  vdiffr::expect_doppelganger("src-wb-vis",
                              autoplot(white_led.source_spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("src-wb-null",
                              autoplot(white_led.source_spct, w.band = NULL))
  vdiffr::expect_doppelganger("src-label-average",
                              autoplot(white_led.source_spct, label.qty = "average"))
  vdiffr::expect_doppelganger("src-label-mean",
                              autoplot(white_led.source_spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("src-label-total",
                              autoplot(white_led.source_spct, label.qty = "total"))
  vdiffr::expect_doppelganger("src-label-contrib",
                              autoplot(white_led.source_spct, label.qty = "contribution"))
  vdiffr::expect_doppelganger("src-label-relative",
                              autoplot(white_led.source_spct, label.qty = "relative"))
  vdiffr::expect_doppelganger("src-range-num-shrink",
                              autoplot(white_led.source_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("src-range-num-shrink-long",
                              autoplot(white_led.source_spct, range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("src-range-num-shrink-r",
                              autoplot(white_led.source_spct, range = c(NA, 700)))
  vdiffr::expect_doppelganger("src-range-num-shrink-l",
                              autoplot(white_led.source_spct, range = c(500, NA)))
  vdiffr::expect_doppelganger("src-range-num-expand",
                              autoplot(white_led.source_spct, range = c(200, 1100)))
  vdiffr::expect_doppelganger("src-range-num-expand-long",
                              autoplot(white_led.source_spct, range = c(200, 600, 1100)))
  vdiffr::expect_doppelganger("src-range-num-expand-r",
                              autoplot(white_led.source_spct, range = c(NA, 1100)))
  vdiffr::expect_doppelganger("src-range-num-expand-l",
                              autoplot(white_led.source_spct, range = c(200, NA)))
  vdiffr::expect_doppelganger("src-range-wb",
                              autoplot(white_led.source_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("src-no-annotations",
                              autoplot(white_led.source_spct, annotations = ""))
  vdiffr::expect_doppelganger("src-reserve-space",
                              autoplot(white_led.source_spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("src-minus-annotations",
                              autoplot(white_led.source_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("src-plus-annotations",
                              autoplot(white_led.source_spct, annotations = c("+", "boundaries")))

  vdiffr::expect_doppelganger("src-default-q-scaled",
                              autoplot(fscale(white_led.source_spct), unit.out = "photon"))
  vdiffr::expect_doppelganger("src-default-q-normalized",
                              autoplot(normalize(white_led.source_spct), unit.out = "photon"))

  vdiffr::expect_doppelganger("src-default-q",
                              autoplot(white_led.source_spct, unit.out = "photon"))
  vdiffr::expect_doppelganger("src-ylim1-q",
                              autoplot(white_led.source_spct, unit.out = "photon", ylim = c(NA, 6)))
  vdiffr::expect_doppelganger("src-ylim2-q",
                              autoplot(white_led.source_spct, unit.out = "photon", ylim = c(-0.5, 6)))
  vdiffr::expect_doppelganger("src-text-size-q",
                              autoplot(white_led.source_spct, unit.out = "photon", text.size = 3.5))
  vdiffr::expect_doppelganger("src-span-11-q",
                              autoplot(white_led.source_spct, unit.out = "photon", span = 11))
  vdiffr::expect_doppelganger("src-wb-vis-q",
                              autoplot(white_led.source_spct, unit.out = "photon", w.band = VIS_bands()))
  vdiffr::expect_doppelganger("src-label-average-q",
                              autoplot(white_led.source_spct, unit.out = "photon", label.qty = "average"))
  vdiffr::expect_doppelganger("src-label-mean-q",
                              autoplot(white_led.source_spct, unit.out = "photon", label.qty = "mean"))
  vdiffr::expect_doppelganger("src-range-num-shrink-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(500, 700)))
  vdiffr::expect_doppelganger("src-range-num-shrink-long-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("src-range-num-shrink-r-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(NA, 700)))
  vdiffr::expect_doppelganger("src-range-num-shrink-l-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(500, NA)))
  vdiffr::expect_doppelganger("src-range-num-expand-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(200, 1100)))
  vdiffr::expect_doppelganger("src-range-num-expand-long-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(200, 600, 1100)))
  vdiffr::expect_doppelganger("src-range-num-expand-r-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(NA, 1100)))
  vdiffr::expect_doppelganger("src-range-num-expand-l-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(200, NA)))
  vdiffr::expect_doppelganger("src-range-wb-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("src-no-annotations-q",
                              autoplot(white_led.source_spct, unit.out = "photon", annotations = ""))
  vdiffr::expect_doppelganger("src-minus-summaries-q",
                              autoplot(white_led.source_spct, unit.out = "photon", annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("src-minus-boxes-q",
                              autoplot(white_led.source_spct, unit.out = "photon", annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("src-plus-boundaries-q",
                              autoplot(white_led.source_spct, unit.out = "photon", annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("src-plus-segments-q",
                              autoplot(white_led.source_spct, unit.out = "photon", annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("src-normalized",
                              autoplot(normalize(white_led.source_spct)))
  vdiffr::expect_doppelganger("src-normalized-pcout",
                              autoplot(normalize(white_led.source_spct), pc.out = TRUE))
  vdiffr::expect_doppelganger("src-normalized-wl",
                              autoplot(normalize(white_led.source_spct, norm = 550)))
  vdiffr::expect_doppelganger("src-normalized-wl-pcout",
                              autoplot(normalize(white_led.source_spct, norm = 550), pc.out = TRUE))
  vdiffr::expect_doppelganger("src-normalized-ylim",
                              autoplot(normalize(white_led.source_spct), ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("src-scaled",
                              autoplot(fscale(white_led.source_spct)))
  vdiffr::expect_doppelganger("src-scaled-total",
                              autoplot(fscale(white_led.source_spct, f = "total")))
  vdiffr::expect_doppelganger("src-scaled-mean",
                              autoplot(fscale(white_led.source_spct, f = "mean")))
  vdiffr::expect_doppelganger("src-scaled-mean-2",
                              autoplot(fscale(white_led.source_spct, target = 2)))
  vdiffr::expect_doppelganger("src-scaled-mean-2-PAR",
                              autoplot(fscale(white_led.source_spct, target = 2, range = PhR())))
  vdiffr::expect_doppelganger("src-scaled-total-200",
                              autoplot(fscale(white_led.source_spct, f = "total", target = 200)))
  vdiffr::expect_doppelganger("src-scaled-total-200-PAR",
                              autoplot(fscale(white_led.source_spct, f = "total", target = 200, range = PhR())))
  vdiffr::expect_doppelganger("src-scaled-irrad-200",
                              autoplot(fscale(white_led.source_spct, f = irrad, target = 200)))
  vdiffr::expect_doppelganger("src-scaled-irrad-200-PAR",
                              autoplot(fscale(white_led.source_spct, f = irrad, target = 200, range = PhR())))
  vdiffr::expect_doppelganger("src-product-wb",
                              autoplot(photobiology::sun.spct * Red()))
  vdiffr::expect_doppelganger("src-product-BSWF",
                              autoplot(sun.spct * CIE()))
  vdiffr::expect_doppelganger("src-product-BSWF-wl",
                              autoplot(sun.spct * CIE(300)))
  # time units
  vdiffr::expect_doppelganger("src-second",
                              autoplot(setTimeUnit(white_led.source_spct, "second", TRUE)))
  vdiffr::expect_doppelganger("src-hour",
                              autoplot(setTimeUnit(white_led.source_spct, "hour", TRUE)))
  vdiffr::expect_doppelganger("src-day",
                              autoplot(setTimeUnit(white_led.source_spct, "day", TRUE)))
  vdiffr::expect_doppelganger("src-exposure",
                              autoplot(setTimeUnit(white_led.source_spct, "exposure", TRUE)))
  vdiffr::expect_doppelganger("src-duration",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(4.5, "minutes"), TRUE)))
  vdiffr::expect_doppelganger("src-none",
                              autoplot(setTimeUnit(white_led.source_spct, "unknown", TRUE)))
  vdiffr::expect_doppelganger("src-seconds",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "seconds"), TRUE)))
  vdiffr::expect_doppelganger("src-hours",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "hours"), TRUE)))
  vdiffr::expect_doppelganger("src-days",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "days"), TRUE)))

  vdiffr::expect_doppelganger("src-second-q",
                              autoplot(setTimeUnit(white_led.source_spct, "second", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("src-hour-q",
                              autoplot(setTimeUnit(white_led.source_spct, "hour", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("src-day-q",
                              autoplot(setTimeUnit(white_led.source_spct, "day", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("src-exposure-q",
                              autoplot(setTimeUnit(white_led.source_spct, "exposure", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("src-duration-q",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(4.5, "minutes"), TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("src-none-q",
                              autoplot(setTimeUnit(white_led.source_spct, "unknown", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("src-seconds-q",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "seconds"), TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("src-hours-q",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "hours"), TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("src-days-q",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "days"), TRUE), unit.out = "photon"))

  })

test_that("source_mspct", {
  set_annotations_default()
  white_led.source_spct <- interpolate_spct(photobiology::white_led.source_spct, length.out = length.out.spct)
  two_leds.mspct <- source_mspct(list(one = white_led.source_spct,
                                      half = white_led.source_spct / 2))

  vdiffr::expect_doppelganger("src-mspct-default",
                              autoplot(two_leds.mspct))
  vdiffr::expect_doppelganger("src-mspct-default-idfactor",
                              autoplot(two_leds.mspct, idfactor = "spectrum"))
  vdiffr::expect_doppelganger("src-mspct-default-geom-spct",
                              autoplot(two_leds.mspct, geom = "spct"))
  vdiffr::expect_doppelganger("src-mspct-ylim",
                              autoplot(two_leds.mspct, ylim = c(-0.1, 1.2)))
  vdiffr::expect_doppelganger("src-mspct-default-range-shrink",
                              autoplot(two_leds.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("src-mspct-default-range-shrink-long",
                              autoplot(two_leds.mspct, range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("src-mspct-default-range-shrink-r",
                              autoplot(two_leds.mspct, range = c(NA, 700)))
  vdiffr::expect_doppelganger("src-mspct-default-range-shrink-l",
                              autoplot(two_leds.mspct, range = c(500, NA)))
  vdiffr::expect_doppelganger("src-mspct-default-range-expand",
                              autoplot(two_leds.mspct, range = c(200, 1100)))
  vdiffr::expect_doppelganger("src-mspct-default-range-expand-long",
                              autoplot(two_leds.mspct, range = c(200, 600, 1100)))
  vdiffr::expect_doppelganger("src-mspct-default-range-expand-r",
                              autoplot(two_leds.mspct, range = c(NA, 1100)))
  vdiffr::expect_doppelganger("src-mspct-default-range-expand-l",
                              autoplot(two_leds.mspct, range = c(200, NA)))
  vdiffr::expect_doppelganger("src-mspct-default-wband-wb",
                              autoplot(two_leds.mspct, w.band = VIS()))
  vdiffr::expect_doppelganger("src-mspct-default-wband-wbls",
                              autoplot(two_leds.mspct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("src-mspct-default-wband-null",
                              autoplot(two_leds.mspct, w.band = NULL))
  vdiffr::expect_doppelganger("src-mspct-default-e",
                              autoplot(two_leds.mspct, unit.out = "energy"))
  vdiffr::expect_doppelganger("src-mspct-default-p",
                              autoplot(two_leds.mspct, unit.out = "photon"))
  vdiffr::expect_doppelganger("src-mspct-no-annotations",
                              autoplot(two_leds.mspct, annotations = ""))
  vdiffr::expect_doppelganger("src-mspct-reserve-space",
                              autoplot(two_leds.mspct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("src-mspct-mean",
                              autoplot(two_leds.mspct, plot.data = "mean"))
  vdiffr::expect_doppelganger("src-mspct-median",
                              autoplot(two_leds.mspct, plot.data = "median"))
  vdiffr::expect_doppelganger("src-mspct-sum",
                              autoplot(two_leds.mspct, plot.data = "sum"))
  # triggers warning
  # vdiffr::expect_doppelganger("src-mspct-prod",
  #                             autoplot(two_leds.mspct, plot.data = "prod"))
  testthat::expect_warning(autoplot(two_leds.mspct, plot.data = "prod"))
  vdiffr::expect_doppelganger("src-mspct-se",
                              autoplot(two_leds.mspct, plot.data = "se"))
  vdiffr::expect_doppelganger("src-mspct-se-ylab",
                              autoplot(two_leds.mspct, plot.data = "se", ylab = "Standard error of the mean"))
  vdiffr::expect_doppelganger("src-mspct-var",
                              autoplot(two_leds.mspct, plot.data = "var"))
  vdiffr::expect_doppelganger("src-mspct-var-ylab",
                              autoplot(two_leds.mspct, plot.data = "var", ylab = "Variance"))
  vdiffr::expect_doppelganger("src-mspct-mean-e",
                              autoplot(two_leds.mspct, plot.data = "mean", unit.out = "energy"))
  vdiffr::expect_doppelganger("src-mspct-median-e",
                              autoplot(two_leds.mspct, plot.data = "median", unit.out = "energy"))
  vdiffr::expect_doppelganger("src-mspct-mean-p",
                              autoplot(two_leds.mspct, plot.data = "mean", unit.out = "photon"))
  vdiffr::expect_doppelganger("src-mspct-median-p",
                              autoplot(two_leds.mspct, plot.data = "median", unit.out = "photon"))

  vdiffr::expect_doppelganger("src-mspct-facets1",
                              autoplot(two_leds.mspct, facets = 1))
  vdiffr::expect_doppelganger("src-mspct-facets2",
                              autoplot(two_leds.mspct, facets = 2))
  vdiffr::expect_doppelganger("src-mspct-facetsT",
                              autoplot(two_leds.mspct, facets = TRUE))
  vdiffr::expect_doppelganger("src-mspct-facetsT-geom-spct",
                              autoplot(two_leds.mspct, facets = TRUE, geom = "spct"))

})
