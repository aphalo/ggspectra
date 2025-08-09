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
  vdiffr::expect_doppelganger("source-default-scaled",
                              autoplot(fscale(white_led.source_spct)))
  vdiffr::expect_doppelganger("source-default-normalized",
                              autoplot(normalize(white_led.source_spct)))
  vdiffr::expect_doppelganger("source-default-norm-550",
                              autoplot(normalize(white_led.source_spct, norm = 550)))

  vdiffr::expect_doppelganger("source-default",
                              autoplot(white_led.source_spct))
  vdiffr::expect_doppelganger("source-default-geom-spct",
                              autoplot(white_led.source_spct, geom = "spct"))
  vdiffr::expect_doppelganger("source-ylim1",
                              autoplot(white_led.source_spct, ylim = c(NA, 1)))
  vdiffr::expect_doppelganger("source-ylim2",
                              autoplot(white_led.source_spct, ylim = c(-0.1, 1)))
  vdiffr::expect_doppelganger("source-text-size",
                              autoplot(white_led.source_spct, text.size = 3.5))
  vdiffr::expect_doppelganger("source-span-11",
                              autoplot(white_led.source_spct, span = 11))
  vdiffr::expect_doppelganger("source-wb-vis-1",
                              autoplot(white_led.source_spct, w.band = VIS()))
  vdiffr::expect_doppelganger("source-wb-vis",
                              autoplot(white_led.source_spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("source-wb-null",
                              autoplot(white_led.source_spct, w.band = NULL))
  vdiffr::expect_doppelganger("source-label-average",
                              autoplot(white_led.source_spct, label.qty = "average"))
  vdiffr::expect_doppelganger("source-label-mean",
                              autoplot(white_led.source_spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("source-label-total",
                              autoplot(white_led.source_spct, label.qty = "total"))
  vdiffr::expect_doppelganger("source-label-contrib",
                              autoplot(white_led.source_spct, label.qty = "contribution"))
  vdiffr::expect_doppelganger("source-label-relative",
                              autoplot(white_led.source_spct, label.qty = "relative"))
  vdiffr::expect_doppelganger("source-range-num-shrink",
                              autoplot(white_led.source_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("source-range-num-shrink-long",
                              autoplot(white_led.source_spct, range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("source-range-num-shrink-r",
                              autoplot(white_led.source_spct, range = c(NA, 700)))
  vdiffr::expect_doppelganger("source-range-num-shrink-l",
                              autoplot(white_led.source_spct, range = c(500, NA)))
  vdiffr::expect_doppelganger("source-range-num-expand",
                              autoplot(white_led.source_spct, range = c(200, 1100)))
  vdiffr::expect_doppelganger("source-range-num-expand-long",
                              autoplot(white_led.source_spct, range = c(200, 600, 1100)))
  vdiffr::expect_doppelganger("source-range-num-expand-r",
                              autoplot(white_led.source_spct, range = c(NA, 1100)))
  vdiffr::expect_doppelganger("source-range-num-expand-l",
                              autoplot(white_led.source_spct, range = c(200, NA)))
  vdiffr::expect_doppelganger("source-range-wb",
                              autoplot(white_led.source_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("source-no-annotations",
                              autoplot(white_led.source_spct, annotations = ""))
  vdiffr::expect_doppelganger("source-reserve-space",
                              autoplot(white_led.source_spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("source-minus-annotations",
                              autoplot(white_led.source_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("source-plus-annotations",
                              autoplot(white_led.source_spct, annotations = c("+", "boundaries")))

  vdiffr::expect_doppelganger("source-default-q-scaled",
                              autoplot(fscale(white_led.source_spct), unit.out = "photon"))
  vdiffr::expect_doppelganger("source-default-q-normalized",
                              autoplot(normalize(white_led.source_spct), unit.out = "photon"))

  vdiffr::expect_doppelganger("source-default-q",
                              autoplot(white_led.source_spct, unit.out = "photon"))
  vdiffr::expect_doppelganger("source-ylim1-q",
                              autoplot(white_led.source_spct, unit.out = "photon", ylim = c(NA, 6)))
  vdiffr::expect_doppelganger("source-ylim2-q",
                              autoplot(white_led.source_spct, unit.out = "photon", ylim = c(-0.5, 6)))
  vdiffr::expect_doppelganger("source-text-size-q",
                              autoplot(white_led.source_spct, unit.out = "photon", text.size = 3.5))
  vdiffr::expect_doppelganger("source-span-11-q",
                              autoplot(white_led.source_spct, unit.out = "photon", span = 11))
  vdiffr::expect_doppelganger("source-wb-vis-q",
                              autoplot(white_led.source_spct, unit.out = "photon", w.band = VIS_bands()))
  vdiffr::expect_doppelganger("source-label-average-q",
                              autoplot(white_led.source_spct, unit.out = "photon", label.qty = "average"))
  vdiffr::expect_doppelganger("source-label-mean-q",
                              autoplot(white_led.source_spct, unit.out = "photon", label.qty = "mean"))
  vdiffr::expect_doppelganger("source-range-num-shrink-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(500, 700)))
  vdiffr::expect_doppelganger("source-range-num-shrink-long-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(500, 600, 700)))
  vdiffr::expect_doppelganger("source-range-num-shrink-r-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(NA, 700)))
  vdiffr::expect_doppelganger("source-range-num-shrink-l-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(500, NA)))
  vdiffr::expect_doppelganger("source-range-num-expand-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(200, 1100)))
  vdiffr::expect_doppelganger("source-range-num-expand-long-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(200, 600, 1100)))
  vdiffr::expect_doppelganger("source-range-num-expand-r-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(NA, 1100)))
  vdiffr::expect_doppelganger("source-range-num-expand-l-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(200, NA)))
  vdiffr::expect_doppelganger("source-range-wb-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("source-no-annotations-q",
                              autoplot(white_led.source_spct, unit.out = "photon", annotations = ""))
  vdiffr::expect_doppelganger("source-minus-summaries-q",
                              autoplot(white_led.source_spct, unit.out = "photon", annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("source-minus-boxes-q",
                              autoplot(white_led.source_spct, unit.out = "photon", annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("source-plus-boundaries-q",
                              autoplot(white_led.source_spct, unit.out = "photon", annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("source-plus-segments-q",
                              autoplot(white_led.source_spct, unit.out = "photon", annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("source-normalized",
                              autoplot(normalize(white_led.source_spct)))
  vdiffr::expect_doppelganger("source-normalized-pcout",
                              autoplot(normalize(white_led.source_spct), pc.out = TRUE))
  vdiffr::expect_doppelganger("source-normalized-wl",
                              autoplot(normalize(white_led.source_spct, norm = 550)))
  vdiffr::expect_doppelganger("source-normalized-wl-pcout",
                              autoplot(normalize(white_led.source_spct, norm = 550), pc.out = TRUE))
  vdiffr::expect_doppelganger("source-normalized-ylim",
                              autoplot(normalize(white_led.source_spct), ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("source-scaled",
                              autoplot(fscale(white_led.source_spct)))
  vdiffr::expect_doppelganger("source-scaled-total",
                              autoplot(fscale(white_led.source_spct, f = "total")))
  vdiffr::expect_doppelganger("source-scaled-mean",
                              autoplot(fscale(white_led.source_spct, f = "mean")))
  vdiffr::expect_doppelganger("source-scaled-mean-2",
                              autoplot(fscale(white_led.source_spct, target = 2)))
  vdiffr::expect_doppelganger("source-scaled-mean-2-PAR",
                              autoplot(fscale(white_led.source_spct, target = 2, range = PhR())))
  vdiffr::expect_doppelganger("source-scaled-total-200",
                              autoplot(fscale(white_led.source_spct, f = "total", target = 200)))
  vdiffr::expect_doppelganger("source-scaled-total-200-PAR",
                              autoplot(fscale(white_led.source_spct, f = "total", target = 200, range = PhR())))
  vdiffr::expect_doppelganger("source-scaled-irrad-200",
                              autoplot(fscale(white_led.source_spct, f = irrad, target = 200)))
  vdiffr::expect_doppelganger("source-scaled-irrad-200-PAR",
                              autoplot(fscale(white_led.source_spct, f = irrad, target = 200, range = PhR())))
  vdiffr::expect_doppelganger("source-product-wb",
                              autoplot(photobiology::sun.spct * Red()))
  vdiffr::expect_doppelganger("source-product-BSWF",
                              autoplot(sun.spct * CIE()))
  vdiffr::expect_doppelganger("source-product-BSWF-wl",
                              autoplot(sun.spct * CIE(300)))
  # time units
  vdiffr::expect_doppelganger("source-second",
                              autoplot(setTimeUnit(white_led.source_spct, "second", TRUE)))
  vdiffr::expect_doppelganger("source-hour",
                              autoplot(setTimeUnit(white_led.source_spct, "hour", TRUE)))
  vdiffr::expect_doppelganger("source-day",
                              autoplot(setTimeUnit(white_led.source_spct, "day", TRUE)))
  vdiffr::expect_doppelganger("source-exposure",
                              autoplot(setTimeUnit(white_led.source_spct, "exposure", TRUE)))
  vdiffr::expect_doppelganger("source-duration",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(4.5, "minutes"), TRUE)))
  vdiffr::expect_doppelganger("source-none",
                              autoplot(setTimeUnit(white_led.source_spct, "unknown", TRUE)))
  vdiffr::expect_doppelganger("source-seconds",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "seconds"), TRUE)))
  vdiffr::expect_doppelganger("source-hours",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "hours"), TRUE)))
  vdiffr::expect_doppelganger("source-days",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "days"), TRUE)))

  vdiffr::expect_doppelganger("source-second-q",
                              autoplot(setTimeUnit(white_led.source_spct, "second", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("source-hour-q",
                              autoplot(setTimeUnit(white_led.source_spct, "hour", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("source-day-q",
                              autoplot(setTimeUnit(white_led.source_spct, "day", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("source-exposure-q",
                              autoplot(setTimeUnit(white_led.source_spct, "exposure", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("source-duration-q",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(4.5, "minutes"), TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("source-none-q",
                              autoplot(setTimeUnit(white_led.source_spct, "unknown", TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("source-seconds-q",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "seconds"), TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("source-hours-q",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "hours"), TRUE), unit.out = "photon"))
  vdiffr::expect_doppelganger("source-days-q",
                              autoplot(setTimeUnit(white_led.source_spct, lubridate::duration(1, "days"), TRUE), unit.out = "photon"))

  })

test_that("source_mspct", {
  set_annotations_default()
  white_led.source_spct <- interpolate_spct(photobiology::white_led.source_spct, length.out = length.out.spct)
  two_leds.mspct <- source_mspct(list(one = white_led.source_spct,
                                      half = white_led.source_spct / 2))

  vdiffr::expect_doppelganger("source-mspct-default",
                              autoplot(two_leds.mspct))
  vdiffr::expect_doppelganger("source-mspct-default-idfactor",
                              autoplot(two_leds.mspct, idfactor = "spectrum"))
  vdiffr::expect_doppelganger("source-mspct-default-geom-spct",
                              autoplot(two_leds.mspct, geom = "spct"))
  vdiffr::expect_doppelganger("source-mspct-ylim",
                              autoplot(two_leds.mspct, ylim = c(-0.1, 1.2)))
  vdiffr::expect_doppelganger("source-mspct-default-range",
                              autoplot(two_leds.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("source-mspct-default-wband-wb",
                              autoplot(two_leds.mspct, w.band = VIS()))
  vdiffr::expect_doppelganger("source-mspct-default-wband-wbls",
                              autoplot(two_leds.mspct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("source-mspct-default-wband-null",
                              autoplot(two_leds.mspct, w.band = NULL))
  vdiffr::expect_doppelganger("source-mspct-default-e",
                              autoplot(two_leds.mspct, unit.out = "energy"))
  vdiffr::expect_doppelganger("source-mspct-default-p",
                              autoplot(two_leds.mspct, unit.out = "photon"))
  vdiffr::expect_doppelganger("source-mspct-no-annotations",
                              autoplot(two_leds.mspct, annotations = ""))
  vdiffr::expect_doppelganger("source-mspct-reserve-space",
                              autoplot(two_leds.mspct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("source-mspct-mean",
                              autoplot(two_leds.mspct, plot.data = "mean"))
  vdiffr::expect_doppelganger("source-mspct-median",
                              autoplot(two_leds.mspct, plot.data = "median"))
  vdiffr::expect_doppelganger("source-mspct-sum",
                              autoplot(two_leds.mspct, plot.data = "sum"))
  # triggers warning
  # vdiffr::expect_doppelganger("source-mspct-sum",
  #                             autoplot(two_leds.mspct, plot.data = "prod"))
  testthat::expect_warning(autoplot(two_leds.mspct, plot.data = "prod"))
  vdiffr::expect_doppelganger("source-mspct-se",
                              autoplot(two_leds.mspct, plot.data = "se"))
  vdiffr::expect_doppelganger("source-mspct-se-ylab",
                              autoplot(two_leds.mspct, plot.data = "se", ylab = "Standard error of the mean"))
  vdiffr::expect_doppelganger("source-mspct-var",
                              autoplot(two_leds.mspct, plot.data = "var"))
  vdiffr::expect_doppelganger("source-mspct-var-ylab",
                              autoplot(two_leds.mspct, plot.data = "var", ylab = "Variance"))
  vdiffr::expect_doppelganger("source-mspct-mean-e",
                              autoplot(two_leds.mspct, plot.data = "mean", unit.out = "energy"))
  vdiffr::expect_doppelganger("source-mspct-median-e",
                              autoplot(two_leds.mspct, plot.data = "median", unit.out = "energy"))
  vdiffr::expect_doppelganger("source-mspct-mean-p",
                              autoplot(two_leds.mspct, plot.data = "mean", unit.out = "photon"))
  vdiffr::expect_doppelganger("source-mspct-median-p",
                              autoplot(two_leds.mspct, plot.data = "median", unit.out = "photon"))

  vdiffr::expect_doppelganger("source-mspct-facets1",
                              autoplot(two_leds.mspct, facets = 1))
  vdiffr::expect_doppelganger("source-mspct-facets2",
                              autoplot(two_leds.mspct, facets = 2))
  vdiffr::expect_doppelganger("source-mspct-facetsT",
                              autoplot(two_leds.mspct, facets = TRUE))
  vdiffr::expect_doppelganger("source-mspct-facetsT-geom-spct",
                              autoplot(two_leds.mspct, facets = TRUE, geom = "spct"))

})
