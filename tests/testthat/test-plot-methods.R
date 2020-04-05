context("plot")
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

test_that("source_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("source-default",
                              autoplot(white_led.source_spct))
  vdiffr::expect_doppelganger("source-ylim1",
                              autoplot(white_led.source_spct, ylim = c(NA, 1)))
  vdiffr::expect_doppelganger("source-ylim2",
                              autoplot(white_led.source_spct, ylim = c(-0.1, 1)))
  vdiffr::expect_doppelganger("source-text-size",
                              autoplot(white_led.source_spct, text.size = 3.5))
  vdiffr::expect_doppelganger("source-span-31",
                              autoplot(white_led.source_spct, span = 31))
  vdiffr::expect_doppelganger("source-wb-vis",
                              autoplot(white_led.source_spct, w.band = VIS_bands()))
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
  vdiffr::expect_doppelganger("source-range-num",
                              autoplot(white_led.source_spct, range = c(500, 700)))
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

  vdiffr::expect_doppelganger("source-default-q",
                              autoplot(white_led.source_spct, unit.out = "photon"))
  vdiffr::expect_doppelganger("source-ylim1-q",
                              autoplot(white_led.source_spct, unit.out = "photon", ylim = c(NA, 6)))
  vdiffr::expect_doppelganger("source-ylim2-q",
                              autoplot(white_led.source_spct, unit.out = "photon", ylim = c(-0.5, 6)))
  vdiffr::expect_doppelganger("source-text-size-q",
                              autoplot(white_led.source_spct, unit.out = "photon", text.size = 3.5))
  vdiffr::expect_doppelganger("source-span-31-q",
                              autoplot(white_led.source_spct, unit.out = "photon", span = 31))
  vdiffr::expect_doppelganger("source-wb-vis-q",
                              autoplot(white_led.source_spct, unit.out = "photon", w.band = VIS_bands()))
  vdiffr::expect_doppelganger("source-label-average-q",
                              autoplot(white_led.source_spct, unit.out = "photon", label.qty = "average"))
  vdiffr::expect_doppelganger("source-label-mean-q",
                              autoplot(white_led.source_spct, unit.out = "photon", label.qty = "mean"))
  vdiffr::expect_doppelganger("source-range-num-q",
                              autoplot(white_led.source_spct, unit.out = "photon", range = c(500, 700)))
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
  vdiffr::expect_doppelganger("source-normalized-wl",
                              autoplot(normalize(white_led.source_spct, norm = 550)))
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
                              autoplot(fscale(white_led.source_spct, target = 2, range = PAR())))
  vdiffr::expect_doppelganger("source-scaled-total-200",
                              autoplot(fscale(white_led.source_spct, f = "total", target = 200)))
  vdiffr::expect_doppelganger("source-scaled-total-200-PAR",
                              autoplot(fscale(white_led.source_spct, f = "total", target = 200, range = PAR())))
  vdiffr::expect_doppelganger("source-scaled-irrad-200",
                              autoplot(fscale(white_led.source_spct, f = irrad, target = 200)))
  vdiffr::expect_doppelganger("source-scaled-irrad-200-PAR",
                              autoplot(fscale(white_led.source_spct, f = irrad, target = 200, range = PAR())))
  vdiffr::expect_doppelganger("source-product-wb",
                              autoplot(sun.spct * Red()))
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
  two_leds.mspct <- source_mspct(list(one = white_led.source_spct,
                                      half = white_led.source_spct / 2))

  vdiffr::expect_doppelganger("source-mspct-default",
                              autoplot(two_leds.mspct))
  vdiffr::expect_doppelganger("source-mspct-ylim",
                              autoplot(two_leds.mspct, ylim = c(-0.1, 1.2)))
  vdiffr::expect_doppelganger("source-mspct-default-range",
                              autoplot(two_leds.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("source-mspct-default-e",
                              autoplot(two_leds.mspct, unit.out = "energy"))
  vdiffr::expect_doppelganger("source-mspct-default-p",
                              autoplot(two_leds.mspct, unit.out = "photon"))
  vdiffr::expect_doppelganger("source-mspct-no-annotations",
                              autoplot(two_leds.mspct, annotations = ""))
  vdiffr::expect_doppelganger("source-mspct-reserve-space",
                              autoplot(two_leds.mspct, annotations = "reserve.space"))
})

test_that("filter_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("filter-default-tot",
                              autoplot(Ler_leaf_trns.spct))
  vdiffr::expect_doppelganger("filter-default-tot-ylim",
                              autoplot(Ler_leaf_trns.spct, ylim = c(-0.05, 0.5)))
  vdiffr::expect_doppelganger("filter-default-pc-out-tot",
                              autoplot(Ler_leaf_trns.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size-tot",
                              autoplot(Ler_leaf_trns.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-tot",
                              autoplot(Ler_leaf_trns.spct, span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-tot",
                              autoplot(Ler_leaf_trns.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-tot",
                              autoplot(Ler_leaf_trns.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-tot",
                              autoplot(Ler_leaf_trns.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num-tot",
                              autoplot(Ler_leaf_trns.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb-tot",
                              autoplot(Ler_leaf_trns.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-tot",
                              autoplot(Ler_leaf_trns.spct, annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-annotations-tot",
                              autoplot(Ler_leaf_trns.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-plus-annotations-tot",
                              autoplot(Ler_leaf_trns.spct, annotations = c("+", "boundaries")))

  vdiffr::expect_doppelganger("filter-default",
                              autoplot(Ler_leaf_trns_i.spct))
  vdiffr::expect_doppelganger("filter-default-ylim",
                              autoplot(Ler_leaf_trns_i.spct, ylim = c(NA, 1.2)))
  vdiffr::expect_doppelganger("filter-default-pc-out",
                              autoplot(Ler_leaf_trns_i.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size",
                              autoplot(Ler_leaf_trns_i.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31",
                              autoplot(Ler_leaf_trns_i.spct, span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis",
                              autoplot(Ler_leaf_trns_i.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average",
                              autoplot(Ler_leaf_trns_i.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean",
                              autoplot(Ler_leaf_trns_i.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num",
                              autoplot(Ler_leaf_trns_i.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb",
                              autoplot(Ler_leaf_trns_i.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations",
                              autoplot(Ler_leaf_trns_i.spct, annotations = ""))
  vdiffr::expect_doppelganger("filter-reserve-space",
                              autoplot(Ler_leaf_trns_i.spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("filter-minus-summaries",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments",
                              autoplot(Ler_leaf_trns_i.spct, annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("filter-default-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("filter-default-apt-ylim",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", ylim = c(-0.1, 1.2)))
  vdiffr::expect_doppelganger("filter-default-pc-out-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-summaries-q",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments-apt",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("filter-default-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA)))
  vdiffr::expect_doppelganger("filter-default-a-ylim",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), ylim = c(-0.2, 4)))
  vdiffr::expect_doppelganger("filter-text-size-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-wb-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-summaries-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments-a",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("filter-default-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("filter-default-tfr-ylim",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("filter-default-pc-out-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-summaries-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments-tfr",
                              autoplot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", annotations = c("+", "segments")))
})

test_that("filter_mspct", {
  set_annotations_default()
  two_leaves.mspct <- filter_mspct(list(one = Ler_leaf_trns_i.spct,
                                      half = Ler_leaf_trns_i.spct / 2))
  vdiffr::expect_doppelganger("filter-mspct-default",
                              autoplot(two_leaves.mspct))
  vdiffr::expect_doppelganger("filter-mspct-default-ylim",
                              autoplot(two_leaves.mspct, ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("filter-mspct-default-range",
                              autoplot(two_leaves.mspct, range = c(500, 700)))
  # use range to avoid Tfr == 0 values.
  vdiffr::expect_doppelganger("filter-mspct-default-A",
                              autoplot(two_leaves.mspct,
                                   range = c(400,750),
                                   plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("filter-mspct-default-Afr",
                              autoplot(two_leaves.mspct,
                                   range = c(400,750),
                                   plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("filter-mspct-default-Tfr",
                              autoplot(two_leaves.mspct, plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("filter-mspct-no-annotations",
                              autoplot(two_leaves.mspct, annotations = ""))
  vdiffr::expect_doppelganger("filter-mspct-reserve-space",
                              autoplot(two_leaves.mspct, annotations = "reserve.space"))
})

test_that("reflector_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("reflector-default-tot",
                              autoplot(Ler_leaf_rflt.spct))
  vdiffr::expect_doppelganger("reflector-default-tot-ylim",
                              autoplot(Ler_leaf_rflt.spct, ylim = c(-0.1, 0.65)))
  vdiffr::expect_doppelganger("reflector-default-pc-out-tot",
                              autoplot(Ler_leaf_rflt.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("reflector-text-size-tot",
                              autoplot(Ler_leaf_rflt.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("reflector-span-31-tot",
                              autoplot(Ler_leaf_rflt.spct, span = 31))
  vdiffr::expect_doppelganger("reflector-wb-vis-tot",
                              autoplot(Ler_leaf_rflt.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("reflector-label-average-tot",
                              autoplot(Ler_leaf_rflt.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("reflector-label-mean-tot",
                              autoplot(Ler_leaf_rflt.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("reflector-range-num-tot",
                              autoplot(Ler_leaf_rflt.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("reflector-range-wb-tot",
                              autoplot(Ler_leaf_rflt.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("reflector-no-annotations-tot",
                              autoplot(Ler_leaf_rflt.spct, annotations = ""))
  vdiffr::expect_doppelganger("reflector-minus-annotations-tot",
                              autoplot(Ler_leaf_rflt.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("reflector-plus-annotations-tot",
                              autoplot(Ler_leaf_rflt.spct, annotations = c("+", "boundaries")))
})

test_that("reflector_mspct", {
  set_annotations_default()
  two_leaves_Rfr.mspct <- reflector_mspct(list(one = Ler_leaf_rflt.spct,
                                        half = Ler_leaf_rflt.spct / 2))
  vdiffr::expect_doppelganger("reflector-mspct-default",
                              autoplot(two_leaves_Rfr.mspct))
  vdiffr::expect_doppelganger("reflector-mspct-default-range",
                              autoplot(two_leaves_Rfr.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("reflector-mspct-no-annotations",
                              autoplot(two_leaves_Rfr.mspct, annotations = ""))
  vdiffr::expect_doppelganger("reflector-mspct-reserve-space",
                              autoplot(two_leaves_Rfr.mspct, annotations = "reserve.space"))
})

test_that("object_spct", {
  set_annotations_default()
  vdiffr::expect_doppelganger("object-default-tot",
                              autoplot(Ler_leaf.spct))
  vdiffr::expect_doppelganger("object-default-pc-out-tot",
                              autoplot(Ler_leaf.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("object-text-size-tot",
                              autoplot(Ler_leaf.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("object-wb-vis-tot",
                              autoplot(Ler_leaf.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("object-range-num-tot",
                              autoplot(Ler_leaf.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("object-range-wb-tot",
                              autoplot(Ler_leaf.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("object-no-annotations-tot",
                              autoplot(Ler_leaf.spct, annotations = ""))
  vdiffr::expect_doppelganger("object-reserve-space-tot",
                              autoplot(Ler_leaf.spct, annotations = "reserve.space"))
  vdiffr::expect_doppelganger("object-minus-guide-tot",
                              autoplot(Ler_leaf.spct, annotations = c("-", "colour.guide")))
  vdiffr::expect_doppelganger("object-plus-segments-tot",
                              autoplot(Ler_leaf.spct, annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("object-default-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE))
  vdiffr::expect_doppelganger("object-default-stk-ylim",
                              autoplot(Ler_leaf.spct, stacked = FALSE, ylim = c(-0.2, 1.2)))
  vdiffr::expect_doppelganger("object-default-pc-out-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, pc.out = TRUE))
  vdiffr::expect_doppelganger("object-text-size-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, text.size = 3.5))
  vdiffr::expect_doppelganger("object-minus-peaks-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, span = 101, annotations = c("-", "peaks")))
  vdiffr::expect_doppelganger("object-peaks-valleys-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, span = 101, annotations = c("+", "peaks", "valleys")))
  vdiffr::expect_doppelganger("object-plus-valleys-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, span = 101, annotations = c("+", "valleys")))
  vdiffr::expect_doppelganger("object-wb-vis-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("object-range-num-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, range = c(500, 700)))
  vdiffr::expect_doppelganger("object-range-wb-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("object-no-annotations-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, annotations = ""))
  vdiffr::expect_doppelganger("object-minus-labels-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, annotations = c("-", "labels")))
  vdiffr::expect_doppelganger("object-plus-segments-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE, annotations = c("+", "segments")))
})

test_that("object_spct_as_reflector", {
  set_annotations_default()
  vdiffr::expect_doppelganger("object-as-reflector-default-tot",
                              autoplot(Ler_leaf.spct, plot.qty = "reflectance"))
  vdiffr::expect_doppelganger("object-as-reflector-default-pc-out-tot",
                              autoplot(Ler_leaf.spct, pc.out = TRUE, plot.qty = "reflectance"))
  vdiffr::expect_doppelganger("object-as-reflector-text-size-tot",
                              autoplot(Ler_leaf.spct, text.size = 3.5, plot.qty = "reflectance"))
  vdiffr::expect_doppelganger("object-as-reflector-wb-vis-tot",
                              autoplot(Ler_leaf.spct, w.band = VIS_bands(), plot.qty = "reflectance"))
  vdiffr::expect_doppelganger("object-as-reflector-range-num-tot",
                              autoplot(Ler_leaf.spct, range = c(500, 700), plot.qty = "reflectance"))
  vdiffr::expect_doppelganger("object-as-reflector-range-wb-tot",
                              autoplot(Ler_leaf.spct, range = waveband(c(500, 700)), plot.qty = "reflectance"))
  vdiffr::expect_doppelganger("object-as-reflector-no-annotations-tot",
                              autoplot(Ler_leaf.spct, annotations = "", plot.qty = "reflectance"))
  vdiffr::expect_doppelganger("object-as-reflector-reserve-space-tot",
                              autoplot(Ler_leaf.spct, annotations = "reserve.space", plot.qty = "reflectance"))
  vdiffr::expect_doppelganger("object-as-reflector-minus-guide-tot",
                              autoplot(Ler_leaf.spct, annotations = c("-", "colour.guide"), plot.qty = "reflectance"))
  vdiffr::expect_doppelganger("object-as-reflector-plus-segments-tot",
                              autoplot(Ler_leaf.spct, annotations = c("+", "segments"), plot.qty = "reflectance"))
})

test_that("object_spct_as_filter", {
  set_annotations_default()
  vdiffr::expect_doppelganger("object-filter-tfr-default-tot",
                              autoplot(Ler_leaf.spct, plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("object-as-filter-tfr-default-pc-out-tot",
                              autoplot(Ler_leaf.spct, pc.out = TRUE, plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("object-as-filter-tfr-text-size-tot",
                              autoplot(Ler_leaf.spct, text.size = 3.5, plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("object-as-filter-tfr-wb-vis-tot",
                              autoplot(Ler_leaf.spct, w.band = VIS_bands(), plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("object-as-filter-tfr-range-num-tot",
                              autoplot(Ler_leaf.spct, range = c(500, 700), plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("object-as-filter-tfr-range-wb-tot",
                              autoplot(Ler_leaf.spct, range = waveband(c(500, 700)), plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("object-as-filter-tfr-no-annotations-tot",
                              autoplot(Ler_leaf.spct, annotations = "", plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("object-as-filter-tfr-reserve-space-tot",
                              autoplot(Ler_leaf.spct, annotations = "reserve.space", plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("object-as-filter-tfr-minus-guide-tot",
                              autoplot(Ler_leaf.spct, annotations = c("-", "colour.guide"), plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("object-as-filter-tfr-plus-segments-tot",
                              autoplot(Ler_leaf.spct, annotations = c("+", "segments"), plot.qty = "transmittance"))

  vdiffr::expect_doppelganger("object-as-filter-afr-default-tot",
                              autoplot(Ler_leaf.spct, plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("object-as-filter-afr-default-pc-out-tot",
                              autoplot(Ler_leaf.spct, pc.out = TRUE, plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("object-as-filter-afr-text-size-tot",
                              autoplot(Ler_leaf.spct, text.size = 3.5, plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("object-as-filter-afr-wb-vis-tot",
                              autoplot(Ler_leaf.spct, w.band = VIS_bands(), plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("object-as-filter-afr-range-num-tot",
                              autoplot(Ler_leaf.spct, range = c(500, 700), plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("object-as-filter-afr-range-wb-tot",
                              autoplot(Ler_leaf.spct, range = waveband(c(500, 700)), plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("object-as-filter-afr-no-annotations-tot",
                              autoplot(Ler_leaf.spct, annotations = "", plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("object-as-filter-afr-reserve-space-tot",
                              autoplot(Ler_leaf.spct, annotations = "reserve.space", plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("object-as-filter-afr-minus-guide-tot",
                              autoplot(Ler_leaf.spct, annotations = c("-", "colour.guide"), plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("object-as-filter-afr-plus-segments-tot",
                              autoplot(Ler_leaf.spct, annotations = c("+", "segments"), plot.qty = "absorptance"))

  vdiffr::expect_doppelganger("object-as-filter-a-default-tot",
                              autoplot(Ler_leaf.spct, plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("object-as-filter-a-default-pc-out-tot",
                              autoplot(Ler_leaf.spct, pc.out = TRUE, plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("object-as-filter-a-text-size-tot",
                              autoplot(Ler_leaf.spct, text.size = 3.5, plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("object-as-filter-a-wb-vis-tot",
                              autoplot(Ler_leaf.spct, w.band = VIS_bands(), plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("object-as-filter-a-range-num-tot",
                              autoplot(Ler_leaf.spct, range = c(500, 700), plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("object-as-filter-a-range-wb-tot",
                              autoplot(Ler_leaf.spct, range = waveband(c(500, 700)), plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("object-as-filter-a-no-annotations-tot",
                              autoplot(Ler_leaf.spct, annotations = "", plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("object-as-filter-a-reserve-space-tot",
                              autoplot(Ler_leaf.spct, annotations = "reserve.space", plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("object-as-filter-a-minus-guide-tot",
                              autoplot(Ler_leaf.spct, annotations = c("-", "colour.guide"), plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("object-as-filter-a-plus-segments-tot",
                              autoplot(Ler_leaf.spct, annotations = c("+", "segments"), plot.qty = "absorbance"))
})

test_that("response_spct", {
  set_annotations_default()
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

  vdiffr::expect_doppelganger("response-second-q",
                              autoplot(setTimeUnit(ccd.spct, "second", TRUE), norm = NULL, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-hour-q",
                              autoplot(setTimeUnit(ccd.spct, "hour", TRUE), norm = NULL, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-day-q",
                              autoplot(setTimeUnit(ccd.spct, "day", TRUE), norm = NULL, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-exposure-q",
                              autoplot(setTimeUnit(ccd.spct, "exposure", TRUE), norm = NULL, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-duration-q",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(4.5, "minutes"), TRUE), norm = NULL, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-none-q",
                              autoplot(setTimeUnit(ccd.spct, "unknown", TRUE), norm = NULL, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-seconds-q",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "seconds"), TRUE), norm = NULL, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-hours-q",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "hours"), TRUE), norm = NULL, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-days-q",
                              autoplot(setTimeUnit(ccd.spct, lubridate::duration(1, "days"), TRUE), norm = NULL, unit.out = "photon"))

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


test_that("plot_spct", {
  set_annotations_default()
  # skip_on_cran()
  vdiffr::expect_doppelganger("raw-default-autop",
                               plot(white_led.raw_spct))
  vdiffr::expect_doppelganger("cps-default-autop",
                               plot(white_led.cps_spct))
  vdiffr::expect_doppelganger("source-default-autop",
                               plot(white_led.source_spct))
  vdiffr::expect_doppelganger("source-product-BSWF-autop",
                               plot(sun.spct * CIE()))
  vdiffr::expect_doppelganger("filter-default-tot-autop",
                               plot(Ler_leaf_trns.spct))
  vdiffr::expect_doppelganger("reflector-default-tot-autop",
                               plot(Ler_leaf_rflt.spct))
  vdiffr::expect_doppelganger("object-default-tot-autop",
                               plot(Ler_leaf.spct))
  vdiffr::expect_doppelganger("object-default-stk-autop",
                               plot(Ler_leaf.spct, stacked = FALSE))
  vdiffr::expect_doppelganger("response-default-autop",
                               plot(ccd.spct))
})

test_that("plot_mspct", {
  set_annotations_default()
  two_ccds.mspct <- response_mspct(list(one = ccd.spct,
                                        half = ccd.spct / 2))
  vdiffr::expect_doppelganger("response-mspct-default-autop",
                               plot(two_ccds.mspct))
})

test_that("plot_waveband", {
  vdiffr::expect_doppelganger("waveband-default-cie-autop",
                               plot(CIE(), range = c(230, 430)))
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
