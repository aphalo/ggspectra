context("plot")
library(photobiology)
library(photobiologyWavebands)

# We prepare shorter data objects for spectra to improve tests' runtime
sun.spct <- interpolate_spct(sun.spct, length.out = 50)
white_led.raw_spct <- interpolate_spct(white_led.raw_spct, length.out = 50)
white_led.cps_spct <- interpolate_spct(white_led.cps_spct, length.out = 50)
white_led.source_spct <- interpolate_spct(white_led.source_spct, length.out = 50)
Ler_leaf_trns.spct <- interpolate_spct(Ler_leaf_trns.spct, length.out = 50)
Ler_leaf_trns_i.spct <- interpolate_spct(Ler_leaf_trns_i.spct, length.out = 50)
Ler_leaf_rflt.spct <- interpolate_spct(Ler_leaf_rflt.spct, length.out = 50)
Ler_leaf.spct <- interpolate_spct(Ler_leaf.spct, length.out = 50)
ccd.spct <- interpolate_spct(ccd.spct, length.out = 50)

test_that("raw_spct", {
  # skip_on_cran()
  vdiffr::expect_doppelganger("raw-default",
                      plot(white_led.raw_spct))
  vdiffr::expect_doppelganger("raw-range-num",
                              plot(white_led.raw_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("raw-range-wb",
                              plot(white_led.raw_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("raw-no-annotations",
                      plot(white_led.raw_spct, annotations = ""))
  vdiffr::expect_doppelganger("raw-minus-annotations",
                      plot(white_led.raw_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("raw-plus-annotations",
                      plot(white_led.raw_spct, annotations = c("+", "boundaries")))
})

test_that("cps_spct", {
  vdiffr::expect_doppelganger("cps-default",
                              plot(white_led.cps_spct))
  vdiffr::expect_doppelganger("cps-range-num",
                              plot(white_led.cps_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("cps-range-wb",
                              plot(white_led.cps_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("cps-no-annotations",
                              plot(white_led.cps_spct, annotations = ""))
  vdiffr::expect_doppelganger("cps-minus-annotations",
                              plot(white_led.cps_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("cps-plus-annotations",
                              plot(white_led.cps_spct, annotations = c("+", "boundaries")))
})

test_that("source_spct", {
  vdiffr::expect_doppelganger("source-default",
                              plot(white_led.source_spct))
  vdiffr::expect_doppelganger("source-text-size",
                              plot(white_led.source_spct, text.size = 3.5))
  vdiffr::expect_doppelganger("source-span-31",
                              plot(white_led.source_spct, span = 31))
  vdiffr::expect_doppelganger("source-wb-vis",
                              plot(white_led.source_spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("source-label-average",
                              plot(white_led.source_spct, label.qty = "average"))
  vdiffr::expect_doppelganger("source-label-mean",
                              plot(white_led.source_spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("source-label-total",
                              plot(white_led.source_spct, label.qty = "total"))
  vdiffr::expect_doppelganger("source-label-contrib",
                              plot(white_led.source_spct, label.qty = "contribution"))
  vdiffr::expect_doppelganger("source-label-relative",
                              plot(white_led.source_spct, label.qty = "relative"))
  vdiffr::expect_doppelganger("source-range-num",
                              plot(white_led.source_spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("source-range-wb",
                              plot(white_led.source_spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("source-no-annotations",
                              plot(white_led.source_spct, annotations = ""))
  vdiffr::expect_doppelganger("source-minus-annotations",
                              plot(white_led.source_spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("source-plus-annotations",
                              plot(white_led.source_spct, annotations = c("+", "boundaries")))

  vdiffr::expect_doppelganger("source-default-q",
                              plot(white_led.source_spct, unit.out = "photon"))
  vdiffr::expect_doppelganger("source-text-size-q",
                              plot(white_led.source_spct, unit.out = "photon", text.size = 3.5))
  vdiffr::expect_doppelganger("source-span-31-q",
                              plot(white_led.source_spct, unit.out = "photon", span = 31))
  vdiffr::expect_doppelganger("source-wb-vis-q",
                              plot(white_led.source_spct, unit.out = "photon", w.band = VIS_bands()))
  vdiffr::expect_doppelganger("source-label-average-q",
                              plot(white_led.source_spct, unit.out = "photon", label.qty = "average"))
  vdiffr::expect_doppelganger("source-label-mean-q",
                              plot(white_led.source_spct, unit.out = "photon", label.qty = "mean"))
  vdiffr::expect_doppelganger("source-range-num-q",
                              plot(white_led.source_spct, unit.out = "photon", range = c(500, 700)))
  vdiffr::expect_doppelganger("source-range-wb-q",
                              plot(white_led.source_spct, unit.out = "photon", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("source-no-annotations-q",
                              plot(white_led.source_spct, unit.out = "photon", annotations = ""))
  vdiffr::expect_doppelganger("source-minus-summaries-q",
                              plot(white_led.source_spct, unit.out = "photon", annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("source-minus-boxes-q",
                              plot(white_led.source_spct, unit.out = "photon", annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("source-plus-boundaries-q",
                              plot(white_led.source_spct, unit.out = "photon", annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("source-plus-segments-q",
                              plot(white_led.source_spct, unit.out = "photon", annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("source-normalized",
                              plot(normalize(white_led.source_spct)))
  vdiffr::expect_doppelganger("source-normalized-wl",
                              plot(normalize(white_led.source_spct, norm = 550)))
  vdiffr::expect_doppelganger("source-scaled",
                              plot(fscale(white_led.source_spct)))
  vdiffr::expect_doppelganger("source-scaled-total",
                              plot(fscale(white_led.source_spct, f = "total")))
  vdiffr::expect_doppelganger("source-scaled-mean",
                              plot(fscale(white_led.source_spct, f = "mean")))
  vdiffr::expect_doppelganger("source-scaled-100",
                              plot(fscale(white_led.source_spct, target = 100)))
  vdiffr::expect_doppelganger("source-scaled-100-PAR",
                              plot(fscale(white_led.source_spct, target = 100, range = PAR())))
  vdiffr::expect_doppelganger("source-scaled-total-100",
                              plot(fscale(white_led.source_spct, f = "total", target = 100)))
  vdiffr::expect_doppelganger("source-product-wb",
                              plot(sun.spct * Red()))
  vdiffr::expect_doppelganger("source-product-BSWF",
                              plot(sun.spct * CIE()))
  vdiffr::expect_doppelganger("source-product-BSWF-wl",
                              plot(sun.spct * CIE(300)))
})

test_that("source_mspct", {
  two_leds.mspct <- source_mspct(list(one = white_led.source_spct,
                                      half = white_led.source_spct / 2))
  vdiffr::expect_doppelganger("source-mspct-default",
                              plot(two_leds.mspct))
  vdiffr::expect_doppelganger("source-mspct-default-range",
                              plot(two_leds.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("source-mspct-default-e",
                              plot(two_leds.mspct, unit.out = "energy"))
  vdiffr::expect_doppelganger("source-mspct-default-p",
                              plot(two_leds.mspct, unit.out = "photon"))
  vdiffr::expect_doppelganger("source-mspct-no-annotations",
                              plot(two_leds.mspct, annotations = ""))
})

test_that("filter_spct", {
  vdiffr::expect_doppelganger("filter-default-tot",
                              plot(Ler_leaf_trns.spct))
  vdiffr::expect_doppelganger("filter-default-pc-out-tot",
                              plot(Ler_leaf_trns.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size-tot",
                              plot(Ler_leaf_trns.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-tot",
                              plot(Ler_leaf_trns.spct, span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-tot",
                              plot(Ler_leaf_trns.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-tot",
                              plot(Ler_leaf_trns.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-tot",
                              plot(Ler_leaf_trns.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num-tot",
                              plot(Ler_leaf_trns.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb-tot",
                              plot(Ler_leaf_trns.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-tot",
                              plot(Ler_leaf_trns.spct, annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-annotations-tot",
                              plot(Ler_leaf_trns.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-plus-annotations-tot",
                              plot(Ler_leaf_trns.spct, annotations = c("+", "boundaries")))

  vdiffr::expect_doppelganger("filter-default",
                              plot(Ler_leaf_trns_i.spct))
  vdiffr::expect_doppelganger("filter-default-pc-out",
                              plot(Ler_leaf_trns_i.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size",
                              plot(Ler_leaf_trns_i.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31",
                              plot(Ler_leaf_trns_i.spct, span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis",
                              plot(Ler_leaf_trns_i.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average",
                              plot(Ler_leaf_trns_i.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean",
                              plot(Ler_leaf_trns_i.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num",
                              plot(Ler_leaf_trns_i.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb",
                              plot(Ler_leaf_trns_i.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations",
                              plot(Ler_leaf_trns_i.spct, annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-summaries",
                              plot(Ler_leaf_trns_i.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes",
                              plot(Ler_leaf_trns_i.spct, annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries",
                              plot(Ler_leaf_trns_i.spct, annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments",
                              plot(Ler_leaf_trns_i.spct, annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("filter-default-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("filter-default-pc-out-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-summaries-q",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments-apt",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorptance", annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("filter-default-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA)))
  vdiffr::expect_doppelganger("filter-text-size-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-wb-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-summaries-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments-a",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "absorbance", range = c(300, NA), annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("filter-default-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("filter-default-pc-out-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", pc.out = TRUE))
  vdiffr::expect_doppelganger("filter-text-size-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", text.size = 3.5))
  vdiffr::expect_doppelganger("filter-span-31-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", span = 31))
  vdiffr::expect_doppelganger("filter-wb-vis-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", w.band = VIS_bands()))
  vdiffr::expect_doppelganger("filter-label-average-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", label.qty = "average"))
  vdiffr::expect_doppelganger("filter-label-mean-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", label.qty = "mean"))
  vdiffr::expect_doppelganger("filter-range-num-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", range = c(500, 700)))
  vdiffr::expect_doppelganger("filter-range-wb-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("filter-no-annotations-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", annotations = ""))
  vdiffr::expect_doppelganger("filter-minus-summaries-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("filter-minus-boxes-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("filter-plus-boundaries-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("filter-plus-segments-tfr",
                              plot(Ler_leaf_trns_i.spct, plot.qty = "transmittance", annotations = c("+", "segments")))
})

test_that("filter_mspct", {
  two_leaves.mspct <- filter_mspct(list(one = Ler_leaf_trns_i.spct,
                                      half = Ler_leaf_trns_i.spct / 2))
  vdiffr::expect_doppelganger("filter-mspct-default",
                              plot(two_leaves.mspct))
  vdiffr::expect_doppelganger("filter-mspct-default-range",
                              plot(two_leaves.mspct, range = c(500, 700)))
  # use range to avoid Tfr == 0 values.
  vdiffr::expect_doppelganger("filter-mspct-default-A",
                              plot(two_leaves.mspct,
                                   range = c(400,750),
                                   plot.qty = "absorbance"))
  vdiffr::expect_doppelganger("filter-mspct-default-Afr",
                              plot(two_leaves.mspct,
                                   range = c(400,750),
                                   plot.qty = "absorptance"))
  vdiffr::expect_doppelganger("filter-mspct-default-Tfr",
                              plot(two_leaves.mspct, plot.qty = "transmittance"))
  vdiffr::expect_doppelganger("filter-mspct-no-annotations",
                              plot(two_leaves.mspct, annotations = ""))
})

test_that("reflector_spct", {
  vdiffr::expect_doppelganger("reflector-default-tot",
                              plot(Ler_leaf_rflt.spct))
  vdiffr::expect_doppelganger("reflector-default-pc-out-tot",
                              plot(Ler_leaf_rflt.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("reflector-text-size-tot",
                              plot(Ler_leaf_rflt.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("reflector-span-31-tot",
                              plot(Ler_leaf_rflt.spct, span = 31))
  vdiffr::expect_doppelganger("reflector-wb-vis-tot",
                              plot(Ler_leaf_rflt.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("reflector-label-average-tot",
                              plot(Ler_leaf_rflt.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("reflector-label-mean-tot",
                              plot(Ler_leaf_rflt.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("reflector-range-num-tot",
                              plot(Ler_leaf_rflt.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("reflector-range-wb-tot",
                              plot(Ler_leaf_rflt.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("reflector-no-annotations-tot",
                              plot(Ler_leaf_rflt.spct, annotations = ""))
  vdiffr::expect_doppelganger("reflector-minus-annotations-tot",
                              plot(Ler_leaf_rflt.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("reflector-plus-annotations-tot",
                              plot(Ler_leaf_rflt.spct, annotations = c("+", "boundaries")))
})

test_that("reflector_mspct", {
  two_leaves_Rfr.mspct <- reflector_mspct(list(one = Ler_leaf_rflt.spct,
                                        half = Ler_leaf_rflt.spct / 2))
  vdiffr::expect_doppelganger("reflector-mspct-default",
                              plot(two_leaves_Rfr.mspct))
  vdiffr::expect_doppelganger("reflector-mspct-default-range",
                              plot(two_leaves_Rfr.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("reflector-mspct-no-annotations",
                              plot(two_leaves_Rfr.mspct, annotations = ""))
})

test_that("object_spct", {
  vdiffr::expect_doppelganger("object-default-tot",
                              plot(Ler_leaf.spct))
  vdiffr::expect_doppelganger("object-default-pc-out-tot",
                              plot(Ler_leaf.spct, pc.out = TRUE))
  vdiffr::expect_doppelganger("object-text-size-tot",
                              plot(Ler_leaf.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("object-wb-vis-tot",
                              plot(Ler_leaf.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("object-range-num-tot",
                              plot(Ler_leaf.spct, range = c(500, 700)))
  vdiffr::expect_doppelganger("object-range-wb-tot",
                              plot(Ler_leaf.spct, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("object-no-annotations-tot",
                              plot(Ler_leaf.spct, annotations = ""))
  vdiffr::expect_doppelganger("object-minus-guide-tot",
                              plot(Ler_leaf.spct, annotations = c("-", "clour.guide")))
  vdiffr::expect_doppelganger("object-plus-segments-tot",
                              plot(Ler_leaf.spct, annotations = c("+", "segments")))

  vdiffr::expect_doppelganger("object-default-stk",
                              plot(Ler_leaf.spct, stacked = FALSE))
  vdiffr::expect_doppelganger("object-default-pc-out-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, pc.out = TRUE))
  vdiffr::expect_doppelganger("object-text-size-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, text.size = 3.5))
  vdiffr::expect_doppelganger("object-minus-peaks-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, span = 101, annotations = c("-", "peaks")))
  vdiffr::expect_doppelganger("object-peaks-valleys-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, span = 101, annotations = c("+", "peaks", "valleys")))
  vdiffr::expect_doppelganger("object-plus-valleys-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, span = 101, annotations = c("+", "valleys")))
  vdiffr::expect_doppelganger("object-wb-vis-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("object-range-num-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, range = c(500, 700)))
  vdiffr::expect_doppelganger("object-range-wb-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("object-no-annotations-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, annotations = ""))
  vdiffr::expect_doppelganger("object-minus-labels-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, annotations = c("-", "labels")))
  vdiffr::expect_doppelganger("object-plus-segments-stk",
                              plot(Ler_leaf.spct, stacked = FALSE, annotations = c("+", "segments")))
})

test_that("response_spct", {
  vdiffr::expect_doppelganger("response-default",
                              plot(ccd.spct))
  vdiffr::expect_doppelganger("response-text-size",
                              plot(ccd.spct, text.size = 3.5))
  vdiffr::expect_doppelganger("response-span-31",
                              plot(ccd.spct, span = 31))
  vdiffr::expect_doppelganger("response-wb-vis",
                              plot(ccd.spct, w.band = VIS_bands()))
  vdiffr::expect_doppelganger("response-label-average",
                              plot(ccd.spct, label.qty = "average"))
  vdiffr::expect_doppelganger("response-label-mean",
                              plot(ccd.spct, label.qty = "mean"))
  vdiffr::expect_doppelganger("response-label-total",
                              plot(ccd.spct, label.qty = "total"))
  vdiffr::expect_doppelganger("response-label-contrib",
                              plot(ccd.spct, label.qty = "contribution"))
  vdiffr::expect_doppelganger("response-label-relative",
                              plot(ccd.spct, label.qty = "relative"))
  vdiffr::expect_doppelganger("response-range-num",
                              plot(ccd.spct, range = c(300, 700)))
  vdiffr::expect_doppelganger("response-range-wb",
                              plot(ccd.spct, range = waveband(c(300, 700))))
  vdiffr::expect_doppelganger("response-no-annotations",
                              plot(ccd.spct, annotations = ""))
  vdiffr::expect_doppelganger("response-minus-annotations",
                              plot(ccd.spct, annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("response-plus-annotations",
                              plot(ccd.spct, annotations = c("+", "boundaries")))

  vdiffr::expect_doppelganger("response-default-q",
                              plot(ccd.spct, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-text-size-q",
                              plot(ccd.spct, unit.out = "photon", text.size = 3.5))
  vdiffr::expect_doppelganger("response-span-31-q",
                              plot(ccd.spct, unit.out = "photon", span = 31))
  vdiffr::expect_doppelganger("response-wb-vis-q",
                              plot(ccd.spct, unit.out = "photon", w.band = VIS_bands()))
  vdiffr::expect_doppelganger("response-label-average-q",
                              plot(ccd.spct, unit.out = "photon", label.qty = "average"))
  vdiffr::expect_doppelganger("response-label-mean-q",
                              plot(ccd.spct, unit.out = "photon", label.qty = "mean"))
  vdiffr::expect_doppelganger("response-label-total-q",
                              plot(ccd.spct, unit.out = "photon", label.qty = "total"))
  vdiffr::expect_doppelganger("response-label-contrib-q",
                              plot(ccd.spct, unit.out = "photon", label.qty = "contribution"))
  vdiffr::expect_doppelganger("response-label-relative-q",
                              plot(ccd.spct, unit.out = "photon", label.qty = "relative"))
  vdiffr::expect_doppelganger("response-range-num-q",
                              plot(ccd.spct, unit.out = "photon", range = c(500, 700)))
  vdiffr::expect_doppelganger("response-range-wb-q",
                              plot(ccd.spct, unit.out = "photon", range = waveband(c(500, 700))))
  vdiffr::expect_doppelganger("response-no-annotations-q",
                              plot(ccd.spct, unit.out = "photon", annotations = ""))
  vdiffr::expect_doppelganger("response-minus-summaries-q",
                              plot(ccd.spct, unit.out = "photon", annotations = c("-", "summaries")))
  vdiffr::expect_doppelganger("response-minus-boxes-q",
                              plot(ccd.spct, unit.out = "photon", annotations = c("-", "boxes")))
  vdiffr::expect_doppelganger("response-plus-boundaries-q",
                              plot(ccd.spct, unit.out = "photon", annotations = c("+", "boundaries")))
  vdiffr::expect_doppelganger("response-plus-segments-q",
                              plot(ccd.spct, unit.out = "photon", annotations = c("+", "segments")))
})

test_that("response_mspct", {
  two_ccds.mspct <- response_mspct(list(one = ccd.spct,
                                      half = ccd.spct / 2))
  vdiffr::expect_doppelganger("response-mspct-default",
                              plot(two_ccds.mspct))
  vdiffr::expect_doppelganger("response-mspct-default-range",
                              plot(two_ccds.mspct, range = c(500, 700)))
  vdiffr::expect_doppelganger("response-mspct-default-e",
                              plot(two_ccds.mspct, unit.out = "energy"))
  vdiffr::expect_doppelganger("response-mspct-default-p",
                              plot(two_ccds.mspct, unit.out = "photon"))
  vdiffr::expect_doppelganger("response-mspct-no-annotations",
                              plot(two_ccds.mspct, annotations = ""))
})

test_that("waveband", {
  vdiffr::expect_doppelganger("waveband-default-cie",
                              plot(CIE(), range = c(230, 430)))
  vdiffr::expect_doppelganger("waveband-default-red",
                              plot(Red()))
  vdiffr::expect_doppelganger("waveband-text-size",
                              plot(Red(), text.size = 3.5))
  vdiffr::expect_doppelganger("waveband-plus-segments",
                              plot(Red(), annotations = c("+", "segments")))
})

## autoplot tests use same names as equivalent plot tests
## consequently testing that output from both methods is identical

test_that("autoplot_spct", {
  # skip_on_cran()
  vdiffr::expect_doppelganger("raw-default",
                              autoplot(white_led.raw_spct))
  vdiffr::expect_doppelganger("cps-default",
                              autoplot(white_led.cps_spct))
  vdiffr::expect_doppelganger("source-default",
                              autoplot(white_led.source_spct))
  vdiffr::expect_doppelganger("source-product-BSWF",
                              autoplot(sun.spct * CIE()))
  vdiffr::expect_doppelganger("filter-default-tot",
                              autoplot(Ler_leaf_trns.spct))
  vdiffr::expect_doppelganger("reflector-default-tot",
                              autoplot(Ler_leaf_rflt.spct))
  vdiffr::expect_doppelganger("object-default-tot",
                              autoplot(Ler_leaf.spct))
  vdiffr::expect_doppelganger("object-default-stk",
                              autoplot(Ler_leaf.spct, stacked = FALSE))
  vdiffr::expect_doppelganger("response-default",
                              autoplot(ccd.spct))
})

test_that("autoplot_mspct", {
  two_ccds.mspct <- response_mspct(list(one = ccd.spct,
                                        half = ccd.spct / 2))
  vdiffr::expect_doppelganger("response-mspct-default",
                              autoplot(two_ccds.mspct))
})

test_that("autoplot_waveband", {
  vdiffr::expect_doppelganger("waveband-default-cie",
                              autoplot(CIE(), range = c(230, 430)))
})
